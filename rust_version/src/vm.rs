use std::collections::VecDeque;
use std::pin::Pin;

use hashbrown::HashMap;

use crate::arena::ValueId;
use crate::chunk::InstructionDisassembler;
use crate::native_functions::NativeFunctions;
use crate::value::{Closure, Upvalue};
use crate::{
    arena::{Arena, StringId},
    chunk::{CodeOffset, OpCode},
    compiler::Compiler,
    config,
    scanner::Scanner,
    value::{NativeFunction, NativeFunctionImpl, Value},
};

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

macro_rules! runtime_error {
    ($self:ident, $($arg:expr),* $(,)?) => {
        eprintln!($($arg),*);
        for frame in $self.frames.iter().rev() {
            let line = frame.closure().function.chunk.get_line(&CodeOffset(frame.ip - 1));
            eprintln!("[line {}] in {}", *line, *frame.closure().function.name);
        }
    };
}

macro_rules! binary_op {
    ($self:ident, $op:tt) => {
        if !$self.binary_op(|a, b| a $op b) {
            return InterpretResult::RuntimeError;
        }
    }
}

type BinaryOp<T> = fn(f64, f64) -> T;

struct Global {
    value: ValueId,
    mutable: bool,
}

pub struct CallFrame {
    closure: ValueId,
    ip: usize,
    stack_base: usize,
}

impl CallFrame {
    pub fn closure(&self) -> &Closure {
        (*self.closure).as_closure()
    }
}

struct BuiltinConstants {
    pub nil: ValueId,
    pub true_: ValueId,
    pub false_: ValueId,
}

impl BuiltinConstants {
    #[must_use]
    pub fn new(arena: &mut Arena) -> Self {
        Self {
            nil: arena.add_value(Value::Nil),
            true_: arena.add_value(Value::Bool(true)),
            false_: arena.add_value(Value::Bool(false)),
        }
    }

    pub fn bool(&self, val: bool) -> ValueId {
        if val {
            self.true_
        } else {
            self.false_
        }
    }
}

pub struct VM {
    arena: Pin<Box<Arena>>,
    builtin_constants: BuiltinConstants,
    frames: Vec<CallFrame>,
    stack: Vec<ValueId>,
    globals: HashMap<StringId, Global>,
    open_upvalues: VecDeque<ValueId>,
}

impl VM {
    #[must_use]
    pub fn new() -> Self {
        let mut arena = Pin::new(Box::new(Arena::new()));
        Self {
            builtin_constants: BuiltinConstants::new(&mut arena),
            arena,
            frames: Vec::with_capacity(crate::config::FRAMES_MAX),
            stack: Vec::with_capacity(crate::config::STACK_MAX),
            globals: HashMap::new(),
            open_upvalues: VecDeque::new(),
        }
    }

    pub fn interpret(&mut self, source: &[u8]) -> InterpretResult {
        let scanner = Scanner::new(source);
        let mut native_functions = NativeFunctions::new();
        native_functions.create_names(&mut self.arena);
        let mut compiler = Compiler::new(scanner, &mut self.arena);
        native_functions.register_names(&mut compiler);

        let result = if let Some(function) = compiler.compile() {
            native_functions.define_functions(self);

            let function_id = self.arena.add_function(function);
            let closure = Value::closure(function_id);
            let value_id = self.arena.add_value(closure);
            self.stack_push(value_id);
            self.execute_call(value_id, 0);
            self.run()
        } else {
            InterpretResult::CompileError
        };

        if result == InterpretResult::Ok {
            assert_eq!(self.stack.len(), 0);
        }
        result
    }

    fn run(&mut self) -> InterpretResult {
        let trace_execution = config::TRACE_EXECUTION.load();
        let stress_gc = config::STRESS_GC.load();
        loop {
            if trace_execution {
                let function = &self.frame().closure().function;
                let mut disassembler = InstructionDisassembler::new(&function.chunk);
                println!("{}", self.frame().ip);
                *disassembler.offset = self.frame().ip;
                println!(
                    "          [{}]",
                    self.stack
                        .iter()
                        .map(|v| format!("{}", **v))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                print!("{:?}", disassembler);
            }
            self.collect_garbage(stress_gc);
            match OpCode::try_from(self.read_byte("instruction"))
                .expect("Internal error: unrecognized opcode")
            {
                OpCode::Print => {
                    println!(
                        "{}",
                        *self.stack.pop().expect("Stack underflow in OP_PRINT.")
                    );
                }
                OpCode::Pop => {
                    self.stack.pop().expect("Stack underflow in OP_POP.");
                }
                OpCode::Dup => {
                    self.stack_push_value(
                        (**self.stack.last().expect("stack underflow in OP_DUP")).clone(),
                    );
                }
                op @ (OpCode::GetLocal | OpCode::GetLocalLong) => self.get_local(op),
                op @ (OpCode::SetLocal | OpCode::SetLocalLong) => self.set_local(op),
                op @ (OpCode::GetGlobal | OpCode::GetGlobalLong) => {
                    if let Some(value) = self.get_global(op) {
                        return value;
                    }
                }
                op @ (OpCode::SetGlobal | OpCode::SetGlobalLong) => {
                    if let Some(value) = self.set_global(op) {
                        return value;
                    }
                }
                op @ (OpCode::DefineGlobal
                | OpCode::DefineGlobalLong
                | OpCode::DefineGlobalConst
                | OpCode::DefineGlobalConstLong) => self.define_global(op),
                OpCode::JumpIfFalse => self.jump_if_false(),
                OpCode::Call => {
                    if let Some(value) = self.call() {
                        return value;
                    }
                }
                OpCode::Return => {
                    if let Some(value) = self.return_() {
                        return value;
                    }
                }
                OpCode::Constant => {
                    let value = *self.read_constant(false);
                    self.stack_push(value)
                }
                OpCode::ConstantLong => {
                    let value = *self.read_constant(true);
                    self.stack_push(value)
                }
                OpCode::Negate => {
                    if let Some(value) = self.negate() {
                        return value;
                    }
                }
                OpCode::Not => self.not_(),
                OpCode::Nil => self.stack_push(self.builtin_constants.nil),
                OpCode::True => self.stack_push(self.builtin_constants.true_),
                OpCode::False => self.stack_push(self.builtin_constants.false_),
                OpCode::Equal => self.equal(),
                OpCode::Add => {
                    if let Some(value) = self.add() {
                        return value;
                    }
                }
                OpCode::Subtract => binary_op!(self, -),
                OpCode::Multiply => binary_op!(self, *),
                OpCode::Divide => binary_op!(self, /),
                OpCode::Greater => binary_op!(self, >),
                OpCode::Less => binary_op!(self, <),
                OpCode::Jump => {
                    let offset =
                        self.read_16bit_number("Internal error: missing operand for OP_JUMP");
                    self.frame_mut().ip += offset;
                }
                OpCode::Loop => {
                    let offset =
                        self.read_16bit_number("Internal error: missing operand for OP_LOOP");
                    self.frame_mut().ip -= offset;
                }
                OpCode::Closure => {
                    let value = *self.read_constant(false);
                    let function = value.as_function();
                    let mut closure = Closure::new(*function);

                    for _ in 0..usize::from(closure.upvalue_count) {
                        let is_local = self.read_byte("Missing 'is_local' operand for OP_CLOSURE");
                        debug_assert!(
                            is_local == 0 || is_local == 1,
                            "'is_local` must be 0 or 1, got {}",
                            is_local
                        );
                        let is_local = is_local == 1;

                        let index =
                            usize::from(self.read_byte("Missing 'index' operand for OP_CLOSURE"));
                        if is_local {
                            closure.upvalues.push(self.capture_upvalue(index));
                        } else {
                            closure
                                .upvalues
                                .push((*self.frame().closure).as_closure().upvalues[index]);
                        }
                    }
                    let closure_id = self.arena.add_value(Value::from(closure));
                    self.stack_push(closure_id);
                }
                OpCode::GetUpvalue => {
                    let upvalue_index =
                        usize::from(self.read_byte("Missing argument for OP_GET_UPVALUE"));
                    let upvalue_location = self.frame().closure.as_closure().upvalues
                        [upvalue_index]
                        .upvalue_location();
                    match *upvalue_location {
                        Upvalue::Open(absolute_local_index) => {
                            self.stack_push(self.stack[absolute_local_index]);
                        }
                        Upvalue::Closed(value_id) => self.stack_push(value_id),
                    }
                }
                OpCode::SetUpvalue => {
                    let upvalue_index =
                        usize::from(self.read_byte("Missing argument for OP_SET_UPVALUE"));
                    let upvalue_location = self.frame().closure.as_closure().upvalues
                        [upvalue_index]
                        .upvalue_location()
                        // TODO get rid of this `.clone()`
                        .clone();
                    let new_value = self
                        .stack
                        .last()
                        .map(|x| (**x).clone())
                        .expect("Stack underflow in OP_SET_UPVALUE");
                    match upvalue_location {
                        Upvalue::Open(absolute_local_index) => {
                            *self.stack[absolute_local_index] = new_value;
                        }
                        Upvalue::Closed(mut value_id) => {
                            *value_id = new_value;
                        }
                    }
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalue(self.stack.len()-1);
                    self.stack.pop();
                }
                #[allow(unreachable_patterns)]
                _ => {}
            };
        }
    }

    fn binary_op<T: Into<Value>>(&mut self, op: BinaryOp<T>) -> bool {
        let slice_start = self.stack.len() - 2;

        let ok = match &mut self.stack[slice_start..] {
            [left, right] => {
                if let (Value::Number(a), Value::Number(b)) = (&**left, &**right) {
                    let value = op(*a, *b).into();
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(value);
                    true
                } else {
                    false
                }
            }
            _ => false,
        };

        if !ok {
            runtime_error!(self, "Operands must be numbers.");
        }
        ok
    }

    fn get_local(&mut self, op: OpCode) {
        let slot = if op == OpCode::GetLocalLong {
            self.read_24bit_number("Internal error: missing operand for OP_GET_LOCAL_LONG")
        } else {
            usize::from(self.read_byte("Internal error: missing operand for OP_GET_LOCAL"))
        };
        self.stack_push(*self.stack_get(slot));
    }

    fn set_local(&mut self, op: OpCode) {
        let slot = if op == OpCode::GetLocalLong {
            self.read_24bit_number("Internal error: missing operand for OP_SET_LOCAL_LONG")
        } else {
            usize::from(self.read_byte("Internal error: missing operand for OP_SET_LOCAL"))
        };
        *self.stack_get_mut(slot) = *self.stack.last().expect("stack underflow in OP_SET_LOCAL")
    }

    fn get_global(&mut self, op: OpCode) -> Option<InterpretResult> {
        let constant_index = self.read_constant_index(op == OpCode::GetGlobalLong);
        match &**self.read_constant_value(constant_index) {
            Value::String(name) => match self.globals.get(name) {
                Some(global) => self.stack_push(global.value),
                None => {
                    runtime_error!(self, "Undefined variable '{}'.", **name);
                    return Some(InterpretResult::RuntimeError);
                }
            },
            x => panic!("Internal error: non-string operand to {:?}: {:?}", op, x),
        }
        None
    }

    fn set_global(&mut self, op: OpCode) -> Option<InterpretResult> {
        let constant_index = self.read_constant_index(op == OpCode::SetGlobalLong);

        let name = match &**self.read_constant_value(constant_index) {
            Value::String(name) => *name,
            x => panic!(
                "Internal error: non-string operand to OP_SET_GLOBAL: {:?}",
                x
            ),
        };

        if let Some(global) = self.globals.get_mut(&name) {
            if !global.mutable {
                runtime_error!(self, "Reassignment to global 'const'.");
                return Some(InterpretResult::RuntimeError);
            }
            global.value = *self
                .stack
                .last()
                .unwrap_or_else(|| panic!("stack underflow in {:?}", op));
        } else {
            runtime_error!(self, "Undefined variable '{}'.", *name);
            return Some(InterpretResult::RuntimeError);
        }

        None
    }

    fn define_global(&mut self, op: OpCode) {
        match &**self.read_constant(op == OpCode::DefineGlobalLong) {
            Value::String(name) => {
                let name = *name;
                self.globals.insert(
                    name,
                    Global {
                        value: *self
                            .stack
                            .last()
                            .unwrap_or_else(|| panic!("stack underflow in {:?}", op)),
                        mutable: op != OpCode::DefineGlobalConst
                            && op != OpCode::DefineGlobalConstLong,
                    },
                );
                self.stack.pop();
            }
            x => panic!("Internal error: non-string operand to {:?}: {:?}", op, x),
        }
    }

    fn jump_if_false(&mut self) {
        let offset = self.read_16bit_number("Internal error: missing operand for OP_JUMP_IF_FALSE");
        if self
            .stack
            .last()
            .expect("stack underflow in OP_JUMP_IF_FALSE")
            .is_falsey()
        {
            self.frame_mut().ip += offset;
        }
    }

    fn call(&mut self) -> Option<InterpretResult> {
        let arg_count = self.read_byte("Internal error: missing operand for OP_CALL.");
        let callee = self.stack[self.stack.len() - 1 - usize::from(arg_count)];
        if !self.call_value(callee, arg_count) {
            return Some(InterpretResult::RuntimeError);
        }
        None
    }

    fn return_(&mut self) -> Option<InterpretResult> {
        let result = self.stack.pop();
        let frame = self
            .frames
            .pop()
            .expect("Call stack underflow in OP_RETURN");

        if self.frames.is_empty() {
            self.stack.pop();
            return Some(InterpretResult::Ok);
        }

        self.close_upvalue(frame.stack_base);
        self.stack.truncate(frame.stack_base);
        self.stack_push(result.expect("Stack underflow in OP_RETURN"));
        None
    }

    fn negate(&mut self) -> Option<InterpretResult> {
        let value = &mut **self
            .stack
            .last_mut()
            .expect("Stack underflow in OP_NEGATE.");
        match value {
            Value::Number(n) => *n = -*n,
            _ => {
                runtime_error!(self, "Operand must be a number.");
                return Some(InterpretResult::RuntimeError);
            }
        }
        None
    }

    fn not_(&mut self) {
        let value = self
            .stack
            .pop()
            .expect("Stack underflow in OP_NOT.")
            .is_falsey();
        self.stack_push(self.builtin_constants.bool(value))
    }

    fn equal(&mut self) {
        let value = *self
            .stack
            .pop()
            .expect("Stack underflow in OP_EQUAL (first).")
            == *self
                .stack
                .pop()
                .expect("Stack underflow in OP_EQUAL (second).");
        self.stack_push(self.builtin_constants.bool(value));
    }

    fn add(&mut self) -> Option<InterpretResult> {
        let slice_start = self.stack.len() - 2;
        let ok = match &mut self.stack[slice_start..] {
            [left, right] => match (&mut **left, &**right) {
                (Value::Number(a), Value::Number(b)) => {
                    let value = (*a + *b).into();
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(value);
                    true
                }
                (Value::String(a), Value::String(b)) => {
                    // This could be optimized by allowing mutations via the arena
                    let new_string_id = self.arena.add_string(format!("{}{}", **a, **b));
                    self.stack.pop();
                    self.stack.pop();
                    self.stack_push_value(new_string_id.into());
                    true
                }
                _ => false,
            },
            _ => false,
        };

        if !ok {
            runtime_error!(
                self,
                "Operands must be two numbers or two strings. Got: [{}]",
                self.stack[slice_start..]
                    .iter()
                    .map(|v| format!("{}", **v))
                    .collect::<Vec<_>>()
                    .join(", ")
            );

            return Some(InterpretResult::RuntimeError);
        }
        None
    }
    fn read_byte(&mut self, msg: &str) -> u8 {
        let frame = self.frame_mut();
        frame.ip += 1;
        let index = frame.ip - 1;
        *frame.closure().function.chunk.code().get(index).expect(msg)
    }

    fn read_24bit_number(&mut self, msg: &str) -> usize {
        (usize::from(self.read_byte(msg)) << 16)
            + (usize::from(self.read_byte(msg)) << 8)
            + (usize::from(self.read_byte(msg)))
    }

    fn read_16bit_number(&mut self, msg: &str) -> usize {
        (usize::from(self.read_byte(msg)) << 8) + (usize::from(self.read_byte(msg)))
    }

    fn read_constant_index(&mut self, long: bool) -> usize {
        if long {
            self.read_24bit_number("read_constant/long")
        } else {
            usize::from(self.read_byte("read_constant"))
        }
    }

    fn read_constant_value(&self, index: usize) -> &ValueId {
        self.frame().closure().function.chunk.get_constant(index)
    }

    fn read_constant(&mut self, long: bool) -> &ValueId {
        let index = self.read_constant_index(long);
        self.read_constant_value(index)
    }

    #[inline]
    fn stack_push(&mut self, value_id: ValueId) {
        self.stack.push(value_id);
        // This check has a pretty big performance overhead; disabled for now
        // TODO find a better way: keep the check and minimize overhead
        /*
        if self.stack.len() > STACK_MAX {
            runtime_error!(self, "Stack overflow");
        }
        */
    }

    #[inline]
    fn stack_push_value(&mut self, value: Value) {
        let value_id = self.arena.add_value(value);
        self.stack.push(value_id);
    }

    fn stack_get(&self, slot: usize) -> &ValueId {
        &self.stack[self.stack_base() + slot]
    }

    fn stack_get_mut(&mut self, slot: usize) -> &mut ValueId {
        let offset = self.stack_base();
        &mut self.stack[offset + slot]
    }

    fn frame(&self) -> &CallFrame {
        self.frames
            .last()
            .expect("Out of execute_call frames, somehow?")
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        let i = self.frames.len() - 1;
        &mut self.frames[i]
    }

    fn stack_base(&self) -> usize {
        self.frame().stack_base
    }

    fn call_value(&mut self, callee: ValueId, arg_count: u8) -> bool {
        match &*callee {
            Value::Closure(_) => self.execute_call(callee, arg_count),
            Value::NativeFunction(f) => self.execute_native_call(f, arg_count),
            _ => {
                runtime_error!(self, "Can only call functions and classes.");
                false
            }
        }
    }

    fn execute_native_call(&mut self, f: &NativeFunction, arg_count: u8) -> bool {
        let arity = f.arity;
        if arg_count != arity {
            runtime_error!(
                self,
                "Native function '{}' expected {} argument{}, got {}.",
                f.name,
                arity,
                {
                    if arity != 1 {
                        "s"
                    } else {
                        ""
                    }
                },
                arg_count
            );
            return false;
        }
        let fun = f.fun;
        let start_index = self.stack.len() - usize::from(arg_count);
        let args = self.stack[start_index..]
            .iter()
            .map(|v| (**v).clone())
            .collect::<Vec<_>>();
        match fun(&args, &mut self.arena) {
            Ok(value) => {
                self.stack
                    .truncate(self.stack.len() - usize::from(arg_count) - 1);
                self.stack_push_value(value);
                true
            }
            Err(e) => {
                runtime_error!(self, "{}", e);
                false
            }
        }
    }

    fn capture_upvalue(&mut self, local: usize) -> ValueId {
        let local = self.frame().stack_base + local;
        let mut upvalue_index = 0;
        let mut upvalue = None;

        for (i, this) in self.open_upvalues.iter().enumerate() {
            if this.upvalue_location().as_open() <= local {
                break;
            }
            upvalue = Some(this);
            upvalue_index = i;
        }

        if let Some(upvalue) = upvalue {
            if upvalue.upvalue_location().as_open() == local {
                return *upvalue;
            }
        }

        let upvalue = Value::Upvalue(Upvalue::Open(local));
        let upvalue_id = self.arena.add_value(upvalue);
        self.open_upvalues.insert(upvalue_index, upvalue_id);

        upvalue_id
    }

    fn close_upvalue(&mut self, last: usize) {
        while self
            .open_upvalues
            .get(0)
            .map(|v| v.upvalue_location().as_open() >= last)
            .unwrap_or(false)
        {
            let mut upvalue = self.open_upvalues.pop_front().unwrap();
            debug_assert!(matches!(*upvalue, Value::Upvalue(_)));

            let pointed_value = self.stack[upvalue.upvalue_location().as_open()];
            *upvalue.upvalue_location_mut() = Upvalue::Closed(pointed_value);
        }
    }

    fn execute_call(&mut self, closure: ValueId, arg_count: u8) -> bool {
        let arity = closure.as_closure().function.arity;
        let arg_count = usize::from(arg_count);
        if arg_count != arity {
            runtime_error!(
                self,
                "Expected {} argument{} but got {}.",
                arity,
                {
                    if arity != 1 {
                        "s"
                    } else {
                        ""
                    }
                },
                arg_count
            );
            return false;
        }

        if self.frames.len() == crate::config::FRAMES_MAX {
            runtime_error!(self, "Stack overflow.");
            return false;
        }

        debug_assert!(
            matches!(*closure, Value::Closure(_)),
            "`execute_call` must be called with a `Closure`, got: {}",
            *closure
        );

        self.frames.push(CallFrame {
            closure,
            ip: 0,
            stack_base: self.stack.len() - arg_count - 1,
        });
        true
    }

    pub fn define_native(&mut self, name: StringId, arity: u8, fun: NativeFunctionImpl) {
        let value = Value::NativeFunction(NativeFunction {
            name: name.to_string(),
            arity,
            fun,
        });
        let value_id = self.arena.add_value(value);
        self.globals.insert(
            name,
            Global {
                value: value_id,
                mutable: false,
            },
        );
    }

    fn collect_garbage(&mut self, stress_gc: bool) {
        if !stress_gc { //  && !self.arena.needs_gc()
            return;
        }

        self.arena.gc_start();

        // Mark roots
        for value in &self.stack {
            self.arena.mark_value(value);
        }
        for value in self.globals.values() {
            self.arena.mark_value(&value.value);
        }
        for frame in &self.frames {
            self.arena.mark_function(&frame.closure().function);
        }
        for upvalue in &self.open_upvalues {
            self.arena.mark_value(upvalue);
        }
        self.arena.mark_value(&self.builtin_constants.nil);
        self.arena.mark_value(&self.builtin_constants.false_);
        self.arena.mark_value(&self.builtin_constants.true_);

        // Trace references
        self.arena.trace();

        // Remove references to unmarked strings in `self.globals`
        let globals_to_remove = self
            .globals
            .keys()
            .filter(|string_id| !string_id.marked())
            .cloned()
            .collect::<Vec<_>>();
        for id in globals_to_remove {
            self.globals.remove(&id);
        }

        // Finally, sweep
        self.arena.sweep();
    }
}
