use std::io;
use std::time::{SystemTime, UNIX_EPOCH};

use hashbrown::HashMap;

use crate::{
    compiler::Compiler,
    heap::{Heap, StringId, ValueId},
    value::Value,
    vm::VM,
};

fn clock_native(heap: &mut Heap, _args: &[&ValueId]) -> Result<ValueId, String> {
    Ok(heap.values.add(Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )))
}

fn sqrt_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    match &heap.values[args[0]] {
        Value::Number(n) => Ok(heap.values.add(n.sqrt().into())),
        x => Err(format!("'sqrt' expected numeric argument, got: {}", *x)),
    }
}

fn input_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    match &heap.values[args[0]] {
        Value::String(prompt) => {
            println!("{}", &heap.strings[prompt].clone());
            let mut choice = String::new();
            match io::stdin().read_line(&mut choice) {
                Ok(_) => {
                    let string = Value::String(heap.strings.add(choice.trim().to_string()));
                    Ok(heap.values.add(string))
                }
                Err(e) => Err(format!("'input' could not read line: {}", e)),
            }
        }
        x => Err(format!("'input' expected string argument, got: {}", x)),
    }
}

fn to_number_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    match &heap.values[args[0]] {
        Value::String(string_id) => {
            let string = &heap.strings[string_id];
            let converted: Result<f64, _> = string.parse();
            match converted {
                Ok(result) => Ok(heap.values.add(Value::Number(result))),
                Err(_) => Err(format!(
                    "'number' could not convert string '{}' to a number.",
                    string
                )),
            }
        }
        value @ Value::Number(_) => Ok(heap.values.add(value.clone())),
        Value::Bool(value) => Ok(heap.values.add(Value::Number(f64::from(*value)))),
        x => Err(format!(
            "'number' expected string, number or bool argument, got: {}",
            x
        )),
    }
}

fn getattr_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    match (&heap.values[args[0]], &heap.values[args[1]]) {
        (Value::Instance(instance), Value::String(string_id)) => {
            let field = &heap.strings[string_id];
            match instance.fields.get(field).cloned() {
                Some(value_id) => Ok(value_id),
                None => Err(format!("Undefined property '{}'.", *field)),
            }
        }
        (instance @ Value::Instance(_), x) => Err(format!(
            "`getattr` can only index with string indexes, got: `{}` (instance: `{}`)",
            x, instance
        )),
        (not_instance, _) => Err(format!(
            "`getattr` only works on instances, got `{}`",
            not_instance
        )),
    }
}

fn setattr_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    if let Value::String(string_id) = &heap.values[args[1]] {
        let field = heap.strings[string_id].clone();
        if let Value::Instance(instance) = &mut heap.values[args[0]] {
            instance.fields.insert(field, *args[2]);
            Ok(heap.builtin_constants().nil)
        } else {
            Err(format!(
                "`setattr` only works on instances, got `{}`",
                heap.values[args[0]]
            ))
        }
    } else {
        Err(format!(
            "`setattr` can only index with string indexes, got: `{}` (instance: `{}`)",
            heap.values[args[1]], heap.values[args[0]]
        ))
    }
}

fn hasattr_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    match (&heap.values[args[0]], &heap.values[args[1]]) {
        (Value::Instance(instance), Value::String(string_id)) => Ok(heap
            .builtin_constants()
            .bool(instance.fields.contains_key(&heap.strings[string_id]))),
        (instance @ Value::Instance(_), x) => Err(format!(
            "`hasattr` can only index with string indexes, got: `{}` (instance: `{}`)",
            x, instance
        )),
        (not_instance, _) => Err(format!(
            "`hasattr` only works on instances, got `{}`",
            not_instance
        )),
    }
}

fn delattr_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    if let Value::String(string_id) = &heap.values[args[1]] {
        let field = heap.strings[string_id].clone();
        if let Value::Instance(instance) = &mut heap.values[args[0]] {
            match instance.fields.remove(&field) {
                Some(_) => Ok(heap.builtin_constants().nil),
                None => Err(format!("Undefined property '{}'.", field)),
            }
        } else {
            Err(format!(
                "`delattr` only works on instances, got `{}`",
                heap.values[args[0]]
            ))
        }
    } else {
        Err(format!(
            "`delattr` can only index with string indexes, got: `{}` (instance: `{}`)",
            heap.values[args[1]], heap.values[args[0]]
        ))
    }
}

pub struct NativeFunctions {
    string_ids: HashMap<String, StringId>,
}

impl NativeFunctions {
    #[must_use]
    pub fn new() -> Self {
        Self {
            string_ids: HashMap::new(),
        }
    }

    pub fn create_names(&mut self, heap: &mut Heap) {
        for name in [
            "clock", "sqrt", "input", "number", "getattr", "setattr", "hasattr", "delattr",
        ] {
            let string_id = heap.strings.add(name.to_string());
            self.string_ids.insert(name.to_string(), string_id);
        }
    }

    pub fn register_names(&mut self, compiler: &mut Compiler) {
        compiler.inject_strings(&self.string_ids);
    }

    pub fn define_functions(&self, vm: &mut VM) {
        vm.define_native(self.string_ids["clock"], 0, clock_native);
        vm.define_native(self.string_ids["sqrt"], 1, sqrt_native);
        vm.define_native(self.string_ids["input"], 1, input_native);
        vm.define_native(self.string_ids["number"], 1, to_number_native);
        vm.define_native(self.string_ids["getattr"], 2, getattr_native);
        vm.define_native(self.string_ids["setattr"], 3, setattr_native);
        vm.define_native(self.string_ids["hasattr"], 2, hasattr_native);
        vm.define_native(self.string_ids["delattr"], 2, delattr_native);
    }
}
