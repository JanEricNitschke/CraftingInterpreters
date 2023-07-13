use rand::Rng;
use std::io;
use std::time::{SystemTime, UNIX_EPOCH};

use rustc_hash::FxHashMap as HashMap;

use crate::{
    compiler::Compiler,
    heap::{Heap, StringId, ValueId},
    value::{Number, Value},
    vm::VM,
};

fn clock_native(heap: &mut Heap, _args: &[&ValueId]) -> Result<ValueId, String> {
    Ok(heap.add_value(Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64()
            .into(),
    )))
}

fn sqrt_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    match &heap.values[args[0]] {
        Value::Number(Number::Float(n)) => Ok(heap.add_value(n.sqrt().into())),
        Value::Number(Number::Integer(n)) => Ok(heap.add_value((*n as f64).sqrt().into())),
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
                    let string = Value::String(heap.add_string(choice.trim().to_string()));
                    Ok(heap.add_value(string))
                }
                Err(e) => Err(format!("'input' could not read line: {}", e)),
            }
        }
        x => Err(format!("'input' expected string argument, got: {}", x)),
    }
}

fn to_float_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    match &heap.values[args[0]] {
        Value::String(string_id) => {
            let string = &heap.strings[string_id];
            let converted: Result<f64, _> = string.parse();
            match converted {
                Ok(result) => Ok(heap.add_value(Value::Number(result.into()))),
                Err(_) => Err(format!(
                    "'float' could not convert string '{}' to a float.",
                    string
                )),
            }
        }
        Value::Number(n) => Ok(heap.add_value(Value::Number(f64::from(*n).into()))),
        Value::Bool(value) => Ok(heap.add_value(Value::Number(f64::from(*value).into()))),
        x => Err(format!(
            "'float' expected string, number or bool argument, got: {}",
            x
        )),
    }
}

fn to_int_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    match &heap.values[args[0]] {
        Value::String(string_id) => {
            let string = &heap.strings[string_id];
            let converted: Result<i64, _> = string.parse();
            match converted {
                Ok(result) => Ok(heap.add_value(Value::Number(result.into()))),
                Err(_) => Err(format!(
                    "'int' could not convert string '{}' to an integer.",
                    string
                )),
            }
        }
        Value::Number(n) => Ok(heap.add_value(Value::Number(i64::from(*n).into()))),
        Value::Bool(value) => Ok(heap.add_value(Value::Number(i64::from(*value).into()))),
        x => Err(format!(
            "'int' expected string, number or bool argument, got: {}",
            x
        )),
    }
}

fn to_string_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    let value = &heap.values[args[0]];
    let string = Value::String(heap.add_string(value.to_string()));
    Ok(heap.add_value(string))
}

fn type_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    let string = match &heap.values[args[0]] {
        Value::Bool(_) => Value::String(heap.add_string("<type bool>".to_string())),
        Value::BoundMethod(_) => Value::String(heap.add_string("<type bound method>".to_string())),
        Value::Class(_) => Value::String(heap.add_string("<type class>".to_string())),
        Value::Closure(_) => Value::String(heap.add_string("<type closure>".to_string())),
        Value::Function(_) => Value::String(heap.add_string("<type function>".to_string())),
        Value::Instance(instance) => Value::String(
            heap.add_string("<type ".to_string() + instance.class.as_class().name.as_str() + ">"),
        ),
        Value::NativeFunction(_) => {
            Value::String(heap.add_string("<type native function>".to_string()))
        }
        Value::Nil => Value::String(heap.add_string("<type nil>".to_string())),
        Value::Number(n) => match n {
            Number::Float(_) => Value::String(heap.add_string("<type float>".to_string())),
            Number::Integer(_) => Value::String(heap.add_string("<type int>".to_string())),
        },
        Value::String(_) => Value::String(heap.add_string("<type string>".to_string())),
        Value::Upvalue(_) => Value::String(heap.add_string("<type upvalue>".to_string())),
        Value::List(_) => Value::String(heap.add_string("<type list>".to_string())),
    };
    Ok(heap.add_value(string))
}

fn print_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    let end = if args.len() == 2 {
        match &heap.values[args[1]] {
            Value::String(string_id) => heap.strings[string_id].clone(),
            x => {
                return Err(format!(
                    "Option second argument to 'print' has to be a string, got: {}",
                    x
                ))
            }
        }
    } else {
        "\n".to_string()
    };
    let value = &heap.values[args[0]];
    print!("{}{}", value, end);
    Ok(heap.builtin_constants().nil)
}

fn rng_native(heap: &mut Heap, args: &[&ValueId]) -> Result<ValueId, String> {
    match (&heap.values[args[0]], &heap.values[args[1]]) {
        (Value::Number(Number::Integer(min)), Value::Number(Number::Integer(max))) => Ok(heap
            .add_value(Value::Number(
                rand::thread_rng().gen_range(*min..=*max).into(),
            ))),
        (other_1, other_2) => Err(format!(
            "'rng' expected two integers as arguments, got: `{}` and `{}` instead.",
            other_1, other_2
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
            string_ids: HashMap::default(),
        }
    }

    pub fn create_names(&mut self, heap: &mut Heap) {
        for name in [
            "clock", "sqrt", "input", "float", "int", "str", "type", "getattr", "setattr",
            "hasattr", "delattr", "rng", "print",
        ] {
            let string_id = heap.add_string(name.to_string());
            self.string_ids.insert(name.to_string(), string_id);
        }
    }

    pub fn register_names(&mut self, compiler: &mut Compiler) {
        compiler.inject_strings(&self.string_ids);
    }

    pub fn define_functions(&self, vm: &mut VM) {
        vm.define_native(self.string_ids["clock"], &[0], clock_native);
        vm.define_native(self.string_ids["sqrt"], &[1], sqrt_native);
        vm.define_native(self.string_ids["input"], &[1], input_native);
        vm.define_native(self.string_ids["float"], &[1], to_float_native);
        vm.define_native(self.string_ids["int"], &[1], to_int_native);
        vm.define_native(self.string_ids["str"], &[1], to_string_native);
        vm.define_native(self.string_ids["type"], &[1], type_native);
        vm.define_native(self.string_ids["print"], &[1, 2], print_native);
        vm.define_native(self.string_ids["getattr"], &[2], getattr_native);
        vm.define_native(self.string_ids["setattr"], &[3], setattr_native);
        vm.define_native(self.string_ids["hasattr"], &[2], hasattr_native);
        vm.define_native(self.string_ids["delattr"], &[2], delattr_native);
        vm.define_native(self.string_ids["rng"], &[2], rng_native);
    }
}
