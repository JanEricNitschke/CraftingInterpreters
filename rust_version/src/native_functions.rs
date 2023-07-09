use std::time::{SystemTime, UNIX_EPOCH};
use std::io;

use hashbrown::HashMap;

use crate::{
    arena::{Arena, StringId},
    compiler::Compiler,
    value::Value,
    vm::VM,
};

fn clock_native(_args: &[Value], _arena: &mut Arena) -> Result<Value, String> {
    Ok(Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    ))
}

fn sqrt_native(args: &[Value], _arena: &mut Arena) -> Result<Value, String> {
    match args {
        [Value::Number(n)] => Ok(n.sqrt().into()),
        [x] => Err(format!("'sqrt' expected numeric argument, got: {}", x)),
        _ => unreachable!(),
    }
}

fn input_native(args: &[Value], arena: &mut Arena) -> Result<Value, String> {
    match args {
        [Value::String(prompt)] => {
            println!("{}", (**prompt).clone());
            let mut choice = String::new();
            match             io::stdin().read_line(&mut choice) {
                Ok(_) => {
                    let string_id = arena.add_string(choice.trim().to_string());
                    Ok(Value::String(string_id))
                }
                Err(e) => Err(format!("'input' could not read line: {}", e)),
            }
        }
        [x] => Err(format!("'input' expected string argument, got: {}", x)),
        _ => unreachable!(),
    }
}

fn to_number_native(args: &[Value], _arena: &mut Arena) -> Result<Value, String> {
    match args {
        [Value::String(prompt)] => {
            let prompt =(**prompt).clone();
            let converted: Result<f64, _> = prompt.parse();
            match  converted  {
                Ok(result) => Ok(Value::Number(result)),
                Err(_) => Err(format!("'number' could not convert string '{}' to a number.", prompt)),
            }
        }
        [value @ Value::Number(_)]  => Ok(value.clone()),
        [Value::Bool(value)]  => Ok(Value::Number(f64::from(*value))),
        [x] => Err(format!("'number' expected string, number or bool argument, got: {}", x)),
        _ => unreachable!(),
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

    pub fn create_names(&mut self, arena: &mut Arena) {
        for name in ["clock", "sqrt", "input", "number"] {
            let string_id = arena.add_string(name.to_string());
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
    }
}