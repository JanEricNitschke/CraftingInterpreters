use crate::{chunk::OpCode, value::Value, types::Line,};

use super::Compiler;

impl<'a> Compiler<'a> {
    pub(super) fn emit_byte<T>(&mut self, byte: T, line: Line)
    where
        T: Into<u8>,
    {
        self.current_chunk().write(byte, line)
    }

    pub(super) fn emit_24bit_number(&mut self, number: usize) -> bool {
        let line = self.line();
        self.current_chunk().write_24bit_number(number, line)
    }

    pub(super) fn emit_bytes<T1, T2>(&mut self, byte1: T1, byte2: T2, line: Line)
    where
        T1: Into<u8>,
        T2: Into<u8>,
    {
        self.current_chunk().write(byte1, line);
        self.current_chunk().write(byte2, line);
    }

    pub(super) fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return, self.line());
    }

    pub(super) fn emit_constant<T>(&mut self, value: T)
    where
        T: Into<Value>,
    {
        if !self.chunk.write_constant(value.into(), self.line()) {
            self.error("Too many constants in one chunk.");
        }
    }

}