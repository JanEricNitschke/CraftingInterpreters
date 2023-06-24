use crate::{chunk::{OpCode, CodeOffset}, value::Value, types::Line,};

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

    pub(super) fn emit_jump(&mut self, instruction: OpCode) -> CodeOffset {
        let line = self.line();
        self.emit_byte(instruction, line);
        let retval = CodeOffset(self.current_chunk().code().len()-1);
        self.emit_byte(0xff,  line);
        self.emit_byte(0xff, line);
        retval
    }

    pub(super) fn patch_jump(&mut self, jump_offset: CodeOffset) {
        let jump_length = self.current_chunk().code().len() - *jump_offset - OpCode::Jump.instruction_len();

        if jump_length > usize::from(u16::MAX) {
            self.error("Too much code to jump over.");
        }

        self.current_chunk().patch(CodeOffset(*jump_offset+1), (jump_length >> 8) as u8);
        self.current_chunk().patch(CodeOffset(*jump_offset+2), jump_length as u8);
    }

    pub(super) fn emit_loop(&mut self, loop_start: CodeOffset) {
        let offset = self.current_chunk().code().len() - *loop_start + OpCode::Loop.instruction_len();
        let line = self.line();

        self.emit_byte(OpCode::Loop, line);
        if offset > usize::from(u16::MAX) {
            self.error("Loop body too large.");
        }

        self.emit_byte((offset >> 8) as u8, line);
        self.emit_byte(offset as u8, line);
    }

}