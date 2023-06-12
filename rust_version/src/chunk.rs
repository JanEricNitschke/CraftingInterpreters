use derivative::Derivative;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use paste::paste;
use shrinkwraprs::Shrinkwrap;

use crate::{value::Value, config};

#[derive(Shrinkwrap, PartialEq, Eq, Clone, Copy, Debug)]
#[shrinkwrap(mutable)]
pub struct Line(pub usize);

#[derive(Shrinkwrap, Clone, Copy)]
#[shrinkwrap(mutable)]
pub struct CodeOffset(pub usize);

#[derive(Shrinkwrap, Clone, Copy)]
pub struct ConstantIndex(pub u8);

impl From<ConstantIndex> for u8 {
    fn from(index: ConstantIndex) -> Self {
        index.0
    }
}

#[derive(Shrinkwrap, Clone, Copy)]
pub struct ConstantLongIndex(pub usize);

impl TryFrom<ConstantLongIndex> for ConstantIndex {
    type Error = <u8 as TryFrom<usize>>::Error;

    fn try_from(value: ConstantLongIndex) -> Result<Self, Self::Error> {
        let short = u8::try_from(value.0)?;
        Ok(ConstantIndex(short))
    }
}

#[derive(IntoPrimitive, TryFromPrimitive, PartialEq, Eq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    Return,
    Constant,
    ConstantLong,
}

#[derive(PartialEq, Derivative, Clone)]
#[derivative(PartialOrd)]
pub struct Chunk {
    name: String,
    code: Vec<u8>,
    #[derivative(PartialOrd = "ignore")]
    lines: Vec<(usize, Line)>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new(name: String) -> Self {
        Chunk {
            name,
            code: Default::default(),
            lines: Default::default(),
            constants: Default::default(),
        }
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn get_constant<T>(&self, index: T) -> &Value
    where
        T: Into<usize>,
    {
        &self.constants[index.into()]
    }

    pub fn get_line(&self, offset: &CodeOffset) -> Line {
        let mut iter = self.lines.iter();
        let (mut consumed, mut line) = iter.next().unwrap();
        while consumed <= *offset.as_ref() {
            let entry = iter.next().unwrap();
            consumed += entry.0;
            line = entry.1;
        }
        line
    }

    pub fn write<T>(&mut self, what: T, line: Line)
    where
        T: Into<u8>,
    {
        self.code.push(what.into());
        match self.lines.last_mut() {
            Some((count, last_line)) if last_line.as_ref() == line.as_ref() => {
                *count += 1;
            }
            _ => self.lines.push((1, line)),
        }
    }

    pub fn add_constant(&mut self, what: Value) -> ConstantLongIndex {
        self.constants.push(what);
        ConstantLongIndex(self.constants.len()-1)
    }

    pub fn write_constant(&mut self, what: Value, line: Line) -> bool {
        let long_index = self.add_constant(what);
        if let Ok(short_index) = u8::try_from(*long_index) {
            self.write(OpCode::Constant, line);
            self.write(short_index, line);
            true
        } else if !config::STD_MODE.load() {
            self.write(OpCode::ConstantLong, line);
            self.write_24bit_number(*long_index, line)
        } else {
            false
        }
    }

    pub fn write_24bit_number(&mut self, what: usize, line: Line) -> bool {
        let (a, b, c, d) = crate::bitwise::get_4_bytes(what);
        if a > 0 {
            return false;
        }
        self.write(b, line);
        self.write(c, line);
        self.write(d, line);
        true
    }
}

impl std::fmt::Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "== {} ==", self.name)?;
        let mut disassembler = InstructionDisassembler::new(self);
        while disassembler.offset.as_ref() < &self.code.len() {
            writeln!(f, "{:?}", disassembler)?;
            *disassembler.offset += disassembler.instruction_len(*disassembler.offset);
        }
        Ok(())
    }
}

// Debug helpers
pub struct InstructionDisassembler<'chunk> {
    chunk: &'chunk Chunk,
    pub offset: CodeOffset,
}

impl<'chunk> InstructionDisassembler<'chunk> {
    #[must_use]
    pub fn new(chunk: &'chunk Chunk) -> Self {
        Self {
            chunk,
            offset: CodeOffset(0),
        }
    }

    fn instruction_len(&self, offset: usize) -> usize {
        let opcode = OpCode::try_from_primitive(self.chunk.code[offset]).unwrap();
        use OpCode::*;
        std::mem::size_of::<OpCode>()
            + match opcode {
                Return => 0,
                Constant => 1,
                ConstantLong => 3,
            }
    }

    fn debug_simple_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        _offset: &CodeOffset,
    ) -> std::fmt::Result {
        write!(f, "{}", name)
    }

    fn debug_constant_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: &CodeOffset,
    ) -> std::fmt::Result {
        let constant_index = self.chunk.code()[offset.as_ref() + 1];
        write!(f, "{:-16} {:>4}", name, constant_index,)?;
        write!(
            f,
            " '{:?}'",
            *self.chunk.get_constant(constant_index)
        )
    }

    fn debug_constant_long_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: &CodeOffset,
    ) -> std::fmt::Result {
        let code = self.chunk.code();
        let constant_index = ConstantLongIndex(
            (usize::from(code[offset.as_ref() + 1]) << 16)
                + (usize::from(code[offset.as_ref() + 2]) << 8)
                + (usize::from(code[offset.as_ref() + 3])),
        );
        write!(
            f,
            "{:-16} {:>4} '{:?}'",
            name,
            *constant_index,
            *self.chunk.get_constant(*constant_index)
        )
    }
}

macro_rules! disassemble {
    (
        $self:ident,
        $f:ident,
        $offset:ident,
        $m:expr,
        $(
            $k:ident(
                $($v:ident),* $(,)?
            )
        ),* $(,)?
    ) => {paste! {
        match $m {
            $($(
                OpCode::$v => $self.[<debug_ $k _opcode>]($f, stringify!([<OP_ $v:snake:upper>]), $offset)
            ),*),*
        }
    }}
}

impl<'chunk> std::fmt::Debug for InstructionDisassembler<'chunk> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let code = self.chunk.code();
        let offset = &self.offset;

        write!(f, "{:04} ", *offset.as_ref())?;
        if *offset.as_ref() > 0
            && self.chunk.get_line(offset) == self.chunk.get_line(&CodeOffset(offset.as_ref() - 1))
        {
            write!(f, "   | ")?;
        } else {
            write!(f, "{:>4} ", *self.chunk.get_line(offset))?;
        }

        let opcode = OpCode::try_from_primitive(code[*offset.as_ref()])
            .unwrap_or_else(|_| panic!("Unknown opcode: {}", code[*offset.as_ref()]));

        disassemble!(self, f, offset, opcode, simple(Return,),constant(Constant),constant_long(ConstantLong))?;
        Ok(())
    }
}
