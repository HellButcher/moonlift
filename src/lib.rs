use std::{convert::Infallible, io};

use ast::Block;
use lexer::Position;

use crate::{
    codegen_state::{BytecodeGenerator, CodeGenerationError, Proto},
    lexer::LexerError,
    parser::{ParseError, ParseVisitorOutput},
    parser_ast::AstVisitor,
};

mod ast;
mod val;
#[macro_use]
pub mod opcode;
mod codegen;
mod codegen_state;
mod vm;
//mod ffi;
//mod ffi_impl;
//pub mod jit;
mod bytecode_ser;
mod lexer;
mod parser;
mod parser_ast;
mod source;

pub mod gc {
    //! Re-export and extend abfall GC types for Moonlift
    pub use abfall::{GcContext as Gc, GcPtr as GcVal, GcRoot, GcCell, Trace, Tracer};
    
    /// Compatibility wrapper for the old Rooted type
    pub type Rooted<'a, T> = T;
    
    /// Compatibility wrapper for GcRef
    pub type GcRef<'a, T> = GcRoot<T>;
}
pub use val::Value;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum Error<IoError = Infallible, VisitorError = Infallible> {
    #[error(transparent)]
    LexerError(#[from] LexerError<IoError>),

    #[error(transparent)]
    ParseError(#[from] ParseError),

    #[error(transparent)]
    CodegenError(VisitorError),

    #[error(transparent)]
    ModuleError(#[from] ModuleError),
}

#[derive(thiserror::Error, Debug, PartialEq)]
#[error("Parse error at {position}: {error}")]
pub struct ErrorWithPosition<IoError = Infallible, VisitorError = Infallible> {
    pub error: Error<IoError, VisitorError>,
    pub position: Position,
}

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ModuleError {}

#[derive(Debug, PartialEq)]
pub struct Ast {
    block: Block,
}

impl Ast {
    pub fn from_bytes(bytes: impl AsRef<[u8]>) -> Result<Self, ErrorWithPosition> {
        let block = AstVisitor::new().parse_bytes(bytes)?;
        Ok(Self::from_block(block))
    }
    pub fn read(read: impl io::Read) -> Result<Self, ErrorWithPosition<io::Error>> {
        let block = AstVisitor::new().parse_read(read)?;
        Ok(Self::from_block(block))
    }
    fn from_block(block: Block) -> Self {
        Self { block }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Bytecode {
    root: Proto,
}

impl Bytecode {
    pub fn parse_bytes(
        bytes: impl AsRef<[u8]>,
    ) -> Result<Self, ErrorWithPosition<Infallible, CodeGenerationError>> {
        let proto = BytecodeGenerator::new().parse_bytes_with_debug(bytes)?;
        Ok(Self::from_proto(proto))
    }

    pub fn parse(
        read: impl io::Read,
    ) -> Result<Self, ErrorWithPosition<io::Error, CodeGenerationError>> {
        let proto = BytecodeGenerator::new().parse_read_with_debug(read)?;
        Ok(Self::from_proto(proto))
    }

    fn from_proto(root: Proto) -> Self {
        Self { root }
    }
}
