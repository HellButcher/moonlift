use std::{convert::Infallible, io};

use ast::Block;
use lexer::Position;

use crate::{codegen_state::{BytecodeGenerator, CodeGenerationError, Proto}, lexer::LexerError, parser::{ParseError, ParseVisitorOutput}, parser_ast::AstVisitor};

mod ast;
mod val;
#[macro_use]
mod opcode;
mod vm;
mod codegen;
mod codegen_state;
//mod ffi;
//mod ffi_impl;
pub mod jit;
mod lexer;
mod parser;
mod parser_ast;


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

pub struct Bytecode {
    root: Proto,
}

impl Bytecode {
    pub fn parse_bytes(bytes: impl AsRef<[u8]>) -> Result<Self, ErrorWithPosition<Infallible, CodeGenerationError>> {
        let proto = BytecodeGenerator::new().parse_bytes(bytes)?;
        Ok(Self::from_proto(proto))
    }

    pub fn parse(read: impl io::Read) -> Result<Self, ErrorWithPosition<io::Error, CodeGenerationError>> {
        let proto = BytecodeGenerator::new().parse_read(read)?;
        Ok(Self::from_proto(proto))
    }

    fn from_proto(root: Proto) -> Self {
        Self { root }
    }
}
