use std::{convert::Infallible, io};

use ast::Block;
use lexer::{Lexer, Position};
use parser::Parser;

mod ast;
pub mod jit;
mod lexer;
mod parser;

#[derive(thiserror::Error, Debug)]
pub enum Error<E = Infallible> {
    #[error("Parse error at {1}: {0}")]
    ParseError(parser::ParseError<E>, Position),
    #[error(transparent)]
    ModuleError(#[from] ModuleError),
}

#[derive(thiserror::Error, Debug)]
pub enum ModuleError {}

pub struct Source {
    block: Block,
}

impl Source {
    pub fn from_bytes(bytes: impl AsRef<[u8]>) -> Result<Self, Error> {
        let mut lexer = Lexer::from_bytes(bytes);
        let mut parser = Parser::new(&mut lexer);
        match parser.parse() {
            Ok(block) => Ok(Self::from_block(block)?),
            Err(e) => Err(Error::ParseError(e, lexer.position())),
        }
    }
    pub fn read(read: impl io::Read) -> Result<Self, Error<io::Error>> {
        let mut lexer = lexer::Lexer::read(read);
        let mut parser = Parser::new(&mut lexer);
        match parser.parse() {
            Ok(block) => Ok(Self::from_block(block)?),
            Err(e) => Err(Error::ParseError(e, lexer.position())),
        }
    }
    fn from_block(block: Block) -> Result<Self, ModuleError> {
        Ok(Self { block })
    }
}
