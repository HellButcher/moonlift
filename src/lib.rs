use std::{convert::Infallible, io};

use ast::Block;
use lexer::Lexer;
use parser::Parser;

mod ast;
mod lexer;
mod parser;

#[derive(thiserror::Error)]
pub enum Error<E = Infallible> {
    ParseError(#[from] parser::ParseError<E>),
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
        let block = Parser::new(&mut lexer).parse()?;
        Ok(Self::from_block(block)?)
    }
    pub fn read(read: impl io::Read) -> Result<Self, Error<io::Error>> {
        let mut lexer = lexer::Lexer::read(read);
        let block = Parser::new(&mut lexer).parse()?;
        Ok(Self::from_block(block)?)
    }
    fn from_block(block: Block) -> Result<Self, ModuleError> {
        Ok(Self { block })
    }
}
