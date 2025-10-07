use std::sync::Mutex;

use crate::opcode::OpCode;

pub struct Module {
    pub functions: Vec<Function>,
}

pub struct Function {
    pub name: String,
    pub blocks: Vec<Block>,
}

pub struct Block {
    pub statements: Vec<OpCode>,
}

pub struct FunctionBuilder {
    name: String,
    blocks: Mutex<Vec<Block>>,
}

pub struct BlockBuilder<'a> {
    function: &'a FunctionBuilder,
    block: usize,
    statements: Vec<OpCode>,
}

impl FunctionBuilder {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            blocks: Mutex::new(Vec::new()),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn block(&self) -> BlockBuilder<'_> {
        let mut blocks = self.blocks.lock().unwrap();
        let block = blocks.len();
        blocks.push(Block {
            statements: Vec::new(),
        });
        BlockBuilder {
            function: self,
            block,
            statements: Vec::new(),
        }
    }
    pub fn build(self) -> Function {
        Function {
            name: self.name,
            blocks: self.blocks.into_inner().unwrap(),
        }
    }
}

impl BlockBuilder<'_> {
    pub fn number(&self) -> usize {
        self.block
    }

    pub fn push(&mut self, op: OpCode) {
        self.statements.push(op);
    }
}
impl Drop for BlockBuilder<'_> {
    fn drop(&mut self) {
        let mut blocks = self.function.blocks.lock().unwrap();
        blocks[self.block].statements = std::mem::take(&mut self.statements);
    }
}
