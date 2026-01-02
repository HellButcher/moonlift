use crate::{
    gc::Gc,
    val::{TypeTag, Value},
};

pub struct VM {
    pub pc: usize,
    pub gc: Gc,
    pub global: Value,
}

impl VM {
    pub fn get(&self, reg: u8) -> Value {
        todo!()
    }
    pub fn set(&mut self, var: u8, val: Value) {
        todo!()
    }
    pub fn get_uv(&self, uv: u8) -> Value {
        todo!()
    }
    pub fn set_uv(&mut self, uv: u8, val: Value) {
        todo!()
    }
    pub fn get_table(&self, var: u8, key: Value) -> Value {
        todo!()
    }
    pub fn set_table(&mut self, var: u8, key: Value, val: Value) {
        todo!()
    }
    pub fn get_const(&self, k: u16) -> Value {
        todo!()
    }
    pub fn get_func(&self, p: u16) -> Value {
        todo!()
    }
}
