use std::collections::HashMap;
// Direct Bytecode Parser Integration für Moonlift

use std::collections::hash_map::Entry;
use std::hash::Hash;
use std::ops::Deref;
use std::{cell::Cell, ops::Range};

use crate::opcode::{Op, OpCode};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum CodeGenerationError {
    #[error("Variable '{0}' not found")]
    VariableNotFound(String),
    #[error("Too many local variables (max 255)")]
    TooManyVariables,
    #[error("Too many constants (max 65535)")]
    TooManyConstants,
    #[error("Invalid OpCode {0:?}")]
    InvalidOpCode(OpCode),
}


use std::pin::Pin;

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Float(f64),
    Integer(i64),
    String(Pin<Box<[u8]>>),
    // ggf. weitere Typen
}

struct StrPtr(*const [u8]);
impl PartialEq for StrPtr {
    fn eq(&self, other: &StrPtr) -> bool {
        let (this, that) = unsafe { ( self.0.as_ref().unwrap(), other.0.as_ref().unwrap())};
        this == that
    }
}
impl Eq for StrPtr {}

impl Hash for StrPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let this = unsafe {  self.0.as_ref().unwrap() };
        this.hash(state);
    }
}

pub struct ConstantPool {
    constants: Vec<Constant>,
    string_map: HashMap<StrPtr, u16>, // pointer to string data -> idx
    number_map: HashMap<i64, u16>, // integer and float bits
}

impl<'a> ConstantPool{
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            string_map: HashMap::new(),
            number_map: HashMap::new(),
        }
    }

    pub fn into(self) -> Vec<Constant> {
        self.constants
    }

    pub fn from(constants: Vec<Constant>) -> Self{
        let mut result = Self::new();
        result.constants = constants;
        for (idx, constant) in result.constants.iter().enumerate() {
            let idx = idx as u16;
            match constant {
                Constant::String(s) => {
                    let ptr = StrPtr(s.deref());
                    result.string_map.insert(ptr, idx);
                }
                Constant::Integer(i) => {
                    result.number_map.insert(*i, idx);
                }
                Constant::Float(f) => {
                    let bits = f.to_bits() as i64;
                    result.number_map.insert(bits, idx);
                }
            }
        }
        result
    }

    pub fn add_string(&mut self, string: Box<[u8]>) -> Result<u16, CodeGenerationError> {
        let pin = Pin::new(string);
        let ptr = StrPtr(pin.deref());
        match self.string_map.entry(ptr) {
            Entry::Occupied(o) => Ok(*o.get()),
            Entry::Vacant(v) => {
                let idx = self.constants.len();
                if idx > u16::MAX as usize {
                    return Err(CodeGenerationError::TooManyConstants);
                }
                let idx = idx as u16;
                self.constants.push(Constant::String(pin.clone()));
                v.insert(idx);
                Ok(idx)
            }
        }
    }

    pub fn add_float(&mut self, f: f64) -> Result<u16, CodeGenerationError> {
        let bits = f.to_bits() as i64;
        match self.number_map.entry(bits) {
            Entry::Occupied(o) => Ok(*o.get()),
            Entry::Vacant(v) => {
                let idx = self.constants.len();
                if idx > u16::MAX as usize {
                    return Err(CodeGenerationError::TooManyConstants);
                }
                let idx = idx as u16;
                self.constants.push(Constant::Float(f));
                v.insert(idx);
                Ok(idx)
            }
        }
    }

    pub fn add_integer(&mut self, i: i64) -> Result<u16, CodeGenerationError> {
        match self.number_map.entry(i) {
            Entry::Occupied(o) => Ok(*o.get()),
            Entry::Vacant(v) => {
                let idx = self.constants.len();
                if idx > u16::MAX as usize {
                    return Err(CodeGenerationError::TooManyConstants);
                }
                let idx = idx as u16;
                self.constants.push(Constant::Integer(i));
                v.insert(idx);
                Ok(idx)
            }
        }
    }

    pub fn get(&self, idx: u16) -> Option<&Constant> {
        self.constants.get(idx as usize)
    }
}

/// Hilfsstruktur für lokale Variablen und temporäre Werte
pub struct Frame<'a> {
    next_slot: u8, // nächster freier Slot
    max_num_slots: Cell<u8>, // höchster benutzter Slot (für locals_count)
    vars: std::collections::HashMap<String, u8>, // Name -> Slot
    parent: Option<&'a Frame<'a>>,
}

impl Frame<'static> {
    pub fn new() -> Self {
        Self {
            next_slot: 0,
            max_num_slots: Cell::new(0),
            vars: std::collections::HashMap::new(),
            parent: None,
        }
    }
}
impl<'a> Frame<'a> {
    pub fn nested<'b>(parent: &'a Frame<'b>) -> Self {
        Self {
            next_slot: parent.next_slot,
            max_num_slots: Cell::new(parent.next_slot),
            vars: std::collections::HashMap::new(),
            parent: Some(parent),
        }
    }

    fn update_max_num_slots(&self, num_slots: u8) {
        if num_slots > self.max_num_slots.get() {
            self.max_num_slots.set(num_slots);
            if let Some(parent) = self.parent {
                parent.update_max_num_slots(num_slots);
            }
        }
    }

    /// Holt den Slot einer Variable
    pub fn get(&self, name: &str) -> Option<u8> {
        if let Some(&slot) = self.vars.get(name) {
            Some(slot)
        } else if let Some(parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    /// Reserviert einen Slot für eine Variable
    pub fn alloc(&mut self, name: &str) -> u8 {
        let slot = self.alloc_temp();
        self.vars.insert(name.to_string(), slot);
        slot
    }

    /// Reserviert einen temporären Slot
    pub fn alloc_temp(&mut self) -> u8 {
        let slot = self.next_slot;
        self.next_slot += 1;
        self.update_max_num_slots(self.next_slot);
        slot
    }

    /// Reserves multiple slots
    pub fn alloc_temps(&mut self, count: u8) -> Range<u8> {
        let start = self.next_slot;
        self.next_slot += count;
        self.update_max_num_slots(self.next_slot);
        start..self.next_slot
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProgramCounter(pub usize);

impl ProgramCounter {
    #[inline]
    pub const fn as_offset_relative_to(self, relative_to_pc: Self) -> u16 {
        (self.0 as isize - relative_to_pc.0 as isize) as u16
    }
}

pub struct Proto {
    pub num_params: u8,
    pub is_vararg: bool,
    pub max_stack_size: u8,
    pub bytecode: Vec<Op>,
    pub constants: Vec<Constant>,
    pub upvalues: Vec<String>, // names of upvalues
    pub protos: Vec<Proto>, // nested prototypes
}

pub struct BytecodeGenerator {
    pub bytecode: Vec<Op>,
    pub frame: Frame<'static>,
    pub constants: ConstantPool,
    pub upvalues: Vec<String>, // names of upvalues
    pub protos: Vec<Proto>, // nested prototypes
}

impl BytecodeGenerator {
    pub fn new() -> Self {
        Self {
            bytecode: Vec::new(),
            frame: Frame::new(),
            constants: ConstantPool::new(),
            upvalues: Vec::new(),
            protos: Vec::new(),
        }
    }

    #[inline]
    pub const fn pc(&self) -> ProgramCounter {
        ProgramCounter(self.bytecode.len())
    }

    /// Returns the instruction that controls the jump at `jump_op_pc`.
    /// THis is either the conditional Opcode before the jump, or the jump itself for unconditional jumps.
    #[inline]
    fn get_jmp_ctrl_mut(&mut self, jump_op_pc: ProgramCounter) -> &mut Op {
        let (a,b)  = self.bytecode.split_at_mut(jump_op_pc.0);
        let jump_op = &mut b[0];
        #[cfg(debug_assertions)]
        if !matches!(jump_op, Op::Jmp(_)) {
            panic!("get_jmp_ctrl called on non-jump opcode");
        }
        let Some(prev_op) = a.last_mut() else {
            return jump_op; // Kein vorheriger Opcode
        };
        prev_op
    }

    pub fn negate_jmp_ctrl(&mut self, jump_op_pc: ProgramCounter) -> bool {
        self.get_jmp_ctrl_mut(jump_op_pc).negate()
    }

    /// Returns the target PC of a jump instruction
    #[inline]
    pub fn get_jmp_target(&self, jump_op_pc: ProgramCounter) -> ProgramCounter {
        let Op::Jmp(args) = &self.bytecode[jump_op_pc.0] else {
            panic!("get_jmp_target called on non-jump opcode");
        };
        ProgramCounter((jump_op_pc.0 as isize + args.d as isize) as usize)
    }

    /// Updates a jump instruction to point to a new target PC
    pub fn update_jmp_target(&mut self, jump_op_pc: ProgramCounter, target_pc: ProgramCounter) {
        let Op::Jmp(args) = &mut self.bytecode[jump_op_pc.0] else {
            panic!("update_jmp_target called on non-jump opcode");
        };
        args.d = target_pc.as_offset_relative_to(jump_op_pc);
    }

    #[inline]
    pub fn update_jmp_here(&mut self, jump_op_pc: ProgramCounter) {
        let target_pc = self.pc();
        self.update_jmp_target(jump_op_pc, target_pc);
    }

    pub fn emit_jmp_placeholder(&mut self) -> ProgramCounter {
        self.emit(Op::Jmp(crate::opcode::AD { a: 0, d: 0 }))
    }

    pub fn emit_jmp(&mut self, target_pc: ProgramCounter) -> ProgramCounter {
        self.emit_jmp_x(target_pc, 0)
    }

    pub fn emit_jmp_x(&mut self, target_pc: ProgramCounter, arg: u8) -> ProgramCounter {
        let offset = target_pc.as_offset_relative_to(self.pc());
        self.emit(Op::Jmp(crate::opcode::AD { a: arg, d: offset }))
    }

    #[inline]
    pub fn emit(&mut self, op: Op) -> ProgramCounter{
        let pc = self.pc();
        self.bytecode.push(op);
        pc
    }
}

impl std::ops::Index<ProgramCounter> for BytecodeGenerator {
    type Output = Op;

    #[inline]
    fn index(&self, index: ProgramCounter) -> &Self::Output {
        &self.bytecode[index.0]
    }
}

impl std::ops::IndexMut<ProgramCounter> for BytecodeGenerator {
    #[inline]
    fn index_mut(&mut self, index: ProgramCounter) -> &mut Self::Output {
        &mut self.bytecode[index.0]
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum ExprValue {
    #[default]
    Void,
    Nil,
    Bool(bool),
    Const(u16), // Index in Constant Pool
    ConstFloat(f64),
    ConstInt(i64),
    ConstString(Box<[u8]>),
    Local(u8),   // Register/Slot
    NonReloc(u8),   // Register/Slot
    Reloc(ProgramCounter),   // instruction with relocatable register A
    Upval(u8),  // Upvalue Index
    Idx{ table_slot: u8, key_slot: u8 }, // table[key]
    IdxI{ table_slot: u8, key_value: u8 }, // table[key] where key is integer constant
    IdxStr{ table_slot: u8, key_const: u8 }, // table[key] where key is constant index
    IdxUp{ table_uv: u8, key_const: u8 }, // table[key] where key is constant index
    Jmp(ProgramCounter), // pc of the jump instruction
    Call(ProgramCounter), // pc of the call instruction
    VarArg(ProgramCounter), // pc of the VarArg instruction
}
