use core::panic;
use std::collections::HashMap;
// Direct Bytecode Parser Integration für Moonlift

use std::collections::hash_map::Entry;
use std::fmt;
use std::hash::Hash;
use std::ops::{Add, BitAnd, BitOr, BitXor, Deref, DerefMut, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub};
use std::{cell::Cell, ops::Range};

use crate::ast::{InfixOp, UnaryOp};
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

#[derive(Clone, PartialEq)]
pub enum Constant {
    Float(f64),
    Integer(i64),
    String(Pin<Box<[u8]>>),
    // ggf. weitere Typen
}

impl fmt::Debug for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Float(v) => write!(f, "Float({})", v),
            Constant::Integer(v) => write!(f, "Integer({})", v),
            Constant::String(s) => {
                if let Ok(s) = std::str::from_utf8(s) {
                    write!(f, "String({:?})", s)
                } else {
                    write!(f, "String({:?})", s)
                }
            }
        }
    }
}

struct StrPtr(*const [u8]);
impl PartialEq for StrPtr {
    fn eq(&self, other: &StrPtr) -> bool {
        let (this, that) = unsafe { (self.0.as_ref().unwrap(), other.0.as_ref().unwrap()) };
        this == that
    }
}
impl Eq for StrPtr {}

impl Hash for StrPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let this = unsafe { self.0.as_ref().unwrap() };
        state.write(this);
    }
}

pub struct ConstantPool {
    constants: Vec<Constant>,
    string_map: HashMap<StrPtr, u16>, // pointer to string data -> idx
    number_map: HashMap<i64, u16>,    // integer and float bits
}

impl<'a> ConstantPool {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            string_map: HashMap::new(),
            number_map: HashMap::new(),
        }
    }

    pub fn into_boxed_slice(self) -> Box<[Constant]> {
        self.constants.into_boxed_slice()
    }

    pub fn from_boxed_slice(constants: Box<[Constant]>) -> Self {
        let mut result = Self::new();
        result.constants = constants.into_vec();
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

    fn next_idx(&self) -> Result<u16, CodeGenerationError> {
        let idx = self.constants.len();
        if idx > u16::MAX as usize {
            return Err(CodeGenerationError::TooManyConstants);
        }
        Ok(idx as u16)
    }

    pub fn add_const(&mut self, constant: Constant) -> Result<u16, CodeGenerationError> {
        match constant {
            Constant::String(s) => self.add_string(Pin::into_inner(s)),
            Constant::Float(f) => self.add_float(f),
            Constant::Integer(i) => self.add_integer(i),
        }
    }

    pub fn add_string(&mut self, string: Box<[u8]>) -> Result<u16, CodeGenerationError> {
        let pin = Pin::new(string);
        let ptr = StrPtr(pin.deref());
        let idx = self.next_idx()?;
        match self.string_map.entry(ptr) {
            Entry::Occupied(o) => Ok(*o.get()),
            Entry::Vacant(v) => {
                self.constants.push(Constant::String(pin));
                v.insert(idx);
                Ok(idx)
            }
        }
    }

    pub fn add_float(&mut self, f: f64) -> Result<u16, CodeGenerationError> {
        let bits = f.to_bits() as i64;
        let idx = self.next_idx()?;
        match self.number_map.entry(bits) {
            Entry::Occupied(o) => Ok(*o.get()),
            Entry::Vacant(v) => {
                self.constants.push(Constant::Float(f));
                v.insert(idx);
                Ok(idx)
            }
        }
    }

    pub fn add_integer(&mut self, i: i64) -> Result<u16, CodeGenerationError> {
        let idx = self.next_idx()?;
        match self.number_map.entry(i) {
            Entry::Occupied(o) => Ok(*o.get()),
            Entry::Vacant(v) => {
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

impl fmt::Debug for ConstantPool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.constants.iter()).finish()
    }
}

pub type Reg = u8;
pub const NO_REG: Reg = !0;

#[derive(Debug)]
/// Hilfsstruktur für lokale Variablen und temporäre Werte
pub struct Frame {
    next_slot: u8,           // nächster freier Slot
    max_num_slots: u8,       // höchster benutzter Slot (für locals_count)
    vars: Vec<(String, u8)>, // (Name,Slot)
    scopes: Vec<u8>,         // next_slot at scope start
}

impl Frame {
    pub const fn new() -> Self {
        Self {
            next_slot: 0,
            max_num_slots: 0,
            vars: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(self.next_slot);
    }

    pub fn leave_scope(&mut self) {
        let scope_start = self.scopes.pop().expect("no scope to leave");
        self.next_slot = scope_start;
        while let Some((_, slot)) = self.vars.last() {
            if *slot >= self.next_slot {
                self.vars.pop();
            } else {
                break;
            }
        }
    }

    /// Holt den Slot einer Variable
    pub fn get(&self, name: &str) -> Option<Reg> {
        for (cur, slot) in self.vars.iter().rev() {
            if cur == name {
                return Some(*slot);
            }
        }
        None
    }

    /// Reserviert einen Slot für eine Variable
    pub fn alloc(&mut self, name: String) -> Reg {
        let slot = self.alloc_temp();
        self.vars.push((name, slot));
        slot
    }

    /// Reserviert einen temporären Slot
    pub fn alloc_temp(&mut self) -> Reg {
        let slot = self.next_slot;
        self.next_slot += 1;
        if self.max_num_slots < self.next_slot {
            self.max_num_slots = self.next_slot;
        }
        slot
    }

    /// Reserves multiple slots
    pub fn alloc_temps(&mut self, count: u8) -> Range<Reg> {
        let start = self.next_slot;
        self.next_slot += count;
        if self.max_num_slots < self.next_slot {
            self.max_num_slots = self.next_slot;
        }
        start..self.next_slot
    }

    #[inline]
    pub fn free(&mut self, reg: Reg) {
        self.free_range(reg..reg+1);
    }

    pub fn free_range(&mut self, range: Range<Reg>) {
        if range.start < *self.scopes.last().unwrap() {
            panic!("Can only free registers from current scope");
        }
        if let Some((_, last_var)) = self.vars.last() {
            if range.start <= *last_var {
                // TODO: TBD is this panic correct, or just ignore
                panic!("Can only free named variables");
                // range.start = *last_var + 1;
                // if range.start >= range.end {
                //     return;
                // }
            }
        }
        if range.end != self.next_slot {
            panic!("Can only free the last allocated registers");
        }
        self.next_slot = range.start;
    }

    pub fn free2(&mut self, reg1: Reg, reg2: Reg) {
        if reg1 > reg2 {
            self.free(reg1);
            self.free(reg2);
        } else {
            self.free(reg2);
            self.free(reg1);
        }
    }

}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProgramCounter(pub usize);

impl ProgramCounter {

    pub const NO_JUMP: Self = Self(!0);

    #[inline]
    pub const fn as_offset_relative_to(self, relative_to_pc: Self) -> i16 {
        (self.0 as isize - relative_to_pc.0 as isize) as i16
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct JumpList(pub ProgramCounter);


impl JumpList {
    pub const NO_JUMP: Self = Self(ProgramCounter::NO_JUMP);

    #[inline]
    pub const fn has_jumps(&self) -> bool {
        self.0.0 != !0
    }
}

#[derive(Debug, Clone)]
pub struct Proto {
    pub num_params: u8,
    pub is_vararg: bool,
    pub num_upvalues: u8,
    pub max_stack_size: u8,
    pub bytecode: Box<[Op]>,
    pub constants: Box<[Constant]>,
    pub protos: Box<[Proto]>,    // nested prototypes
}

#[derive(Debug)]
pub struct ProtoGenerator {
    pub num_params: u8,
    pub is_vararg: bool,
    pub bytecode: Vec<Op>,
    pub frame: Frame,
    pub constants: ConstantPool,
    pub upvalues: Vec<String>, // names of upvalues
    pub protos: Vec<Proto>,    // nested prototypes
    pub ifjumps: Vec<(bool, bool, JumpList)>, // jump-lists for pending if-then-else
    pub loops: Vec<(bool, ProgramCounter, JumpList)>, // loop start positions & end jump-lists
    pub dead: bool,
}

#[derive(Debug)]
pub struct BytecodeGenerator {
    pub protos: Vec<ProtoGenerator>
}

impl BytecodeGenerator {
    #[inline]
    pub const fn new() -> Self {
        Self {
            protos: Vec::new()
        }
    }

    pub fn current_proto_mut(&mut self) -> &mut ProtoGenerator {
        self.protos.last_mut().expect("no current proto")
    }
}

impl Deref for BytecodeGenerator {
    type Target = ProtoGenerator;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.protos.last().expect("no current proto")
    }
}

impl DerefMut for BytecodeGenerator {

    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.protos.last_mut().expect("no current proto")
    }
}


impl ProtoGenerator {
    pub fn new(is_vararg: bool, args: Vec<String>) -> Self {
        let num_params = args.len() as u8;
        let mut frame = Frame::new();
        for arg in args {
            frame.alloc(arg);
        }
        Self {
            num_params,
            is_vararg,
            bytecode: Vec::new(),
            frame,
            constants: ConstantPool::new(),
            upvalues: Vec::new(),
            protos: Vec::new(),
            ifjumps: Vec::new(),
            loops: Vec::new(),
            dead: false,
        }
    }

    pub fn into_proto(self) -> Proto {
        Proto {
            num_params: self.num_params,
            is_vararg: self.is_vararg,
            num_upvalues: self.upvalues.len() as u8,
            max_stack_size: self.frame.max_num_slots,
            bytecode: self.bytecode.into_boxed_slice(),
            constants: self.constants.into_boxed_slice(),
            protos: self.protos.into_boxed_slice(),
        }
    }

    /// Returns the current program counter (next instruction to be emitted)
    #[inline]
    pub const fn pc(&self) -> ProgramCounter {
        ProgramCounter(self.bytecode.len())
    }

    /// Returns the instruction that controls the jump at `jump_op_pc`.
    /// THis is either the conditional Opcode before the jump, or the jump itself for unconditional jumps.
    #[inline]
    fn get_jmp_ctrl_mut(&mut self, jump_op_pc: ProgramCounter) -> &mut Op {
        let (a, b) = self.bytecode.split_at_mut(jump_op_pc.0);
        let jump_op = &mut b[0];
        #[cfg(debug_assertions)]
        if !matches!(jump_op.opcode(), OpCode::Jmp) {
            panic!("get_jmp_ctrl called on non-jump opcode");
        }
        let Some(prev_op) = a.last_mut() else {
            return jump_op; // Kein vorheriger Opcode
        };
        prev_op
    }

    /// Negates the condition of a jump instruction.
    /// The condition is the instruction before the jump, or the jump itself for unconditional jumps.
    pub fn negate_jmp_ctrl(&mut self, jump_op_pc: ProgramCounter) -> bool {
        self.get_jmp_ctrl_mut(jump_op_pc).negate()
    }

    /// Returns the target PC of a jump instruction
    #[inline]
    pub fn get_jmp_target(&self, jump_op_pc: ProgramCounter) -> ProgramCounter {
        let jump_op = &self.bytecode[jump_op_pc.0];
        if !matches!(jump_op.opcode(), OpCode::Jmp) {
            panic!("get_jmp_target called on non-jump opcode");
        }
        let d = jump_op.args().d();
        if d == !0 {
            return ProgramCounter::NO_JUMP;
        } else {
            ProgramCounter((jump_op_pc.0 as isize + d as isize) as usize)
        }
    }

    /// Updates a jump instruction to point to a new target PC
    pub fn patch_jmp_target(&mut self, jump_op_pc: ProgramCounter, target_pc: ProgramCounter) {
        let jump_op = &mut self.bytecode[jump_op_pc.0];
        let new_offset = target_pc.as_offset_relative_to(jump_op_pc);
        match_op! {(jump_op) {
            Jmp(_, ref mut target) => {
                *target = new_offset;
            },
            _ => panic!("update_jmp_target called on non-jump opcode"),

        }}
    }

    /// Updates a list of jumps to point to a new target PC.
    /// The list is a linked list of jumps, where each jump's target is the next jump in the list, and the last jump points to `NO_JUMP`.
    pub fn patch_jmp_list_aux(
        &mut self,
        mut jump_list: JumpList,
        value_target_pc: ProgramCounter,
        dest: Reg,
        default_target_pc: ProgramCounter,
    ) {
        while jump_list != JumpList::NO_JUMP {
            let next_jump_pc = self.get_jmp_target(jump_list.0);
            if self.patch_test_set_dest(jump_list.0, dest) {
                self.patch_jmp_target(jump_list.0, value_target_pc);
            } else {
                self.patch_jmp_target(jump_list.0, default_target_pc);
            }
            jump_list = JumpList(next_jump_pc);
        }
    }

    /// Updates a list of jumps to point to a new target PC.
    /// The list is a linked list of jumps, where each jump's target is the next jump in the list, and the last jump points to `NO_JUMP`.
    pub fn patch_jmp_list(&mut self, jump_list: JumpList, target_pc: ProgramCounter) {
        self.patch_jmp_list_aux(jump_list, target_pc, NO_REG, target_pc);
    }

    #[inline]
    pub fn patch_jmp_list_here(&mut self, jump_list: JumpList) {
        let target_pc = self.pc();
        self.patch_jmp_list(jump_list, target_pc);
    }

    pub fn concat_jump_list(&mut self, list1: &mut JumpList, list2: JumpList) {
        if list2 == JumpList::NO_JUMP {
            return;
        }
        if *list1 == JumpList::NO_JUMP {
            *list1 = list2;
            return;
        }
        let mut list = *list1;
        loop {
            let next = JumpList(self.get_jmp_target(list.0));
            if next == JumpList::NO_JUMP {
                break;
            } else {
                list = next;
            }
        }
        // fix last element
        self.patch_jmp_target(list.0, list2.0);
    }

    #[inline]
    pub fn push_jump_list(&mut self, list1: &mut JumpList, jmp: ProgramCounter) {
        // jmp target should be NO_JUMP
        self.concat_jump_list(list1, JumpList(jmp));
    }


    /// Patches a IsTC/IsFC instruction to set its destination register to `dest`.
    /// If `dest` is NO_REG or the same as the source register, the
    /// instruction is patched to a simple IsT/IsF op (without setting register).
    pub fn patch_test_set_dest(&mut self, jump_op_pc: ProgramCounter, dest: Reg) -> bool {
        let jmp_ctrl = self.get_jmp_ctrl_mut(jump_op_pc);
        let new_op = match_op!((jmp_ctrl) {
            IsTC(_,d) => {
                if dest != NO_REG && dest != d {
                    op!(IsTC(dest, d))
                } else {
                    op!(IsT(d))
                }
            },
            IsFC(_,d) => {
                if dest != NO_REG && dest != d {
                    op!(IsFC(dest, d))
                } else {
                    op!(IsF(d))
                }
            },
            _ => return false,
        });
        *jmp_ctrl = new_op;
        true
    }

    /// Patches a jump instruction to point to the current PC.
    #[inline]
    pub fn patch_jmp_here(&mut self, jump_op_pc: ProgramCounter) {
        let target_pc = self.pc();
        self.patch_jmp_target(jump_op_pc, target_pc);
    }

    pub fn emit_jmp_placeholder(&mut self) -> ProgramCounter {
        self.emit(op![Jmp(0, !0)])
    }

    pub fn emit_jmp(&mut self, target_pc: ProgramCounter) -> ProgramCounter {
        self.emit_jmp_x(target_pc, 0)
    }

    pub fn emit_jmp_x(&mut self, target_pc: ProgramCounter, arg: u8) -> ProgramCounter {
        let offset = target_pc.as_offset_relative_to(self.pc());
        self.emit(op![Jmp(arg, offset)])
    }

    #[inline]
    pub fn emit(&mut self, op: Op) -> ProgramCounter {
        if self.dead {
            return ProgramCounter::NO_JUMP;
        }
        let pc = self.pc();
        self.bytecode.push(op);
        pc
    }

    pub fn patch_one_ret_pc(&mut self, pc: ProgramCounter) -> Option<ExprValue> {
        match_op! {(&mut self.bytecode[pc.0]) {
            Call(a,ref mut b,_) => {
                *b = 2; // 1 result
                Some(ExprValue::NonReloc(a)) // result is in base register
            },
            VArg(_,ref mut b,_) => {
                *b = 2; // 1 result
                Some(ExprValue::Reloc(pc))
            },
            _ => None,
        }}
    }

    /// Fix an expression to return one result.
    /// If expression is not a multi-ret expression (function call or vararg), it already returns one result, so nothing needs to be done.
    /// Function calls become `NonReloc` expressions (as its result comes fixed in the base register of the call).
    /// vararg expressions become `Reloc`` as the opcode allows to puts its results where it wants.
    pub fn patch_one_ret(&mut self, value: ExprValue) -> ExprValue {
        match value {
            ExprValue::Call(pc) | ExprValue::VarArg(pc) => {
                self.patch_one_ret_pc(pc).expect("invalid op")
            }
            _ => value,
        }
    }

    /// Converts an expression value into a relocatable or non-relocatable form.
    /// This is analogous to Lua's `luaK_dischargevars`, ensuring that constants,
    /// locals, upvalues, and indexed accesses are properly loaded or moved into registers.
    /// Jumps and calls are handled as pending relocations.
    pub fn discharge_vars_mut(&mut self, value: &mut ExprValue) {
        match value {
            ExprValue::Void
            | ExprValue::Const(_)
            | ExprValue::Jmp(_)
            | ExprValue::NonReloc(_)
            | ExprValue::Reloc(_) => {},
            ExprValue::Local(r) => {
                *value = ExprValue::NonReloc(*r);
            },
            ExprValue::Global(k) => {
                let pc = self.emit(op![GGet(0, *k)]);
                *value = ExprValue::Reloc(pc);
            },
            ExprValue::Upval(uv) => {
                let pc = self.emit(op![UGet(0, *uv)]);
                *value = ExprValue::Reloc(pc);
            }
            ExprValue::Idx {
                table_slot,
                key_slot,
            } => {
                let pc = self.emit(op![TGetV(0, *table_slot, *key_slot)]);
                *value = ExprValue::Reloc(pc);
            }
            ExprValue::IdxI {
                table_slot,
                key_value,
            } => {
                let pc = self.emit(op![TGetB(0, *table_slot, *key_value)]);
                *value = ExprValue::Reloc(pc);
            }
            ExprValue::IdxStr {
                table_slot,
                key_const,
            } => {
                let pc = self.emit(op![TGetS(0, *table_slot, *key_const)]);
                *value = ExprValue::Reloc(pc);
            }
            ExprValue::Call(pc) | ExprValue::VarArg(pc) => {
                *value = self.patch_one_ret_pc(*pc).expect("invalid op")
            }
        }
    }


    #[inline]
    pub fn discharge_vars(&mut self, mut value: ExprValue) -> ExprValue {
        self.discharge_vars_mut(&mut value);
        value
    }

    pub fn emit_const_to_reg(
        &mut self,
        value: ConstValue,
        dest: u8,
    ) -> Result<ProgramCounter, CodeGenerationError> {
        match value {
            ConstValue::Nil => Ok(self.emit(op![KPri(dest, 0)])),
            ConstValue::Bool(false) => Ok(self.emit(op![KPri(dest, 1)])),
            ConstValue::Bool(true) => Ok(self.emit(op![KPri(dest, 2)])),
            ConstValue::Int(i) => {
                if i16::MIN as i64 <= i && i <= i16::MAX as i64 {
                    Ok(self.emit(op![KShort(dest, i as i16)]))
                } else {
                    let k = self.constants.add_integer(i)?;
                    Ok(self.emit(op![KNum(dest, k)]))
                }
            }
            ConstValue::Float(f) => {
                let k = self.constants.add_float(f)?;
                Ok(self.emit(op![KNum(dest, k)]))
            }
            ConstValue::Str(s) => {
                let k = self.constants.add_string(s)?;
                Ok(self.emit(op![KStr(dest, k)]))
            }
        }
    }

    /// Ensures the value is loaded into the given register.
    /// This is analogous to Lua's `discharge2reg`, emitting the correct LOAD/MOVE opcode
    /// depending on the value type. Jumps are not loaded into registers.
    pub fn discharge_to_reg_mut(
        &mut self,
        value: &mut ExprValue,
        dest: u8,
    ) -> Result<DischargedRegOrJmp, CodeGenerationError> {
        self.discharge_vars_mut(value);
        let taken_value = std::mem::replace(value, ExprValue::NonReloc(dest));
        match taken_value {
            ExprValue::Const(c) => {
                self.emit_const_to_reg(c, dest)?;
            }
            ExprValue::NonReloc(r) if r == dest => {}
            ExprValue::NonReloc(r) => {
                self.emit(op![Mov(dest, r)]);
            }
            ExprValue::Reloc(pc) => {
                self[pc].set_a_dst(dest).expect("invalid op");
            }
            ExprValue::Jmp(jump) => {
                // Jumps cannot be loaded into registers, but we keep the jump list for patching
                *value = ExprValue::Jmp(jump);
                return Ok(DischargedRegOrJmp::Jmp(jump));
            }
            _ => panic!("Cannot discharge expression"),
        }
        Ok(DischargedRegOrJmp::Reg(dest))
    }

    #[inline]
    pub fn discharge_to_reg(
        &mut self,
        mut value: ExprValue,
        dest: u8,
    ) -> Result<DischargedRegOrJmp, CodeGenerationError> {
        self.discharge_to_reg_mut(&mut value, dest)
    }

    /// Ensures the value is loaded into any register (allocates a temp if needed).
    /// This is analogous to Lua's `discharge2anyreg`.
    pub fn discharge_to_any_reg_mut(
        &mut self,
        value: &mut ExprValue,
    ) -> Result<DischargedRegOrJmp, CodeGenerationError> {
        match value {
            ExprValue::NonReloc(r) => Ok(DischargedRegOrJmp::Reg(*r)),
            _ => {
                let dest = self.frame.alloc_temp();
                self.discharge_to_reg_mut(value, dest)
            }
        }
    }

    #[inline]
    pub fn discharge_to_any_reg(
        &mut self,
        mut value: ExprValue,
    ) -> Result<DischargedRegOrJmp, CodeGenerationError> {
        self.discharge_to_any_reg_mut(&mut value)
    }

    pub fn expr_to_reg(&mut self, expr: &mut Expr, dest: u8) -> Result<(), CodeGenerationError> {
        // TODO: handle jump lists
        match self.discharge_to_reg_mut(&mut expr.value, dest)? {
            DischargedRegOrJmp::Jmp(jump) => todo!(), // TODO: handle jumps
            DischargedRegOrJmp::Reg(r) => {
                debug_assert_eq!(dest, r);
                Ok(())
            }
        }
    }

    pub fn expr_to_next_reg(&mut self, expr: &mut Expr) -> Result<u8, CodeGenerationError> {
        self.discharge_vars_mut(&mut expr.value);
        self.expr_free(&expr.value);
        let dest = self.frame.alloc_temp();
        self.expr_to_reg(expr, dest)?;
        Ok(dest)
    }

    pub fn expr_to_any_reg(&mut self, expr: &mut Expr) -> Result<u8, CodeGenerationError> {
        match expr.value {
            ExprValue::NonReloc(r) => Ok(r),
            _ => {
                let dest = self.frame.alloc_temp();
                self.expr_to_reg(expr, dest)?;
                Ok(dest)
            }
        }
    }

    pub fn expr_free(&mut self, value: &ExprValue) {
        if let ExprValue::NonReloc(r) = value {
            self.frame.free(*r);
        }
    }

    pub fn expr_free2(&mut self, lhs: &ExprValue, rhs: &ExprValue) {
        match (lhs, rhs) {
            (ExprValue::NonReloc(r1), ExprValue::NonReloc(r2)) => {
                self.frame.free2(*r1, *r2);
            }
            (ExprValue::NonReloc(r), _) | (_, ExprValue::NonReloc(r)) => {
                self.frame.free(*r);
            }
            _ => {}
        }
    }

    /// Emit instruction to jump if 'e' is 'cond' (that is, if 'cond'
    /// is true, code will jump if 'e' is true.) Return jump position.
    /// Optimize when 'e' is 'not' something, inverting the condition
    /// and removing the 'not'.
    pub fn emit_jmp_condition(
        &mut self,
        value: ExprValue,
        condition: bool,
    ) -> ProgramCounter {
        let value = self.discharge_vars(value);
        if let ExprValue::Reloc(pc) = value {
            match_op!((self[pc]) {
                Not(a,d) => {
                    debug_assert_eq!(self.pc().0, pc.0 + 1);
                    if condition {
                        self[pc] = op![IsFC(a, d)];
                    } else {
                        self[pc] = op![IsTC(a, d)];
                    }
                    let pc = self.emit_jmp_placeholder();
                    return pc;
                },
                _ => {}
            });
        }
        let reg = self.discharge_to_any_reg(value).unwrap().unwrap_reg();
        self.frame.free(reg);
        if condition {
            self.emit(op![IsTC(0, reg)]);
        } else {
            self.emit(op![IsFC(0, reg)]);
        }
        self.emit_jmp_placeholder()
    }

    /// Emit code to go through if 'e' is truthy, jump otherwise.
    /// Returns the PC of the jump instruction that needs to be patched later.
    pub fn go_if_true(&mut self, expr: &mut Expr) -> JumpList {
        let pc = match &expr.value {
            ExprValue::Jmp(jump_pc) => {
                if self.negate_jmp_ctrl(*jump_pc) {
                    *jump_pc
                } else {
                    panic!("Failed to negate jump condition")
                }
            },
            ExprValue::Const(c) if c.is_truthy() => {
                ProgramCounter::NO_JUMP // always true, no jump needed
            },
            _ => {
                self.emit_jmp_condition(expr.take(), false) // jump if false
            },
        };
        self.push_jump_list(&mut expr.jump_false, pc);
        self.patch_jmp_list_here(expr.jump_true);
        expr.jump_true = JumpList::NO_JUMP;
        expr.jump_false
    }

    /// Emit code to go through if 'e' is falsy, jump otherwise.
    /// Returns the PC of the jump instruction that needs to be patched later.
    pub fn go_if_false(&mut self, expr: &mut Expr) -> JumpList {
        let pc = match &expr.value {
            ExprValue::Jmp(jump_pc) => {
                // Already a conditional jump - return as is
                *jump_pc
            },
            ExprValue::Const(c) if c.is_falsy() => {
                ProgramCounter::NO_JUMP // always false, no jump needed
            },
            _ => {
                self.emit_jmp_condition(expr.take(), false) // jump if false
            },
        };
        self.push_jump_list(&mut expr.jump_true, pc);
        self.patch_jmp_list_here(expr.jump_false);
        expr.jump_false = JumpList::NO_JUMP;
        expr.jump_true
    }
}

impl std::ops::Index<ProgramCounter> for ProtoGenerator {
    type Output = Op;

    #[inline]
    fn index(&self, index: ProgramCounter) -> &Self::Output {
        &self.bytecode[index.0]
    }
}

impl std::ops::IndexMut<ProgramCounter> for ProtoGenerator {
    #[inline]
    fn index_mut(&mut self, index: ProgramCounter) -> &mut Self::Output {
        &mut self.bytecode[index.0]
    }
}

#[derive(Debug, Clone, Default)]
pub enum ConstValue {
    #[default]
    Nil,
    Bool(bool),
    Float(f64),
    Int(i64),
    Str(Box<[u8]>),
}

impl ConstValue {
    #[inline]
    pub const fn is_nil(&self) -> bool {
        matches!(self, ConstValue::Nil)
    }

    #[inline]
    pub const fn is_truthy(&self) -> bool {
        match self {
            ConstValue::Nil => false,
            ConstValue::Bool(b) => *b,
            ConstValue::Float(f) => *f != 0.0,
            ConstValue::Int(i) => *i != 0,
            ConstValue::Str(_) => true,
        }
    }

    #[inline]
    pub const fn is_falsy(&self) -> bool {
        !self.is_truthy()
    }

    #[inline]
    pub const fn is_float(&self) -> bool {
        matches!(self, ConstValue::Float(_))
    }

    #[inline]
    pub const fn as_i64(&self) -> i64 {
        match self {
            ConstValue::Int(i) => *i,
            ConstValue::Float(f) => *f as i64,
            ConstValue::Bool(true) => 1,
            ConstValue::Bool(false) => 0,
            ConstValue::Nil => 0,
            ConstValue::Str(_) => 0,
        }
    }

    #[inline]
    pub const fn as_f64(&self) -> f64 {
        match self {
            ConstValue::Int(i) => *i as f64,
            ConstValue::Float(f) => *f,
            ConstValue::Bool(true) => 1.0,
            ConstValue::Bool(false) => 0.0,
            ConstValue::Nil => 0.0,
            ConstValue::Str(_) => 0.0,
        }
    }

    #[inline]
    pub const fn fadd(&self, rhs: &Self) -> f64 {
        self.as_f64() + rhs.as_f64()
    }

    #[inline]
    pub const fn iadd(&self, rhs: &Self) -> i64 {
        self.as_i64() + rhs.as_i64()
    }

    #[inline]
    pub const fn fsub(&self, rhs: &Self) -> f64 {
        self.as_f64() - rhs.as_f64()
    }

    #[inline]
    pub const fn isub(&self, rhs: &Self) -> i64 {
        self.as_i64() - rhs.as_i64()
    }

    #[inline]
    pub const fn fmul(&self, rhs: &Self) -> f64 {
        self.as_f64() * rhs.as_f64()
    }

    #[inline]
    pub const fn imul(&self, rhs: &Self) -> i64 {
        self.as_i64() * rhs.as_i64()
    }

    #[inline]
    pub const fn fdiv(&self, rhs: &Self) -> f64 {
        self.as_f64() / rhs.as_f64()
    }

    #[inline]
    pub const fn idiv(&self, rhs: &Self) -> i64 {
        self.as_i64() / rhs.as_i64()
    }

    #[inline]
    pub const fn frem(&self, rhs: &Self) -> f64 {
        self.as_f64() % rhs.as_f64()
    }

    #[inline]
    pub const fn irem(&self, rhs: &Self) -> i64 {
        self.as_i64() % rhs.as_i64()
    }

    #[inline]
    pub const fn fminus(&self) -> f64 {
        -self.as_f64()
    }

    #[inline]
    pub const fn iminus(&self) -> i64 {
        -self.as_i64()
    }

    #[inline]
    pub fn pow(&self, rhs: &Self) -> ConstValue {
        match (self, rhs) {
            (a, ConstValue::Float(b)) => Self::Float(a.as_f64().powf(*b)),
            (ConstValue::Float(a), b) => Self::Float(a.powi(b.as_i64() as i32)),
            _ => Self::Int(self.as_i64().pow(rhs.as_i64() as u32)),
        }
    }

    #[inline]
    pub const fn len(&self) -> i64 {
        if let ConstValue::Str(s) = self {
            s.len() as i64
        } else {
            0 // always zero (only tables and strings have length)
        }
    }
}

impl Add for &ConstValue {
    type Output = ConstValue;

    fn add(self, rhs: Self) -> ConstValue {
        if self.is_float() || rhs.is_float() {
            ConstValue::Float(self.fadd(rhs))
        } else {
            ConstValue::Int(self.iadd(rhs))
        }
    }
}

impl Sub for &ConstValue {
    type Output = ConstValue;

    fn sub(self, rhs: Self) -> ConstValue {
        if self.is_float() || rhs.is_float() {
            ConstValue::Float(self.fsub(rhs))
        } else {
            ConstValue::Int(self.isub(rhs))
        }
    }
}

impl Mul for &ConstValue {
    type Output = ConstValue;

    fn mul(self, rhs: Self) -> ConstValue {
        if self.is_float() || rhs.is_float() {
            ConstValue::Float(self.fmul(rhs))
        } else {
            ConstValue::Int(self.imul(rhs))
        }
    }
}

impl Div for &ConstValue {
    type Output = ConstValue;

    fn div(self, rhs: Self) -> ConstValue {
        if self.is_float() || rhs.is_float() {
            ConstValue::Float(self.fdiv(rhs))
        } else {
            ConstValue::Int(self.idiv(rhs))
        }
    }
}

impl Rem for &ConstValue {
    type Output = ConstValue;

    fn rem(self, rhs: Self) -> ConstValue {
        if self.is_float() || rhs.is_float() {
            ConstValue::Float(self.frem(rhs))
        } else {
            ConstValue::Int(self.irem(rhs))
        }
    }
}

impl BitAnd for &ConstValue {
    type Output = ConstValue;

    fn bitand(self, rhs: Self) -> ConstValue {
        ConstValue::Int(self.as_i64() & rhs.as_i64())
    }
}

impl BitOr for &ConstValue {
    type Output = ConstValue;

    fn bitor(self, rhs: Self) -> ConstValue {
        ConstValue::Int(self.as_i64() | rhs.as_i64())
    }
}

impl BitXor for &ConstValue {
    type Output = ConstValue;

    fn bitxor(self, rhs: Self) -> ConstValue {
        ConstValue::Int(self.as_i64() ^ rhs.as_i64())
    }
}

impl Shl for &ConstValue {
    type Output = ConstValue;

    fn shl(self, rhs: Self) -> ConstValue {
        ConstValue::Int(self.as_i64() << rhs.as_i64())
    }
}

impl Shr for &ConstValue {
    type Output = ConstValue;

    fn shr(self, rhs: Self) -> ConstValue {
        ConstValue::Int(self.as_i64() >> rhs.as_i64())
    }
}

impl Neg for &ConstValue {
    type Output = ConstValue;

    fn neg(self) -> ConstValue {
        if self.is_float() {
            ConstValue::Float(-self.as_f64())
        } else {
            ConstValue::Int(-self.as_i64())
        }
    }
}

impl Not for &ConstValue {
    type Output = bool;

    fn not(self) -> bool {
        self.is_falsy()
    }
}

impl PartialEq for ConstValue {
    fn eq(&self, other: &Self) -> bool {
        if self.is_float() || other.is_float() {
            self.as_f64() == other.as_f64()
        } else {
            self.as_i64() == other.as_i64()
        }
    }
}

impl PartialOrd for ConstValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.is_float() || other.is_float() {
            self.as_f64().partial_cmp(&other.as_f64())
        } else {
            self.as_i64().partial_cmp(&other.as_i64())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum ExprValue {
    #[default]
    Void,
    Const(ConstValue),     // Constant value
    NonReloc(u8),          // Register/Slot
    Reloc(ProgramCounter), // instruction with relocatable register A
    Jmp(ProgramCounter),   // pc of the jump instruction
    Local(u8),             // Register/Slot
    Global(u16),           // Constant String Index
    Upval(u8),             // Upvalue Index
    Idx {
        table_slot: u8,
        key_slot: u8,
    }, // table[key]
    IdxI {
        table_slot: u8,
        key_value: u8,
    }, // table[key] where key is integer constant
    IdxStr {
        table_slot: u8,
        key_const: u8,
    }, // table[key] where key is constant index
    Call(ProgramCounter),  // pc of the call instruction
    VarArg(ProgramCounter), // pc of the VarArg instruction
}

pub struct Expr {
    pub value: ExprValue,
    pub jump_true: JumpList,  // pc of jump if true
    pub jump_false: JumpList, // pc of jump if false
}

impl Expr {
    #[allow(non_upper_case_globals)]
    pub const Void: Self = Self {
        value: ExprValue::Void,
        jump_true: JumpList::NO_JUMP,
        jump_false: JumpList::NO_JUMP,
    };

    #[inline]
    #[allow(non_snake_case)]
    pub const fn Reloc(pc: ProgramCounter) -> Self {
        Self::new(ExprValue::Reloc(pc))
    }

    #[inline]
    pub const fn new(value: ExprValue) -> Self {
        Self {
            value,
            jump_true: JumpList::NO_JUMP,
            jump_false: JumpList::NO_JUMP,
        }
    }

    #[inline]
    pub const fn has_jumps(&self) -> bool {
        self.jump_true.has_jumps() || self.jump_false.has_jumps()
    }

    #[inline]
    pub const fn is_void(&self) -> bool {
        matches!(self.value, ExprValue::Void)
    }

    #[inline]
    pub fn take(&mut self) -> ExprValue {
        self.replace(ExprValue::Void)
    }

    #[inline]
    pub fn replace(&mut self, new_value: ExprValue) -> ExprValue {
        std::mem::replace(&mut self.value, new_value)
    }

    #[inline]
    pub fn update(&mut self, f: impl FnOnce(ExprValue) -> ExprValue) {
        self.value = f(self.take());
    }
}

pub enum DischargedRegOrJmp {
    Reg(u8),
    Jmp(ProgramCounter),
}

impl ExprValue {
    #[allow(non_upper_case_globals)]
    pub const Nil: Self = Self::Const(ConstValue::Nil);

    #[allow(non_upper_case_globals)]
    pub const True: Self = Self::Bool(true);

    #[allow(non_upper_case_globals)]
    pub const False: Self = Self::Bool(false);

    #[allow(non_upper_case_globals)]
    pub const NoJump: Self = Self::Jmp(ProgramCounter::NO_JUMP);

    #[inline]
    #[allow(non_snake_case)]
    pub const fn Bool(value: bool) -> Self {
        Self::Const(ConstValue::Bool(value))
    }

    #[inline]
    #[allow(non_snake_case)]
    pub const fn Float(value: f64) -> Self {
        Self::Const(ConstValue::Float(value))
    }

    #[inline]
    #[allow(non_snake_case)]
    pub const fn Int(value: i64) -> Self {
        Self::Const(ConstValue::Int(value))
    }

    #[inline]
    #[allow(non_snake_case)]
    pub const fn Str(value: Box<[u8]>) -> Self {
        Self::Const(ConstValue::Str(value))
    }

    #[inline]
    pub fn as_const_value(&self) -> Option<&ConstValue> {
        match self {
            Self::Const(c) => Some(c),
            _ => None,
        }
    }

    #[inline]
    pub const fn is_const(&self) -> bool {
        matches!(self, Self::Const(_))
    }

    #[inline]
    pub const fn is_const_truthy(&self) -> bool {
        matches!(self, Self::Const(c) if c.is_truthy())
    }

    #[inline]
    pub const fn is_const_falsy(&self) -> bool {
        matches!(self, Self::Const(c) if c.is_falsy())
    }

    #[inline]
    pub fn const_eval_unary_op(&self, op: UnaryOp) -> Option<ConstValue> {
        let c = self.as_const_value()?;
        c.eval_unary_op(op)
    }

    #[inline]
    pub fn const_eval_infix_op(
        op: InfixOp,
        lhs: &ExprValue,
        rhs: &ExprValue,
    ) -> Option<ConstValue> {
        let lhs = lhs.as_const_value()?;
        let rhs = rhs.as_const_value()?;
        ConstValue::eval_infix_op(op, lhs, rhs)
    }
}

impl DischargedRegOrJmp {
    pub fn unwrap_reg(self) -> u8 {
        match self {
            DischargedRegOrJmp::Reg(r) => r,
            DischargedRegOrJmp::Jmp(_) => panic!("Expected Reg, found Jmp"),
        }
    }
}

impl ConstValue {
    #[allow(non_upper_case_globals)]
    pub const True: Self = Self::Bool(true);
    #[allow(non_upper_case_globals)]
    pub const False: Self = Self::Bool(false);

    #[inline]
    pub const fn as_expr_value(self) -> ExprValue {
        ExprValue::Const(self)
    }

    pub fn eval_unary_op(&self, op: UnaryOp) -> Option<ConstValue> {
        match op {
            UnaryOp::Not => Some(ConstValue::Bool(self.is_falsy())),
            UnaryOp::BitNot => Some(ConstValue::Int(!self.as_i64())),
            UnaryOp::Minus => Some(-self),
            UnaryOp::Len => Some(ConstValue::Int(self.len())),
        }
    }

    pub fn eval_infix_op(op: InfixOp, lhs: &ConstValue, rhs: &ConstValue) -> Option<ConstValue> {
        match op {
            InfixOp::Concat => None,
            InfixOp::Add => Some(lhs + rhs),
            InfixOp::Sub => Some(lhs - rhs),
            InfixOp::Mul => Some(lhs * rhs),
            InfixOp::Div => Some(ConstValue::Float(lhs.fdiv(rhs))),
            InfixOp::FloorDiv => Some(ConstValue::Int(lhs.idiv(rhs))),
            InfixOp::Mod => Some(lhs % rhs),
            InfixOp::Pow => Some(lhs.pow(rhs)),
            InfixOp::BitAnd => Some(lhs & rhs),
            InfixOp::BitOr => Some(lhs | rhs),
            InfixOp::BitXor => Some(lhs ^ rhs),
            InfixOp::ShiftL => Some(lhs << rhs),
            InfixOp::ShiftR => Some(lhs >> rhs),
            InfixOp::And => Some(ConstValue::Bool(lhs.is_truthy() && rhs.is_truthy())),
            InfixOp::Or => Some(ConstValue::Bool(lhs.is_truthy() || rhs.is_truthy())),
            InfixOp::Eq => Some(ConstValue::Bool(lhs == rhs)),
            InfixOp::NotEq => Some(ConstValue::Bool(lhs != rhs)),
            InfixOp::Less => Some(ConstValue::Bool(lhs < rhs)),
            InfixOp::Greater => Some(ConstValue::Bool(lhs > rhs)),
            InfixOp::LessEq => Some(ConstValue::Bool(lhs <= rhs)),
            InfixOp::GreaterEq => Some(ConstValue::Bool(lhs >= rhs)),
        }
    }
}

impl From<ConstValue> for ExprValue {
    #[inline]
    fn from(value: ConstValue) -> Self {
        value.as_expr_value()
    }
}

impl From<ExprValue> for Expr {
    #[inline]
    fn from(value: ExprValue) -> Self {
        Self::new(value)
    }
}
