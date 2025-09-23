// use std::ops::Range; // entfernt, da ungenutzt

use crate::{ast::*, codegen_state::{BytecodeGenerator, CodeGenerationError, Constant, ExprValue, Frame, ProgramCounter}, opcode::Op, parser::ParseVisitor};

enum U16OrReg{
    U16(u16),
    Slot(u8),
}

impl BytecodeGenerator {

    /// Fix an expression to return one result.
    /// If expression is not a multi-ret expression (function call or vararg), it already returns one result, so nothing needs to be done.
    /// Function calls become `NonReloc` expressions (as its result comes fixed in the base register of the call).
    /// vararg expressions become `Reloc`` as the opcode allows to puts its results where it wants.
    pub fn set_one_ret(&mut self, value: ExprValue) -> ExprValue {
        match value {
            ExprValue::Call(pc) => {
                let base = self[pc].set_base_num(2).expect("invalid op");
                ExprValue::NonReloc(base)
            },
            ExprValue::VarArg(pc) => {
                self[pc].set_base_num(2).expect("invalid op");
                ExprValue::Reloc(pc)
            },
            _ => value,
        }
    }

    /// Converts an expression value into a relocatable or non-relocatable form.
    /// This is analogous to Lua's `luaK_dischargevars`, ensuring that constants,
    /// locals, upvalues, and indexed accesses are properly loaded or moved into registers.
    /// Jumps and calls are handled as pending relocations.
    pub fn discharge_vars(&mut self, value: ExprValue) -> ExprValue {
        match value {
            ExprValue::Const(k) => {
                // Just forward, actual loading happens in discharge_to_reg
                ExprValue::Const(k)
            },
            ExprValue::Local(r) => ExprValue::NonReloc(r),
            ExprValue::Upval(uv) => {
                let pc = self.emit(OP![UGet(0, uv)]);
                ExprValue::Reloc(pc)
            }
            ExprValue::Idx { table_slot, key_slot } => {
                let pc = self.emit(OP![TGetV(0, table_slot, key_slot)]);
                ExprValue::Reloc(pc)
            }
            ExprValue::IdxI { table_slot, key_value } => {
                let pc = self.emit(OP![TGetB(0, table_slot, key_value)]);
                ExprValue::Reloc(pc)
            }
            ExprValue::IdxStr { table_slot, key_const } => {
                let pc = self.emit(OP![TGetS(0, table_slot, key_const)]);
                ExprValue::Reloc(pc)
            }
            ExprValue::Call(_) | ExprValue::VarArg(_) => {
                // make shure only one result is returned
                self.set_one_ret(value)
            }
            ExprValue::Jmp(jump_list) => {
                // Jump lists: keep as is for patching
                ExprValue::Jmp(jump_list)
            }
            _ => value,
        }
    }

    /// Ensures the value is loaded into the given register.
    /// This is analogous to Lua's `discharge2reg`, emitting the correct LOAD/MOVE opcode
    /// depending on the value type. Jumps are not loaded into registers.
    pub fn discharge_to_reg(&mut self, value: ExprValue, dest: u8) -> Result<ExprValue, CodeGenerationError> {
        let value = self.discharge_vars(value);
        match value {
            ExprValue::Nil => {
                self.emit(OP![KPri(dest, 0)]);
            }
            ExprValue::Bool(v) => {
                self.emit(OP![KPri(dest, if v { 2 } else { 1 })]);
            }
            ExprValue::ConstFloat(f) => {
                let k = self.constants.add_float(f)?;
                self.emit(OP![KNum(dest, k)]);
            }
            ExprValue::ConstInt(i) => {
                if i16::MIN as i64 <= i && i <= i16::MAX as i64 {
                    self.emit(OP![KShort(dest, i as i16)]);
                } else {
                    let k = self.constants.add_integer(i)?;
                    self.emit(OP![KNum(dest, k)]);
                }
            }
            ExprValue::ConstString(s) => {
                let k = self.constants.add_string(s)?;
                self.emit(OP![KStr(dest, k)]);
            }
            ExprValue::Const(k) => {
                let pool = &self.constants;
                match pool.get(k) {
                    Some(Constant::Float(_) | Constant::Integer(_)) => {
                        self.emit(OP![KNum(dest, k)]);
                    }
                    Some(crate::codegen_state::Constant::String(_)) => {
                        self.emit(OP![KStr(dest, k)]);
                    }
                    _ => unreachable!(),
                }
            }
            ExprValue::NonReloc(r) if r == dest => {},
            ExprValue::NonReloc(r) => {
                self.emit(OP![Mov(dest, r)]);
            }
            ExprValue::Reloc(pc) => {
                self[pc].set_a_dst(dest).expect("invalid op");
            }
            ExprValue::Jmp(jump_list) => {
                // Jumps cannot be loaded into registers, but we keep the jump list for patching
                return Ok(ExprValue::Jmp(jump_list));
            },
            _ => unreachable!(),
        }
        Ok(ExprValue::NonReloc(dest))
    }

    /// Ensures the value is loaded into any register (allocates a temp if needed).
    /// This is analogous to Lua's `discharge2anyreg`.
    pub fn discharge_to_any_reg(&mut self, value: ExprValue) -> Result<ExprValue, CodeGenerationError> {
        match value {
            ExprValue::NonReloc(_) => Ok(value),
            _ => {
                let dest = self.frame.alloc_temp();
                self.discharge_to_reg(value, dest)
            }
        }
    }

    /// Like `discharge_to_any_reg`, but keeps local variables in their register.
    pub fn discharge_to_any_reg_keep_locals(&mut self, value: ExprValue) -> Result<ExprValue, CodeGenerationError> {
        match value {
            ExprValue::Local(_) => Ok(value),
            _ => self.discharge_to_any_reg(value),
        }
    }

    pub fn go_if_true(&mut self, value: ExprValue) -> ProgramCounter {
        match value {
            ExprValue::Nil => ProgramCounter(0),
            _ => ProgramCounter(0), // Dummy, TODO: Implement logic
        }
    }
}

impl ParseVisitor for BytecodeGenerator {
    type Expr = ExprValue;
    type Error = CodeGenerationError;
    type Proto = Proto;
    
    fn enter_scope(&mut self) {
        todo!()
    }
    
    fn leave_scope(&mut self) {
        todo!()
    }
    
    fn expr_number(&mut self, n: Number) -> Self::Expr {
        match n {
            Number::Integer(i) => Self::Expr::ConstInt(i),
            Number::Float(f) => Self::Expr::ConstFloat(f),
        }
    }
    
    fn expr_string(&mut self, s: Box<[u8]>) -> Self::Expr {
        Self::Expr::ConstString(s)
    }
    
    fn expr_boolean(&mut self, b: bool) -> Self::Expr {
        Self::Expr::Bool(b)
    }
    
    fn expr_nil(&mut self) -> Self::Expr {
        Self::Expr::Nil
    }
    
    fn expr_ellipsis(&mut self) -> Self::Expr {
        todo!()
    }
    
    fn expr_function(&mut self, proto: Self::Proto) -> Self::Expr {
        todo!()
    }
    
    fn expr_unary(&mut self, op: UnaryOp, expr: Self::Expr) -> Self::Expr {
        todo!()
    }
    
    fn expr_infix(&mut self, lhs: Self::Expr, op: InfixOp, rhs: Self::Expr) -> Self::Expr {
        todo!()
    }
    
    fn expr_var(&mut self, name: String) -> Self::Expr {
        todo!()
    }
    
    fn expr_index(&mut self, expr: Self::Expr, index: Self::Expr) -> Self::Expr {
        todo!()
    }
    
    fn expr_field(&mut self, expr: Self::Expr, name: String) -> Self::Expr {
        todo!()
    }
    
    fn expr_call(&mut self, prefix: Self::Expr, method: String, args: Vec<Self::Expr>) -> Self::Expr {
        todo!()
    }
    
    fn expr_table_begin(&mut self) {
        todo!()
    }
    
    fn expr_table_field_index(&mut self, key: Self::Expr, value: Self::Expr) {
        todo!()
    }
    
    fn expr_table_field_named(&mut self, name: String, value: Self::Expr) {
        todo!()
    }
    
    fn expr_table_field_exp(&mut self, expr: Self::Expr) {
        todo!()
    }
    
    fn expr_table_end(&mut self) -> Self::Expr {
        todo!()
    }
    
    fn stmt_label(&mut self, label: String) {
        todo!()
    }
    
    fn stmt_goto(&mut self, label: String) {
        todo!()
    }
    
    fn stmt_if(&mut self, condition: Self::Expr) {
        todo!()
    }
    
    fn stmt_else(&mut self) {
        todo!()
    }
    
    fn stmt_endif(&mut self) {
        todo!()
    }
    
    fn stmt_loop(&mut self) {
        todo!()
    }
    
    fn stmt_loop_while(&mut self, condition: Self::Expr) {
        todo!()
    }
    
    fn stmt_loop_repeat_until(&mut self, condition: Self::Expr) {
        todo!()
    }
    
    fn stmt_loop_for(&mut self, var: String, exprs: Vec<Self::Expr>) {
        todo!()
    }
    
    fn stmt_loop_foreach(&mut self, vars: Vec<String>, exprs: Vec<Self::Expr>) {
        todo!()
    }
    
    fn stmt_endloop(&mut self) {
        todo!()
    }
    
    fn stmt_locals(&mut self, names: Vec<(String,String)>, exprs: Vec<Self::Expr>) {
        todo!()
    }
    
    fn stmt_return(&mut self, exprs: Vec<Self::Expr>) {
        todo!()
    }
    
    fn stmt_function(&mut self, name: FuncName, proto: Self::Proto) {
        todo!()
    }
    
    fn stmt_local_function(&mut self, name: String, proto: Self::Proto) {
        todo!()
    }
    
    fn stmt_assignment(&mut self, vars: Vec<Self::Expr>, exprs: Vec<Self::Expr>) {
        todo!()
    }
    
    fn stmt_expression(&mut self, call: Self::Expr) {
        todo!()
    }

    fn enter_function(&mut self, is_method: bool, is_variadic: bool, args: Vec<String>) {
        todo!()
    }

    fn leave_function(&mut self) -> Self::Proto{
        todo!()
    }
}

/// Erzeugt Bytecode aus dem AST.
pub fn generate_bytecode(block: &Block) -> Vec<Op> {
    let mut code = Vec::new();
    let mut frame = Frame::new();
    compile_block(block, &mut code, &mut frame);
    code
}

fn compile_block(block: &Block, code: &mut Vec<Op>, frame: &mut Frame) {
    for stmt in block {
        compile_stmt(stmt, code, frame);
    }
}

fn compile_stmt(stmt: &Statement, code: &mut Vec<Op>, frame: &mut Frame) {
    match stmt {
        Statement::Assign { vars, exprs } => {
            for (var, expr) in vars.iter().zip(exprs.iter()) {
                let dst_slot = match var {
                    Expression::Var(name) => frame.alloc(name),
                    _ => frame.alloc_temp(),
                };
                compile_expr(expr, code, frame, dst_slot);
                // TODO: set field or index
            }
        }
        Statement::Return(exprs) => {
            // Rückgabe aller Werte als ein RET
            let ret_slots = frame.alloc_temps(exprs.len() as u8);
            let dest_start = ret_slots.start;
            for (expr, dest) in exprs.iter().zip(ret_slots) {
                compile_expr(expr, code, frame, dest);
            }
            if exprs.is_empty() {
                code.push(OP![Ret0]);
            } else if exprs.len() == 1 {
                code.push(OP![Ret1(dest_start)]);
            } else {
                code.push(OP![Ret(dest_start, exprs.len())]);
            }
        }
        Statement::Break => {
            // Beispiel: Break als Sprung
            code.push(OP![Jmp(0, 0)]); // TODO: Ziel berechnen
        }
        Statement::If { ifcases, elsecase } => {
            // Beispiel: Nur erster If-Case
            if let Some((cond, block)) = ifcases.first() {
                let cond_slot = frame.alloc_temp();
                compile_expr(cond, code, frame, cond_slot);
                code.push(OP![IsF(cond_slot)]);
                let jump_pos = code.len();
                code.push(OP![Jmp]); // Placeholder for jump
                for stmt in block {
                    compile_stmt(stmt, code, frame);
                }
                let end_offset = code.len() - jump_pos;
                code[jump_pos] = OP![Jmp(end_offset)];
            }
            // TODO: elsecase und weitere Fälle
        }
        Statement::While { cond, block } => {
            let cond_pos = code.len();
            let cond_slot = frame.alloc_temp();
            compile_expr(cond, code, frame, cond_slot);
            code.push(OP![IsF(cond_slot)]);
            let jump_cond_pos = code.len();
            code.push(OP![Jmp]); // Placeholder for jump

            for stmt in block {
                compile_stmt(stmt, code, frame);
            }

            // loop back to beginning
            let loop_offset = cond_pos as isize - code.len() as isize;
            code.push(OP![Jmp(loop_offset)]);

            // continue here if condition is false
            let jump_cond_offset = code.len() - jump_cond_pos;
            code[jump_cond_pos] = OP![Jmp(jump_cond_offset)];
        }
        Statement::Do(block) => {
            compile_block(block, code, frame);
        }
        // ...weitere Statement-Typen...
        _ => {}
    }
}

fn compile_expr(expr: &Expression, code: &mut Vec<Op>, frame: &mut Frame, dst: u8) -> u8 {
    match expr {
        Expression::Number(n) => {
            // TODO: Konstantenpool für Zahlen
            match n {
                Number::Integer(i) => code.push(OP![KShort(dst, *i)]),
                Number::Float(f) => code.push(OP![KNum(dst, *f)]),
            }
            dst
        }
        Expression::Boolean(b) => {
            let pri = if *b { 2 } else { 1 };
            code.push(OP![KPri(dst, pri)]);
            dst
        }
        Expression::String(_s) => {
            // TODO: Konstantenpool für Strings
            code.push(OP![KStr(dst, dst)]);
            dst
        }
        Expression::Var(name) => {
            let src = frame.get(name).unwrap_or_else(|| frame.alloc(name));
            code.push(OP![Mov(dst, src)]);
            dst
        }
        Expression::Infix(lhs, op, rhs) => {
            let left = compile_expr(lhs, code, frame, dst);
            match op {
                InfixOp::And => {
                    code.push(OP![IsFC(dst, dst)]);
                    let jump_pos = code.len();
                    code.push(OP![Jmp]); // Placeholder
                    let _right = compile_expr(rhs, code, frame, dst);
                    let offset = code.len() - jump_pos;
                    code[jump_pos] = OP![Jmp(0, offset)];
                    dst
                }
                InfixOp::Or => {
                    code.push(OP![IsTC(dst, dst)]);
                    let jump_pos = code.len();
                    code.push(OP![Jmp]); // Placeholder
                    let _right = compile_expr(rhs, code, frame, dst);
                    let offset = code.len() - jump_pos;
                    code[jump_pos] = OP![Jmp(0, offset)];
                    dst
                }
                InfixOp::Add => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![AddVV(dst, left, r)]);
                    dst
                }
                InfixOp::Sub => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![SubVV(dst, left, r)]);
                    dst
                }
                InfixOp::Mul => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![MulVV(dst, left, r)]);
                    dst
                }
                InfixOp::Div => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![DivVV(dst, left, r)]);
                    dst
                }
                InfixOp::Mod => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![ModVV(dst, left, r)]);
                    dst
                }
                InfixOp::Pow => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![Pow(dst, left, r)]);
                    dst
                }
                InfixOp::Eq => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![IsEqV(left, r)]);
                    dst
                }
                InfixOp::NotEq => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![IsNeV(left, r)]);
                    dst
                }
                InfixOp::Less => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![IsLt(left, r)]);
                    dst
                }
                InfixOp::Greater => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![IsGt(left, r)]);
                    dst
                }
                InfixOp::LessEq => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![IsLe(left, r)]);
                    dst
                }
                InfixOp::GreaterEq => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![IsGe(left, r)]);
                    dst
                }
                InfixOp::Concat => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![Cat(dst, left, r)]);
                    dst
                }
                InfixOp::FloorDiv => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![IDivVV(dst, left, r)]);
                    dst
                }
                InfixOp::BitAnd => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![BAndVV(dst, left, r)]);
                    dst
                }
                InfixOp::BitOr => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![BOrVV(dst, left, r)]);
                    dst
                }
                InfixOp::BitXor => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![BXorVV(dst, left, r)]);
                    dst
                }
                InfixOp::ShiftL => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![ShLVV(dst, left, r)]);
                    dst
                }
                InfixOp::ShiftR => {
                    let right = frame.alloc_temp();
                    let r = compile_expr(rhs, code, frame, right);
                    code.push(OP![ShRVV(dst, left, r)]);
                    dst
                }
            }
        }
        Expression::Unary(op, expr) => {
            let val = compile_expr(expr, code, frame, dst);
            match op {
                UnaryOp::Not => code.push(OP![Not(dst, val)]),
                UnaryOp::Minus => code.push(OP![UNM(dst, val)]),
                UnaryOp::Len => code.push(OP![Len(dst, val)]),
                UnaryOp::BitNot => code.push(OP![BNot(dst, val)]),
            }
            dst
        }
        // ...weitere Expression-Typen...
        _ => dst
    }
}

// Weitere Hilfsfunktionen und Strukturen können hier ergänzt werden.
