// use std::ops::Range; // entfernt, da ungenutzt

use core::panic;

use crate::{
    ast::*, codegen_state::{
        BytecodeGenerator, CodeGenerationError, ConstValue, DischargedRegOrJmp, Expr, ExprValue, Frame, JumpList, ProgramCounter, Proto, ProtoGenerator
    }, opcode::Op, parser::{ParseVisitor, ParseVisitorOutput}
};

impl BytecodeGenerator {
    fn assign_lvalue(&mut self, lvalue: Expr, mut rvalue: Expr) {
        if lvalue.is_void() || rvalue.is_void() || self.dead {
            return;
        }
        match lvalue.value {
            ExprValue::Local(slot) => {
                self.expr_to_reg(&mut rvalue, slot).unwrap();
            }
            ExprValue::Global(key_const) => {
                let val_slot = self.expr_to_any_reg(&mut rvalue).unwrap();
                self.emit(OP![GSet(val_slot, key_const)]);
                self.expr_free(&rvalue.value);
            }
            ExprValue::Idx { table_slot, key_slot } => {
                let val_slot = self.expr_to_any_reg(&mut rvalue).unwrap();
                self.emit(OP![TSetV(val_slot, table_slot, key_slot)]);
                self.expr_free(&rvalue.value);
                // TODO: how to free table_slot and key_slot?
            },
            ExprValue::IdxI { table_slot, key_value } => {
                let val_slot = self.expr_to_any_reg(&mut rvalue).unwrap();
                self.emit(OP![TSetB(val_slot, table_slot, key_value)]);
                self.expr_free(&rvalue.value);
                // TODO: how to free table_slot
            }
            ExprValue::IdxStr { table_slot, key_const } => {
                let val_slot = self.expr_to_any_reg(&mut rvalue).unwrap();
                self.emit(OP![TSetS(val_slot, table_slot, key_const)]);
                self.expr_free(&rvalue.value);
                // TODO: how to free table_slot
            }
            ExprValue::Upval(uv) => {
                let val_slot = self.expr_to_any_reg(&mut rvalue).unwrap();
                self.emit(OP![USetV(uv, val_slot)]);
                self.expr_free(&rvalue.value);
            }
            _ => panic!("Invalid lvalue in assignment"),
        }
    }
}

impl ParseVisitorOutput for BytecodeGenerator {
    type Output = Proto;

    fn start(&mut self) -> Result<(), Self::Error> {
        self.enter_function(false, false, vec![]);
        Ok(())
    }
    fn done(&mut self) -> Result<Self::Output, Self::Error> {
        Ok(self.leave_function())
    }
}

impl ParseVisitor for BytecodeGenerator {
    type Expr = Expr;
    type Error = CodeGenerationError;
    type Proto = Proto;

    fn enter_scope(&mut self) {
    }

    fn leave_scope(&mut self) {
    }

    fn enter_expr(&mut self) {
    }

    fn leave_expr(&mut self) {
    }

    fn expr_number(&mut self, n: Number) -> Self::Expr {
        if self.dead {
            return Expr::Void;
        }
        match n {
            Number::Integer(i) => Expr::new(ExprValue::Int(i)),
            Number::Float(f) => Expr::new(ExprValue::Float(f)),
        }
    }

    fn expr_string(&mut self, s: Box<[u8]>) -> Self::Expr {
        if self.dead {
            return Expr::Void;
        }
        Expr::new(ExprValue::Str(s))
    }

    fn expr_boolean(&mut self, b: bool) -> Self::Expr {
        if self.dead {
            return Expr::Void;
        }
        Expr::new(ExprValue::Bool(b))
    }

    fn expr_nil(&mut self) -> Self::Expr {
        if self.dead {
            return Expr::Void;
        }
        Expr::new(ExprValue::Nil)
    }

    fn expr_ellipsis(&mut self) -> Self::Expr {
        if self.dead {
            return Expr::Void;
        }
        // Vararg expression - emit an instruction to get varargs
        let pc = self.emit(OP![VArg(0, 0, 0)]); // Will be fixed later
        Expr::new(ExprValue::VarArg(pc))
    }

    fn expr_function(&mut self, proto: Self::Proto) -> Self::Expr {
        if self.dead {
            return Expr::Void;
        }
        // For now, just return a placeholder - full function compilation would be complex
        // In a real implementation, we'd need to compile the proto into a codegen_state::Proto
        let _ = proto; // Silence warning
        let pc = self.emit(OP![FNew(0, 0)]); // Placeholder function index
        Expr::Reloc(pc)
    }

    fn expr_prefix(&mut self, op: UnaryOp, mut expr: Self::Expr) -> Self::Expr {
        if expr.is_void() || self.dead {
            return Expr::Void;
        }
        if let Some(c) = expr.value.const_eval_unary_op(op) {
            debug_assert!(!expr.has_jumps());
            return Expr::new(c.as_expr_value());
        }
        let instr = match op {
            UnaryOp::Not => {
                // swap True/False jump lists of Expr
                std::mem::swap(&mut expr.jump_false, &mut expr.jump_true);
                // TODO: remove values from jump-list conditions
                match self.discharge_to_any_reg_mut(&mut expr.value).unwrap() {
                    DischargedRegOrJmp::Reg(src_reg) => {
                        self.expr_free(&expr.value);
                        // Emit NOT instruction
                        OP![Not(0, src_reg)]
                    }
                    DischargedRegOrJmp::Jmp(pc) => {
                        // negate the jump condition
                        if self.negate_jmp_ctrl(pc) {
                            return expr;
                        } else {
                            panic!("Failed to negate non-conditional jump condition");
                        }
                    }
                }
            },
            UnaryOp::BitNot => {
                let src_reg = self.expr_to_any_reg(&mut expr).unwrap();
                self.expr_free(&expr.value);
                OP![BNot(0, src_reg)]
            },
            UnaryOp::Minus => {
                let src_reg = self.expr_to_any_reg(&mut expr).unwrap();
                self.expr_free(&expr.value);
                OP![UNM(0, src_reg)]
            },
            UnaryOp::Len => {
                let src_reg = self.expr_to_any_reg(&mut expr).unwrap();
                self.expr_free(&expr.value);
                OP![Len(0, src_reg)]
            },
        };
        debug_assert!(!expr.has_jumps()); // TODO: is this correct here?
        let pc = self.emit(instr);
        expr.value = ExprValue::Reloc(pc);
        expr
    }

    fn expr_infix(&mut self, mut lhs: Self::Expr, op: InfixOp) -> Self::Expr {
        if lhs.is_void() || self.dead {
            return Expr::Void;
        }
        match op {
            InfixOp::And => {
                if let ExprValue::Const(c) = &lhs.value {
                    if c.is_falsy() {
                        self.dead = true;
                    }
                    return lhs; // evaluate rhs
                }
                self.go_if_true(&mut lhs);
                lhs
            }
            InfixOp::Or => {
                if let ExprValue::Const(c) = &lhs.value {
                    if c.is_truthy() {
                        self.dead = true;
                    }
                    return lhs;
                }
                self.go_if_false(&mut lhs);
                lhs
            }
            InfixOp::Concat => {
                // ensure operand is on stack
                self.expr_to_next_reg(&mut lhs).unwrap();
                lhs
            },
            _ => lhs,
        }
    }

    fn expr_postfix(&mut self, mut lhs: Self::Expr, op: InfixOp, mut rhs: Self::Expr) -> Self::Expr {
        // Handle short-circuiting constant logical operators first
        if lhs.is_void() {
            return rhs;
        }
        if rhs.is_void() {
            debug_assert!(self.dead);
            self.dead = false;
            return lhs;
        }
        if self.dead {
            return Expr::Void;
        }

        // Constant folding for other infix operations
        if let Some(r) = ExprValue::const_eval_infix_op(op, &lhs.value, &rhs.value) {
            debug_assert!(!lhs.has_jumps() && !rhs.has_jumps()); // TODO: is this correct here?
            return Expr::new(r.as_expr_value());
        }

        // Handle short-circuiting non-const logical operators first
        match op {
            InfixOp::And => {
                debug_assert!(!lhs.jump_true.has_jumps());
                self.concat_jump_list(&mut rhs.jump_false, lhs.jump_false);
                return rhs;
            },
            InfixOp::Or => {
                debug_assert!(!lhs.jump_false.has_jumps());
                self.concat_jump_list(&mut rhs.jump_true, lhs.jump_true);
                return rhs;
            },
            _ => {},
        }


        // Special handling for CONCAT
        if op == InfixOp::Concat {
            // Ensure lhs is in the next register
            let ExprValue::NonReloc(lhs_reg) = lhs.value else {
                panic!("Expected lhs to be in next register");
            };
            // Ensure rhs is in the next register
            let rhs_reg = self.expr_to_next_reg(&mut rhs).unwrap();
            self.expr_free2(&lhs.value, &rhs.value);
            if let Some(Op::Cat(args)) = self.bytecode.last_mut() {
                // Extend existing CONCAT instruction
                debug_assert_eq!(lhs_reg + 1, args.b);
                args.b = lhs_reg;
                return rhs;
            } else {
                let pc = self.emit(OP![Cat(0, lhs_reg, rhs_reg)]); // concat two values
                return Expr::Reloc(pc);
            }
        }

        // TODO: handle constant arguments with specialized opcodes
        // This may require to swap operands for commutative operations

        let lhs_reg = self.expr_to_any_reg(&mut lhs).unwrap();
        let rhs_reg = self.expr_to_any_reg(&mut rhs).unwrap();

        self.expr_free2(&lhs.value, &rhs.value);

        match op {
            // Comparison operators generate conditional jumps
            InfixOp::Less => self.emit(OP![IsLt(lhs_reg, rhs_reg)]),
            InfixOp::Greater => self.emit(OP![IsGt(lhs_reg, rhs_reg)]),
            InfixOp::LessEq => self.emit(OP![IsLe(lhs_reg, rhs_reg)]),
            InfixOp::GreaterEq => self.emit(OP![IsGe(lhs_reg, rhs_reg)]),
            // TODO: use specialized opcodes for constants when possible
            InfixOp::Eq => self.emit(OP![IsEqV(lhs_reg, rhs_reg)]),
            InfixOp::NotEq => self.emit(OP![IsNeV(lhs_reg, rhs_reg)]),

            _ => {
                let pc = match op {
                    // TODO: use specialized opcodes for constants when possible
                    InfixOp::Add => self.emit(OP![AddVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::Sub => self.emit(OP![SubVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::Mul => self.emit(OP![MulVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::Div => self.emit(OP![DivVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::FloorDiv => self.emit(OP![IDivVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::Mod => self.emit(OP![ModVV(0, lhs_reg, rhs_reg)]),

                    InfixOp::Pow => self.emit(OP![Pow(0, lhs_reg, rhs_reg)]),
                    InfixOp::BitAnd => self.emit(OP![BAndVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::BitOr => self.emit(OP![BOrVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::BitXor => self.emit(OP![BXorVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::ShiftL => self.emit(OP![ShLVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::ShiftR => self.emit(OP![ShRVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::Concat => self.emit(OP![Cat(0, lhs_reg, rhs_reg)]),

                    _ => unreachable!(),
                };
                return Expr::Reloc(pc);
            }
        };
        let cmp_pc = self.emit_jmp_placeholder();
        Expr::new(ExprValue::Jmp(cmp_pc))
    }

    fn expr_var(&mut self, name: String) -> Self::Expr {
        if self.dead {
            return Expr::Void;
        }
        if let Some(slot) = self.frame.get(&name) {
            // it's a local variable
            Expr::new(ExprValue::Local(slot))
        } else if let Some(uv_idx) = self.upvalues.iter().position(|uv| uv == &name) {
            // it's an upvalue
            Expr::new(ExprValue::Upval(uv_idx as u8))
        } else {
            // It's a global variable
            let key_const = self
                .constants
                .add_string(name.into_bytes().into_boxed_slice())
                .unwrap();
            Expr::new(ExprValue::Global(key_const))
        }
    }

    fn expr_index(&mut self, mut expr: Self::Expr, mut index: Self::Expr) -> Self::Expr {
        if expr.is_void() || index.is_void() || self.dead {
            return Expr::Void;
        }
        let table_slot = self.expr_to_any_reg(&mut expr).unwrap();
        // TODO: how to free table_slot?

        // Check if index is a constant that can be optimized
        match &index.value {
            ExprValue::Const(ConstValue::Int(i)) if *i >= 0 && *i <= 255 => Expr::new(ExprValue::IdxI {
                table_slot,
                key_value: *i as u8,
            }),
            ExprValue::Const(ConstValue::Str(_)) => {
                let ExprValue::Const(ConstValue::Str(s)) = index.take() else {
                    unreachable!()
                };
                let key_const = self.constants.add_string(s).unwrap();
                if key_const <= 255 {
                    Expr::new(ExprValue::IdxStr {
                        table_slot,
                        key_const: key_const as u8,
                    })
                } else {
                    // Too many constants, fall back to normal indexing
                    let key_slot = self.frame.alloc_temp();
                    self.emit(OP![KStr(key_slot, key_const)]);
                    // TODO: how to free key_slot?
                    Expr::new(ExprValue::Idx {
                        table_slot,
                        key_slot,
                    })
                }
            }
            _ => {
                let key_slot = self.expr_to_any_reg(&mut index).unwrap();
                // TODO: how to free key_slot?
                Expr::new(ExprValue::Idx {
                    table_slot,
                    key_slot,
                })
            }
        }
    }

    fn expr_field(&mut self, mut expr: Self::Expr, name: String) -> Self::Expr {
        if expr.is_void() || self.dead {
            return Expr::Void;
        }
        let table_slot = self.expr_to_any_reg(&mut expr).unwrap();
        // TODO: how to free table_slot?

        // Field access is always string indexing
        let key_const = self
            .constants
            .add_string(name.into_bytes().into_boxed_slice())
            .unwrap();
        if key_const <= 255 {
            Expr::new(ExprValue::IdxStr {
                table_slot,
                key_const: key_const as u8,
            })
        } else {
            // Too many constants, fall back to normal indexing
            let key_slot = self.frame.alloc_temp();
            self.emit(OP![KStr(key_slot, key_const)]);
            // TODO: how to free key_slot?
            Expr::new(ExprValue::Idx {
                table_slot,
                key_slot,
            })
        }
    }

    fn expr_self(&mut self, mut prefix: Self::Expr, method: String) -> Self::Expr {
        if prefix.is_void() || self.dead {
            return Expr::Void;
        }
        // Method call: obj:method(...)
        let table_slot = self.expr_to_any_reg(&mut prefix).unwrap();
        // TODO: how to free table_slot?
        let key_const = self
            .constants
            .add_string(method.into_bytes().into_boxed_slice())
            .unwrap();
        if key_const <= 255 {
            Expr::new(ExprValue::IdxStr {
                table_slot,
                key_const: key_const as u8,
            })
        } else {
            // Too many constants, fall back to normal indexing
            let key_slot = self.frame.alloc_temp();
            self.emit(OP![KStr(key_slot, key_const)]);
            // TODO: how to free key_slot?
            Expr::new(ExprValue::Idx {
                table_slot,
                key_slot,
            })
        }
    }

    fn expr_call(
        &mut self,
        mut prefix: Self::Expr,
        mut args: Vec<Self::Expr>,
        _is_method: bool,
    ) -> Self::Expr {
        if prefix.is_void() || self.dead {
            return Expr::Void;
        }
        // Get the function to call
        self.discharge_vars_mut(&mut prefix.value);

        // Allocate slots for arguments
        let num_args = args.len() as u8;
        let regs = self.frame.alloc_temps(num_args + 1); // +1 for function itself
        let func_slot = regs.start;
        let first_arg = func_slot + 1;

        self.expr_to_reg(&mut prefix, func_slot).unwrap();

        // Load arguments into consecutive slots
        for (i, arg) in args.iter_mut().enumerate() {
            let arg_slot = first_arg + i as u8;
            self.expr_to_reg(arg, arg_slot).unwrap();
        }

        // Emit call instruction
        let pc = self.emit(OP![Call(func_slot, num_args + 1, 0)]); // +1 for function itself

        // TODO: free

        Expr::new(ExprValue::Call(pc))
    }

    fn expr_table_begin(&mut self) {
        if self.dead {
            return;
        }
        // TODO: table creation needs refactoring

        // Create a new table and allocate a slot for it
        let table_slot = self.frame.alloc_temp();
        self.emit(OP![TNew(table_slot, 0)]); // Empty table for now
                                             // We would need to track this table slot somehow, for now simplified
    }

    fn expr_table_field_index(&mut self, _key: Self::Expr, _value: Self::Expr) {
        if self.dead {
            return;
        }
        // For proper implementation, we'd need to track the current table being built
        // For now, just a placeholder
    }

    fn expr_table_field_named(&mut self, _name: String, _value: Self::Expr) {
        if self.dead {
            return;
        }
        // For proper implementation, we'd need to track the current table being built
        // For now, just a placeholder
    }

    fn expr_table_field_exp(&mut self, _expr: Self::Expr) {
        if self.dead {
            return;
        }
        // For proper implementation, we'd need to track the current table being built
        // For now, just a placeholder
    }

    fn expr_table_end(&mut self) -> Self::Expr {
        if self.dead {
            return Expr::Void;
        }
        // Return a placeholder table
        // This is simplified - a proper implementation would track the table construction
        let pc = self.emit(OP![TNew(0, 0)]); // Empty table
        Expr::Reloc(pc)
    }

    fn stmt_label(&mut self, label: String) {
        // TODO: dead?
        self.dead = false;
        todo!()
    }

    fn stmt_goto(&mut self, label: String) {
        if self.dead {
            return;
        }
        todo!()
    }

    fn stmt_if(&mut self, mut condition: Self::Expr) {
        if self.dead {
            return;
        }
        // TODO: handle optimized case: `if x then break`
        let else_jump = self.go_if_true(&mut condition);
        self.ifjumps.push(else_jump);
    }

    fn stmt_else(&mut self) {
        if self.dead {
            return;
        }
        let jump_end = JumpList(self.emit_jmp_placeholder());
        let if_expr_jumps = self.ifjumps.pop().expect("Not inside if");
        self.ifjumps.push(jump_end);
        self.patch_jmp_list_here(if_expr_jumps);
    }

    fn stmt_endif(&mut self) {
        if self.dead {
            return;
        }
        let jump_end = self.ifjumps.pop().expect("not inside if");
        self.patch_jmp_list_here(jump_end);
    }

    fn stmt_loop(&mut self) {
        if self.dead {
            return;
        }
        let pc = self.pc();
        self.loops.push((pc, JumpList::NO_JUMP));
    }

    fn stmt_loop_while(&mut self, mut condition: Self::Expr) {
        if self.dead {
            return;
        }
        let end_jump = self.go_if_true(&mut condition);
        let (_loop_start, mut end_jumps) = *self.loops.last().expect("not inside loop");
        self.concat_jump_list(&mut end_jumps, end_jump);
        self.loops.last_mut().unwrap().1 = end_jumps;
    }

    fn stmt_loop_repeat_until(&mut self, mut condition: Self::Expr) {
        if self.dead {
            return;
        }
        let (loop_start, end_jumps) = *self.loops.last().expect("not inside loop");
        debug_assert!(!end_jumps.has_jumps());
        let continue_jump = self.go_if_false(&mut condition);
        self.patch_jmp_list(continue_jump, loop_start);
    }

    fn stmt_loop_for(&mut self, var: String, exprs: Vec<Self::Expr>) {
        if self.dead {
            return;
        }
        todo!()
    }

    fn stmt_loop_foreach(&mut self, vars: Vec<String>, exprs: Vec<Self::Expr>) {
        if self.dead {
            return;
        }
        todo!()
    }

    fn stmt_endloop(&mut self) {
        if self.dead {
            return;
        }
        let (_loop_start, end_jumps) = self.loops.pop().expect("not inside loop");
        self.patch_jmp_list_here(end_jumps);
    }

    fn stmt_do(&mut self) {
        self.enter_scope();
    }

    fn stmt_enddo(&mut self) {
        self.leave_scope();
    }

    fn stmt_locals(&mut self, names: Vec<(String, String)>, exprs: Vec<Self::Expr>) {
        if self.dead {
            return;
        }
        let mut vars = Vec::with_capacity(names.len());
        for (name, _attrib) in names {
            // TODO: attributes
            vars.push(Expr::new(ExprValue::Local(self.frame.alloc(name))));
        }
        self.stmt_assignment(vars, exprs);
    }

    fn stmt_return(&mut self, mut exprs: Vec<Self::Expr>) {
        if self.dead {
            return;
        }
        if exprs.is_empty() {
            self.emit(OP![Ret0]);
        } else if exprs.len() == 1 {
            let mut expr = exprs.pop().unwrap();
            let reg = self.expr_to_any_reg(&mut expr).unwrap();
            self.emit(OP![Ret1(reg)]);
            self.frame.free(reg);
        } else {
            let num_rets = exprs.len() as u8;
            let regs = self.frame.alloc_temps(num_rets);
            let first_reg = regs.start;
            for (i, expr) in exprs.iter_mut().enumerate() {
                let reg = first_reg + i as u8;
                self.expr_to_reg(expr, reg).unwrap();
            }
            self.emit(OP![Ret(first_reg, num_rets)]);
            self.frame.free_range(regs);
        }
        self.dead = true; // TODO: reset dead back to false after block
    }

    fn stmt_function(&mut self, mut name: FuncName, proto: Self::Proto) {
        if self.dead {
            return;
        }
        let mut drain = name.qname.drain(..);
        let mut var = self.expr_var(drain.next().unwrap());
        for part in drain {
            var = self.expr_field(var, part);
        }
        let func = self.expr_function(proto);
        self.stmt_assignment(vec![var], vec![func]);
    }

    fn stmt_local_function(&mut self, name: String, proto: Self::Proto) {
        if self.dead {
            return;
        }
        let var = self.frame.alloc(name);
        let mut func = self.expr_function(proto);
        self.expr_to_reg(&mut func, var).unwrap();
    }

    fn stmt_assignment(&mut self, vars: Vec<Self::Expr>, exprs: Vec<Self::Expr>) {
        if self.dead {
            return;
        }
        if vars.len() == exprs.len() {
            for (var, expr) in vars.into_iter().zip(exprs.into_iter()) {
                self.assign_lvalue(var, expr);
            }
        } else if exprs.len() == 1 {
            // TODO: unpack variadic
            todo!()
        } else if !exprs.is_empty() {
            panic!("Mismatched number of local variables and expressions");
        }
    }

    fn stmt_expression(&mut self, mut call: Self::Expr) {
        if self.dead {
            return;
        }
        self.expr_to_any_reg(&mut call).unwrap();
        self.expr_free(&call.value);
    }

    fn enter_function(&mut self, is_method: bool, is_vararg: bool, args: Vec<String>) {
        self.protos.push(ProtoGenerator::new(is_vararg, args));
    }

    fn leave_function(&mut self) -> Self::Proto {
        self.protos.pop().expect("not in a function").into_proto()
    }
}
