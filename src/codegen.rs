// use std::ops::Range; // entfernt, da ungenutzt

use core::panic;

use crate::{
    ast::*,
    codegen_state::{
        BytecodeGenerator, CodeGenerationError, ConstValue, DischargedRegOrJmp, Expr, ExprValue,
        JumpList, ProgramCounter, Proto, ProtoGenerator,
    },
    parser::{ParseVisitor, ParseVisitorOutput},
};

impl ProtoGenerator {
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
                self.emit(op![GSet(val_slot, key_const)]);
                self.expr_free(&rvalue.value);
            }
            ExprValue::Idx {
                table_slot,
                key_slot,
            } => {
                let val_slot = self.expr_to_any_reg(&mut rvalue).unwrap();
                self.emit(op![TSetV(val_slot, table_slot, key_slot)]);
                self.expr_free(&rvalue.value);
                self.frame.free2(table_slot, key_slot);
            }
            ExprValue::IdxI {
                table_slot,
                key_value,
            } => {
                let val_slot = self.expr_to_any_reg(&mut rvalue).unwrap();
                self.emit(op![TSetB(val_slot, table_slot, key_value)]);
                self.expr_free(&rvalue.value);
                self.frame.free(table_slot);
            }
            ExprValue::IdxStr {
                table_slot,
                key_const,
            } => {
                let val_slot = self.expr_to_any_reg(&mut rvalue).unwrap();
                self.emit(op![TSetS(val_slot, table_slot, key_const)]);
                self.expr_free(&rvalue.value);
                self.frame.free(table_slot);
            }
            ExprValue::Upval(uv) => {
                let val_slot = self.expr_to_any_reg(&mut rvalue).unwrap();
                self.emit(op![USetV(uv, val_slot)]);
                self.expr_free(&rvalue.value);
            }
            _ => panic!("Invalid lvalue in assignment"),
        }
    }

    fn add_proto(&mut self, proto: Proto) -> u8 {
        let idx = self.protos.len();
        assert!(idx < 256, "Too many nested functions");
        self.protos.push(proto);
        idx as u8
    }
}

pub struct Loop {
    pub was_dead: bool,
    pub loop_start: ProgramCounter,
    pub end_jumps: JumpList,
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
    type ExprList = (Expr, u8);
    type ExprCall = (Expr, u8);
    type ExprTable = (Expr, u8);
    type Loop = Loop;
    type Error = CodeGenerationError;
    type Proto = Proto;

    fn enter_scope(&mut self) {
        self.frame.enter_scope();
    }

    fn leave_scope(&mut self) {
        self.frame.leave_scope();
    }

    fn enter_expr(&mut self) {}

    fn leave_expr(&mut self) {}

    fn expr_list_begin(&mut self) -> Self::ExprList {
        (Expr::Void, 0)
    }

    fn expr_list_item(&mut self, (base, nargs): &mut Self::ExprList, mut expr: Self::Expr) {
        let reg: u8 = self.expr_to_next_reg(&mut expr).unwrap();
        if *nargs == 0 {
            *base = expr;
            *nargs = 1;
        } else {
            let ExprValue::NonReloc(base_reg) = base.value else {
                panic!("Expected base to be in next register");
            };
            debug_assert_eq!(base_reg + *nargs, reg);
            *nargs += 1;
        }
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
        let pc = self.emit(op![VArg(0, 0, 0)]); // Will be fixed later
        Expr::new(ExprValue::VarArg(pc))
    }

    fn expr_function(&mut self, proto: Self::Proto) -> Self::Expr {
        if self.dead {
            return Expr::Void;
        }
        let proto = self.add_proto(proto);
        let pc = self.emit(op![FNew(0, proto as u16)]);
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
                        op![Not(0, src_reg)]
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
            }
            UnaryOp::BitNot => {
                let src_reg = self.expr_to_any_reg(&mut expr).unwrap();
                self.expr_free(&expr.value);
                op![BNot(0, src_reg)]
            }
            UnaryOp::Minus => {
                let src_reg = self.expr_to_any_reg(&mut expr).unwrap();
                self.expr_free(&expr.value);
                op![UNM(0, src_reg)]
            }
            UnaryOp::Len => {
                let src_reg = self.expr_to_any_reg(&mut expr).unwrap();
                self.expr_free(&expr.value);
                op![Len(0, src_reg)]
            }
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
            }
            _ => lhs,
        }
    }

    fn expr_postfix(
        &mut self,
        mut lhs: Self::Expr,
        op: InfixOp,
        mut rhs: Self::Expr,
    ) -> Self::Expr {
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
            }
            InfixOp::Or => {
                debug_assert!(!lhs.jump_false.has_jumps());
                self.concat_jump_list(&mut rhs.jump_true, lhs.jump_true);
                return rhs;
            }
            _ => {}
        }

        // Special handling for CONCAT
        if op == InfixOp::Concat {
            // Ensure lhs is in the next register
            let ExprValue::NonReloc(lhs_reg) = lhs.value else {
                panic!("Expected lhs to be in next register");
            };
            self.discharge_vars_mut(&mut rhs.value);
            // Ensure rhs is in the next register
            if let Some(op) = self.bytecode.last_mut() {
                let r = match_op! {(op) {
                    Cat(_,ref mut b,_) => {
                        debug_assert_eq!(lhs_reg + 1, *b);
                        *b = lhs_reg; // extend existing CONCAT instruction
                        true
                    },
                    _ => false,
                }};
                if r {
                    self.expr_free2(&lhs.value, &rhs.value);
                    return rhs;
                }
            }

            let rhs_reg = self.expr_to_next_reg(&mut rhs).unwrap();
            self.expr_free2(&lhs.value, &rhs.value);

            let pc = self.emit(op![Cat(0, lhs_reg, rhs_reg)]); // concat two values
            return Expr::Reloc(pc);
        }

        // TODO: handle constant arguments with specialized opcodes
        // This may require to swap operands for commutative operations

        let lhs_reg = self.expr_to_any_reg(&mut lhs).unwrap();
        let rhs_reg = self.expr_to_any_reg(&mut rhs).unwrap();

        self.expr_free2(&lhs.value, &rhs.value);

        match op {
            // Comparison operators generate conditional jumps
            InfixOp::Less => self.emit(op![IsLt(lhs_reg, rhs_reg)]),
            InfixOp::Greater => self.emit(op![IsGt(lhs_reg, rhs_reg)]),
            InfixOp::LessEq => self.emit(op![IsLe(lhs_reg, rhs_reg)]),
            InfixOp::GreaterEq => self.emit(op![IsGe(lhs_reg, rhs_reg)]),
            // TODO: use specialized opcodes for constants when possible
            InfixOp::Eq => self.emit(op![IsEqV(lhs_reg, rhs_reg)]),
            InfixOp::NotEq => self.emit(op![IsNeV(lhs_reg, rhs_reg)]),

            _ => {
                let pc = match op {
                    // TODO: use specialized opcodes for constants when possible
                    InfixOp::Add => self.emit(op![AddVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::Sub => self.emit(op![SubVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::Mul => self.emit(op![MulVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::Div => self.emit(op![DivVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::FloorDiv => self.emit(op![IDivVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::Mod => self.emit(op![ModVV(0, lhs_reg, rhs_reg)]),

                    InfixOp::Pow => self.emit(op![Pow(0, lhs_reg, rhs_reg)]),
                    InfixOp::BitAnd => self.emit(op![BAndVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::BitOr => self.emit(op![BOrVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::BitXor => self.emit(op![BXorVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::ShiftL => self.emit(op![ShLVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::ShiftR => self.emit(op![ShRVV(0, lhs_reg, rhs_reg)]),
                    InfixOp::Concat => self.emit(op![Cat(0, lhs_reg, rhs_reg)]),

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
            // TODO: check parent protos for creating upvalues
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
            ExprValue::Const(ConstValue::Int(i)) if *i >= 0 && *i <= 255 => {
                Expr::new(ExprValue::IdxI {
                    table_slot,
                    key_value: *i as u8,
                })
            }
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
                    self.emit(op![KStr(key_slot, key_const)]);
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
            self.emit(op![KStr(key_slot, key_const)]);
            Expr::new(ExprValue::Idx {
                table_slot,
                key_slot,
            })
        }
    }

    fn expr_call_begin(
        &mut self,
        mut prefix: Self::Expr,
        method: Option<String>,
    ) -> Self::ExprCall {
        if let Some(method) = method {
            prefix = self.expr_field(prefix, method)
        }
        self.expr_to_next_reg(&mut prefix).unwrap();
        (prefix, 1)
    }

    fn expr_call_arg(&mut self, list: &mut Self::ExprCall, arg: Self::Expr) {
        self.expr_list_item(list, arg);
    }

    fn expr_call_end(&mut self, (base, nargs): Self::ExprCall) -> Self::Expr {
        let ExprValue::NonReloc(base_reg) = base.value else {
            panic!("Expected base to be in next register");
        };

        // Emit call instruction
        let pc = self.emit(op![Call(base_reg, nargs, 0)]); // +1 for function itself (nargs already includes +1)
        if nargs > 1 {
            self.frame.free_range(base_reg + 1..base_reg + nargs); // KEEP 1 for the result
        }
        Expr::new(ExprValue::Call(pc))
    }

    fn expr_table_begin(&mut self) -> Self::ExprTable {
        if self.dead {
            return (Expr::Void, 0);
        }
        // Create a new table and allocate a slot for it
        let table_slot = self.frame.alloc_temp();
        self.emit(op![TNew(table_slot, 0)]); // Empty table for now

        (Expr::new(ExprValue::NonReloc(table_slot)), 0)
    }

    fn expr_table_field_index(
        &mut self,
        (table, _): &mut Self::ExprTable,
        mut key: Self::Expr,
        mut expr: Self::Expr,
    ) {
        if self.dead {
            return;
        }
        let ExprValue::NonReloc(table_slot) = table.value else {
            panic!("Expected table to be in next register");
        };
        let expr_reg = self.expr_to_any_reg(&mut expr).unwrap();
        let key_reg = self.expr_to_any_reg(&mut key).unwrap();
        self.emit(op![TSetV(expr_reg, table_slot, key_reg)]);
        self.expr_free(&expr.value);
        self.expr_free(&key.value);
    }

    fn expr_table_field_named(
        &mut self,
        (table, _): &mut Self::ExprTable,
        name: String,
        mut expr: Self::Expr,
    ) {
        if self.dead {
            return;
        }
        let ExprValue::NonReloc(table_slot) = table.value else {
            panic!("Expected table to be in next register");
        };
        let expr_reg = self.expr_to_any_reg(&mut expr).unwrap();
        let k = self
            .constants
            .add_string(name.into_bytes().into_boxed_slice())
            .unwrap();
        if k <= 255 {
            self.emit(op![TSetS(expr_reg, table_slot, k as u8)]);
        } else {
            let key_slot = self.frame.alloc_temp();
            self.emit(op![KStr(key_slot, k)]);
            self.emit(op![TSetV(expr_reg, table_slot, key_slot)]);
            self.frame.free(key_slot);
        }
        self.expr_free(&expr.value);
    }

    fn expr_table_field_exp(&mut self, (table, count): &mut Self::ExprTable, mut expr: Self::Expr) {
        if self.dead {
            return;
        }
        let ExprValue::NonReloc(table_slot) = table.value else {
            panic!("Expected table to be in next register");
        };
        let expr_reg = self.expr_to_any_reg(&mut expr).unwrap();
        *count += 1;
        self.emit(op![TSetB(expr_reg, table_slot, *count)]);
        self.expr_free(&expr.value);
    }

    fn expr_table_end(&mut self, (table, _): Self::ExprTable) -> Self::Expr {
        table
    }

    fn stmt_label(&mut self, label: String) {
        // TODO: dead?
        self.dead = false;
        todo!("Labels not implemented");
    }

    fn stmt_goto(&mut self, label: String) {
        if self.dead {
            return;
        }
        todo!("GoTo not implemented");
    }

    fn stmt_if(&mut self, mut condition: Self::Expr) {
        if self.dead {
            self.ifjumps.push((true, true, JumpList::NO_JUMP));
            return;
        }
        // TODO: handle optimized case: `if x then break`
        let else_jump = self.go_if_true(&mut condition);
        self.ifjumps.push((false, false, else_jump));
    }

    fn stmt_else(&mut self) {
        let (was_dead, _, if_expr_jumps) = self.ifjumps.pop().expect("Not inside if");
        let if_was_dead = self.dead;
        self.dead = was_dead;
        if was_dead {
            self.ifjumps.push((true, true, JumpList::NO_JUMP));
            return;
        }
        let jump_end = JumpList(self.emit_jmp_placeholder());
        self.ifjumps.push((false, if_was_dead, jump_end));
        self.patch_jmp_list_here(if_expr_jumps);
    }

    fn stmt_endif(&mut self) {
        let (was_dead, iforelse_was_dead, if_expr_jumps) =
            self.ifjumps.pop().expect("Not inside if");
        self.dead = was_dead || self.dead && iforelse_was_dead;
        if self.dead {
            return;
        }
        self.patch_jmp_list_here(if_expr_jumps);
    }

    fn stmt_loop_begin(&mut self) -> Self::Loop {
        let pc = self.pc();
        Loop {
            was_dead: self.dead,
            loop_start: pc,
            end_jumps: JumpList::NO_JUMP,
        }
    }

    fn stmt_loop_while(&mut self, current_loop: &mut Self::Loop, mut condition: Self::Expr) {
        if self.dead {
            return;
        }
        let end_jump = self.go_if_true(&mut condition);
        self.concat_jump_list(&mut current_loop.end_jumps, end_jump);
    }

    fn stmt_loop_repeat_until(&mut self, current_loop: &mut Self::Loop, mut condition: Self::Expr) {
        if self.dead {
            return;
        }
        debug_assert!(!current_loop.end_jumps.has_jumps());
        let continue_jump = self.go_if_false(&mut condition);
        self.patch_jmp_list(continue_jump, current_loop.loop_start);
        current_loop.loop_start = ProgramCounter::NO_JUMP;
    }

    fn stmt_loop_for_begin(
        &mut self,
        varname: String,
        (base, nargs): Self::ExprList,
    ) -> Self::Loop {
        if self.dead {
            return Loop {
                was_dead: true,
                loop_start: ProgramCounter::NO_JUMP,
                end_jumps: JumpList::NO_JUMP,
            };
        }
        debug_assert!(nargs >= 2);
        let ExprValue::NonReloc(base_reg) = base.value else {
            panic!("Expected base to be in next register");
        };
        self.frame.free_range(base_reg..base_reg + nargs); // Temporary free

        let varname_idx = format!("(for idx {})", varname);
        let varname_limit = format!("(for limit {})", varname);
        let varname_step = format!("(for limit {})", varname);
        let reg_idx = self.frame.alloc(varname_idx);
        debug_assert_eq!(base_reg, reg_idx);
        let reg_limit = self.frame.alloc(varname_limit);
        debug_assert_eq!(base_reg + 1, reg_limit);
        let reg_step = self.frame.alloc(varname_step);
        debug_assert_eq!(base_reg + 2, reg_limit);
        if nargs < 3 {
            // define step = 1
            self.emit(op![KShort(reg_step, 1)]);
        }

        let reg_var = self.frame.alloc(varname);
        debug_assert_eq!(base_reg + 3, reg_var);

        let loop_start = self.emit(op![ForL(reg_idx, !0)]);
        Loop {
            was_dead: false,
            loop_start,
            end_jumps: JumpList(loop_start),
        }
    }

    fn stmt_loop_foreach_begin(
        &mut self,
        vars: Vec<String>,
        (base, nargs): Self::ExprList,
    ) -> Self::Loop {
        if self.dead {
            return Loop {
                was_dead: true,
                loop_start: ProgramCounter::NO_JUMP,
                end_jumps: JumpList::NO_JUMP,
            };
        }
        debug_assert!(nargs >= 1);
        let ExprValue::NonReloc(base_reg) = base.value else {
            panic!("Expected base to be in next register");
        };
        self.frame.free_range(base_reg..base_reg + nargs); // Temporary free
                                                           // TODO: parse expressions as singular items

        let reg_gen = self.frame.alloc(String::from("(for gen)"));
        debug_assert_eq!(base_reg, reg_gen);
        let reg_state = self.frame.alloc(String::from("(for state)"));
        debug_assert_eq!(base_reg + 1, reg_state);
        let reg_ctrl = self.frame.alloc(String::from("(for ctrl)"));
        debug_assert_eq!(base_reg + 2, reg_ctrl);
        let reg_toclose = self.frame.alloc(String::from("(for toclose)"));
        debug_assert_eq!(base_reg + 3, reg_toclose);

        if nargs < 4 {
            // TODO: instead check, if last expr is CALL or VArg, and propagate results directly
            // define toclose = 0
            self.emit_load_prim(reg_state, 1, 4 - nargs); // 1=nil
        }

        let nvars = vars.len() as u8;
        let vars_begin = self.frame.next();
        for var in vars {
            self.frame.alloc(var);
        }
        let vars_end = self.frame.next();
        debug_assert_eq!(vars_end - vars_begin, nvars);

        let loop_start = self.emit(op![IterC(vars_begin, nvars - 1, 0)]);
        let pc_itr = self.emit(op![IterL(vars_begin, !0)]);
        Loop {
            was_dead: false,
            loop_start,
            end_jumps: JumpList(pc_itr),
        }
    }

    fn stmt_loop_end(&mut self, current_loop: Self::Loop) {
        self.dead = current_loop.was_dead;
        if self.dead || !current_loop.end_jumps.has_jumps() {
            return;
        }
        let target = current_loop.end_jumps.0;
        let here = self.pc();
        match_op! {(&mut self[target]) {
            IterL(_, ref mut d) => {
                *d = target.as_offset_relative_to(here);
            },
            ForL(_, ref mut d) => {
                *d = target.as_offset_relative_to(here);
            },
            _ => {
                self.patch_jmp_list(current_loop.end_jumps, here);
            }
        }}
    }

    fn stmt_do(&mut self) {
        self.enter_scope();
    }

    fn stmt_enddo(&mut self) {
        self.leave_scope();
    }

    fn stmt_locals_uninit(&mut self, names: Vec<(String, String)>) {
        if self.dead {
            return;
        }
        for (name, attrib) in names {
            let reg = self.frame.alloc_with_attribs(name, attrib);
            self.emit_load_prim(reg, 1, 1); // 1 = nil
        }
    }

    fn stmt_locals_multi(&mut self, names: Vec<(String, String)>, mut expr: Self::Expr) {
        if self.dead {
            return;
        }
        self.discharge_vars_mut(&mut expr.value);
        self.expr_free(&expr.value);
        let mut vars = Vec::with_capacity(names.len());
        for (name, attrib) in names {
            let reg = self.frame.alloc_with_attribs(name, attrib);
            vars.push(Expr::new(ExprValue::Local(reg)));
        }
        self.stmt_assignment_multi(vars, expr);
    }

    fn stmt_local(&mut self, var: String, attribs: String, mut expr: Self::Expr) {
        if self.dead {
            return;
        }
        self.discharge_vars_mut(&mut expr.value);
        self.expr_free(&expr.value);
        let reg = self.frame.alloc_with_attribs(var, attribs);
        let mut expr = expr;
        self.expr_to_reg(&mut expr, reg).unwrap();
    }

    fn stmt_return(&mut self, (expr, nargs): Self::ExprList) {
        if self.dead {
            return;
        }
        if nargs == 0 {
            self.emit(op![Ret0]);
        } else if nargs == 1 {
            let ExprValue::NonReloc(reg) = expr.value else {
                panic!("Expected expr to be in next register");
            };
            self.frame.free(reg);
            self.emit(op![Ret1(reg, 1)]);
        } else {
            let ExprValue::NonReloc(first_reg) = expr.value else {
                panic!("Expected expr to be in next register");
            };
            self.emit(op![Ret(first_reg, nargs as u16)]);
            self.frame.free_range(first_reg..first_reg + nargs);
        }
        self.dead = true;
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
        self.stmt_assignment(var, func);
    }

    fn stmt_local_function(&mut self, name: String, proto: Self::Proto) {
        if self.dead {
            return;
        }
        let var = self.frame.alloc(name);
        let mut func = self.expr_function(proto);
        self.expr_to_reg(&mut func, var).unwrap();
    }

    fn stmt_assignment(&mut self, var: Self::Expr, expr: Self::Expr) {
        if self.dead {
            return;
        }
        self.assign_lvalue(var, expr);
        self.frame.check_clean();
    }

    fn stmt_assignment_multi(&mut self, vars: Vec<Self::Expr>, mut expr: Self::Expr) {
        if self.dead {
            return;
        }
        if let Some(range) = self.patch_ret_n(&mut expr.value, vars.len() as u8) {
            // multi returning expression, assign all results
            // TODO
            self.frame.free_range(range);
            for mut var in vars {
                self.expr_to_any_reg(&mut var).unwrap();
                self.expr_free(&var.value);
            }
        } else {
            // has not multiple return values: simple one-to-one assignment (rest is filled with `nil`)
            let mut vars_it = vars.into_iter();
            self.assign_lvalue(vars_it.next().unwrap(), expr);
            for var in vars_it {
                self.assign_lvalue(var, Expr::new(ExprValue::Nil));
            }
        }
    }

    fn stmt_expression(&mut self, mut expr: Self::Expr) {
        if self.dead {
            return;
        }
        let a = match expr.value {
            ExprValue::Call(pc) => {
                match_op! {(&mut self[pc]) {
                    Call(a,_,ref mut c) => {
                        *c = 1; // 1 = no results
                        a
                    },
                    _ => panic!("Expression statement is not a function call"),
                }}
            }
            _ => self.expr_to_any_reg(&mut expr).unwrap(),
        };
        self.frame.free(a);
    }

    fn enter_function(&mut self, _is_method: bool, is_vararg: bool, args: Vec<String>) {
        self.protos.push(ProtoGenerator::new(is_vararg, args));
    }

    fn leave_function(&mut self) -> Self::Proto {
        self.protos.pop().expect("not in a function").into_proto()
    }
}
