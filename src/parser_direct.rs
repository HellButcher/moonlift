use crate::codegen_state::CodeGenerationError;
use crate::lexer::{Lexer, LexerError, Source, Token};
use crate::opcode::Op;
use crate::ast::{Number, InfixOp, UnaryOp}; // Nur für Operatoren-Definitionen
use std::convert::Infallible;

use crate::codegen_state::*;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError<E = Infallible> {
    #[error(transparent)]
    LexerError(#[from] LexerError<E>),
    #[error("Unexpected token: expected {expected}, got {got}")]
    UnexpectedToken { expected: &'static str, got: String },
    #[error(transparent)]
    CodeGenerationError(#[from] CodeGenerationError),
}


/// Bytecode Compiler mit direct parsing
pub struct BytecodeParser<'a, S> {
    lexer: &'a mut Lexer<S>,
    last_token: Option<Token>,
    
    // Bytecode generation
    pub bytecode: Vec<Op>,
    pub constants: ConstantPool,
    pub frame: Frame,
    
    // Jump handling
    jump_patches: Vec<JumpPatch>,
    break_patches: Vec<Vec<JumpPatch>>, // Stack für nested loops
}

impl<'a, S: Source> BytecodeParser<'a, S> {
    pub fn new(lexer: &'a mut Lexer<S>) -> Self {
        Self {
            lexer,
            last_token: None,
            bytecode: Vec::new(),
            constants: ConstantPool::new(),
            frame: Frame::new(),
            jump_patches: Vec::new(),
            break_patches: Vec::new(),
        }
    }

    // Token handling (gleich wie vorher)
    fn peek_token(&mut self) -> Result<&Token, ParseError<S::Error>> {
        if self.last_token.is_none() {
            self.last_token = Some(self.lexer.next_token()?);
        }
        Ok(self.last_token.as_ref().unwrap())
    }

    fn next_token(&mut self) -> Result<Token, ParseError<S::Error>> {
        if let Some(t) = self.last_token.take() {
            Ok(t)
        } else {
            Ok(self.lexer.next_token()?)
        }
    }

    fn skip_token(&mut self) {
        self.last_token = None;
    }

    fn try_symbol(&mut self, sym: &str) -> Result<bool, ParseError<S::Error>> {
        match self.peek_token()? {
            Token::Symbol(s) if *s == sym => {
                self.skip_token();
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn try_keyword(&mut self, kw: &str) -> Result<bool, ParseError<S::Error>> {
        match self.peek_token()? {
            Token::Keyword(k) if *k == kw => {
                self.skip_token();
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn expect_keyword(&mut self, kw: &'static str) -> Result<(), ParseError<S::Error>> {
        match self.next_token()? {
            Token::Keyword(k) if k == kw => Ok(()),
            t => Err(ParseError::UnexpectedToken {
                expected: kw,
                got: format!("{:?}", t)
            }),
        }
    }

    fn expect_symbol(&mut self, sym: &'static str) -> Result<(), ParseError<S::Error>> {
        match self.next_token()? {
            Token::Symbol(s) if s == sym => Ok(()),
            t => Err(ParseError::UnexpectedToken {
                expected: sym,
                got: format!("{:?}", t)
            }),
        }
    }

    fn expect_name(&mut self) -> Result<String, ParseError<S::Error>> {
        match self.next_token()? {
            Token::Name(name) => Ok(name),
            t => Err(ParseError::UnexpectedToken {
                expected: "name",
                got: format!("{:?}", t)
            }),
        }
    }

    // Jump handling
    fn emit_jump(&mut self, jump_type: JumpType) -> usize {
        let patch_index = self.bytecode.len();
        self.bytecode.push(OP![Jmp(0, 0)]); // Placeholder
        self.jump_patches.push(JumpPatch {
            instruction_index: patch_index,
            jump_type,
        });
        patch_index
    }

    fn patch_jump(&mut self, patch_index: usize, target: usize) {
        let offset = target as i32 - patch_index as i32;
        self.bytecode[patch_index] = OP![Jmp(0, offset)];
    }

    fn patch_jumps(&mut self, patches: &[JumpPatch], target: usize) {
        for patch in patches {
            self.patch_jump(patch.instruction_index, target);
        }
    }

    // Main parsing entry point
    pub fn parse_chunk(&mut self) -> Result<(), ParseError<S::Error>> {
        self.parse_block()?;
        
        match self.next_token()? {
            Token::Eof => {
                self.bytecode.push(OP![Ret0(0, 0)]); // End with return
                Ok(())
            }
            t => Err(ParseError::UnexpectedToken {
                expected: "EOF",
                got: format!("{:?}", t)
            }),
        }
    }

    fn parse_block(&mut self) -> Result<(), ParseError<S::Error>> {
        self.frame.enter_scope();
        
        while self.try_parse_statement()? {}
        
        // Handle return statement
        if self.try_keyword("return")? {
            self.parse_return_statement()?;
            self.try_symbol(";")?;
        }
        
        self.frame.exit_scope();
        Ok(())
    }

    fn try_parse_statement(&mut self) -> Result<bool, ParseError<S::Error>> {
        match self.peek_token()? {
            Token::Symbol(";") => {
                self.skip_token();
                Ok(true)
            }
            Token::Symbol("::") => {
                self.parse_label()?;
                Ok(true)
            }
            Token::Keyword("break") => {
                self.parse_break()?;
                Ok(true)
            }
            Token::Keyword("goto") => {
                self.parse_goto()?;
                Ok(true)
            }
            Token::Keyword("if") => {
                self.parse_if_statement()?;
                Ok(true)
            }
            Token::Keyword("while") => {
                self.parse_while_loop()?;
                Ok(true)
            }
            Token::Keyword("repeat") => {
                self.parse_repeat_loop()?;
                Ok(true)
            }
            Token::Keyword("for") => {
                self.parse_for_loop()?;
                Ok(true)
            }
            Token::Keyword("do") => {
                self.parse_do_block()?;
                Ok(true)
            }
            Token::Keyword("local") => {
                self.parse_local_statement()?;
                Ok(true)
            }
            Token::Keyword("function") => {
                self.parse_function_statement()?;
                Ok(true)
            }
            Token::Name(_) | Token::Symbol("(") => {
                // Could be assignment or function call
                if self.is_assignment_start()? {
                    self.parse_assignment()?;
                } else {
                    self.parse_expression_statement()?;
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn is_assignment_start(&mut self) -> Result<bool, ParseError<S::Error>> {
        // Look ahead to determine if this is an assignment
        // This is a simplified heuristic
        let mut paren_depth = 0;
        let mut token_count = 0;
        
        // Save current state
        let saved_token = self.last_token.clone();
        
        loop {
            match self.peek_token()? {
                Token::Symbol("(") => paren_depth += 1,
                Token::Symbol(")") => paren_depth -= 1,
                Token::Symbol("=") if paren_depth == 0 => {
                    // Restore state
                    self.last_token = saved_token;
                    return Ok(true);
                }
                Token::Symbol("," | ";") | Token::Keyword(_) if paren_depth == 0 => {
                    // Restore state  
                    self.last_token = saved_token;
                    return Ok(false);
                }
                Token::Eof => {
                    self.last_token = saved_token;
                    return Ok(false);
                }
                _ => {}
            }
            
            self.skip_token();
            token_count += 1;
            
            // Prevent infinite lookahead
            if token_count > 20 {
                self.last_token = saved_token;
                return Ok(false);
            }
        }
    }

    // Statement parsers
    fn parse_label(&mut self) -> Result<(), ParseError<S::Error>> {
        self.expect_symbol("::")?;
        let _label = self.expect_name()?;
        self.expect_symbol("::")?;
        // TODO: Store label position for goto
        Ok(())
    }

    fn parse_break(&mut self) -> Result<(), ParseError<S::Error>> {
        self.skip_token(); // consume 'break'
        
        // Emit jump to end of current loop
        if let Some(patches) = self.break_patches.last_mut() {
            let patch_index = self.emit_jump(JumpType::Break);
            patches.push(JumpPatch {
                instruction_index: patch_index,
                jump_type: JumpType::Break,
            });
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "break inside loop",
                got: "break outside loop".to_string()
            });
        }
        
        Ok(())
    }

    fn parse_goto(&mut self) -> Result<(), ParseError<S::Error>> {
        self.skip_token(); // consume 'goto'
        let _label = self.expect_name()?;
        // TODO: Implement goto to label
        self.bytecode.push(OP![Jmp(0, 0)]); // Placeholder
        Ok(())
    }

    fn parse_if_statement(&mut self) -> Result<(), ParseError<S::Error>> {
        self.skip_token(); // consume 'if'
        
        // Condition
        let cond_slot = self.parse_expression()?;
        self.expect_keyword("then")?;
        
        // Jump if condition is false
        self.bytecode.push(OP![IsF(cond_slot)]);
        let false_jump = self.emit_jump(JumpType::IfFalse);
        
        // Then block
        self.parse_block()?;
        
        let mut else_jump = None;
        let mut elseif_jumps = Vec::new();
        
        // Handle elseif and else
        loop {
            if self.try_keyword("elseif")? {
                // Jump over elseif when coming from then block
                let skip_jump = self.emit_jump(JumpType::Forward);
                elseif_jumps.push(skip_jump);
                
                // Patch false jump from previous condition to here
                self.patch_jump(false_jump, self.bytecode.len());
                
                // New condition  
                let cond_slot = self.parse_expression()?;
                self.expect_keyword("then")?;
                
                self.bytecode.push(OP![IsF(cond_slot)]);
                let new_false_jump = self.emit_jump(JumpType::IfFalse);
                
                self.parse_block()?;
                
                // Continue with new false jump
                // false_jump = new_false_jump; // This would need to be handled differently
            } else if self.try_keyword("else")? && else_jump.is_none() {
                // Jump over else when coming from then/elseif
                else_jump = Some(self.emit_jump(JumpType::Forward));
                
                // Patch false jump to else block
                self.patch_jump(false_jump, self.bytecode.len());
                
                self.parse_block()?;
            } else {
                break;
            }
        }
        
        self.expect_keyword("end")?;
        
        let end_pos = self.bytecode.len();
        
        // Patch all jumps to end
        if let Some(jump) = else_jump {
            self.patch_jump(jump, end_pos);
        }
        
        for jump in elseif_jumps {
            self.patch_jump(jump, end_pos);
        }
        
        // If no else, patch false jump to end
        if else_jump.is_none() {
            self.patch_jump(false_jump, end_pos);
        }
        
        Ok(())
    }

    fn parse_while_loop(&mut self) -> Result<(), ParseError<S::Error>> {
        self.skip_token(); // consume 'while'
        
        // Start new break patch list for this loop
        self.break_patches.push(Vec::new());
        
        let loop_start = self.bytecode.len();
        
        // Condition
        let cond_slot = self.parse_expression()?;
        self.expect_keyword("do")?;
        
        // Jump if false (exit loop)
        self.bytecode.push(OP![IsF(cond_slot)]);
        let exit_jump = self.emit_jump(JumpType::IfFalse);
        
        // Loop body
        self.parse_block()?;
        
        // Jump back to condition
        let back_offset = loop_start as i32 - self.bytecode.len() as i32 - 1;
        self.bytecode.push(OP![Jmp(0, back_offset)]);
        
        self.expect_keyword("end")?;
        
        let loop_end = self.bytecode.len();
        
        // Patch exit jump
        self.patch_jump(exit_jump, loop_end);
        
        // Patch all break statements
        let break_patches = self.break_patches.pop().unwrap();
        self.patch_jumps(&break_patches, loop_end);
        
        Ok(())
    }

    fn parse_repeat_loop(&mut self) -> Result<(), ParseError<S::Error>> {
        self.skip_token(); // consume 'repeat'
        
        self.break_patches.push(Vec::new());
        
        let loop_start = self.bytecode.len();
        
        // Loop body
        self.parse_block()?;
        
        self.expect_keyword("until")?;
        
        // Condition (loop continues while false)
        let cond_slot = self.parse_expression()?;
        
        // Jump back if condition is false
        self.bytecode.push(OP![IsF(cond_slot)]);
        let back_offset = loop_start as i32 - self.bytecode.len() as i32 - 1;
        self.bytecode.push(OP![Jmp(0, back_offset)]);
        
        let loop_end = self.bytecode.len();
        
        // Patch break statements
        let break_patches = self.break_patches.pop().unwrap();
        self.patch_jumps(&break_patches, loop_end);
        
        Ok(())
    }

    fn parse_for_loop(&mut self) -> Result<(), ParseError<S::Error>> {
        self.skip_token(); // consume 'for'
        
        let var = self.expect_name()?;
        
        if self.try_symbol("=")? {
            // Numeric for loop
            self.parse_numeric_for(var)?;
        } else {
            // Generic for loop
            self.parse_generic_for(var)?;
        }
        
        Ok(())
    }

    fn parse_numeric_for(&mut self, var: String) -> Result<(), ParseError<S::Error>> {
        // for var = start, limit, step do ... end
        let var_slot = self.frame.define_local(var)?;
        let limit_slot = self.frame.alloc_temp()?;
        let step_slot = self.frame.alloc_temp()?;
        
        // Start value
        let start_slot = self.parse_expression()?;
        self.bytecode.push(OP![Mov(var_slot, start_slot)]);
        
        self.expect_symbol(",")?;
        
        // Limit value  
        let limit_expr_slot = self.parse_expression()?;
        self.bytecode.push(OP![Mov(limit_slot, limit_expr_slot)]);
        
        // Step value (default 1)
        if self.try_symbol(",")? {
            let step_expr_slot = self.parse_expression()?;
            self.bytecode.push(OP![Mov(step_slot, step_expr_slot)]);
        } else {
            self.bytecode.push(OP![KShort(step_slot, 1)]);
        }
        
        self.expect_keyword("do")?;
        
        self.break_patches.push(Vec::new());
        
        // Loop start
        let loop_start = self.bytecode.len();
        
        // Check condition: var <= limit (for positive step)
        self.bytecode.push(OP![IsGt(var_slot, limit_slot)]); // Jump if var > limit
        let exit_jump = self.emit_jump(JumpType::IfFalse);
        
        // Loop body
        self.parse_block()?;
        
        // Increment variable
        self.bytecode.push(OP![AddVV(var_slot, var_slot, step_slot)]);
        
        // Jump back to condition  
        let back_offset = loop_start as i32 - self.bytecode.len() as i32 - 1;
        self.bytecode.push(OP![Jmp(0, back_offset)]);
        
        self.expect_keyword("end")?;
        
        let loop_end = self.bytecode.len();
        
        // Patch exit and breaks
        self.patch_jump(exit_jump, loop_end);
        let break_patches = self.break_patches.pop().unwrap();
        self.patch_jumps(&break_patches, loop_end);
        
        Ok(())
    }

    fn parse_generic_for(&mut self, first_var: String) -> Result<(), ParseError<S::Error>> {
        // for var1, var2, ... in exp1, exp2, ... do ... end
        let mut vars = vec![first_var];
        
        // Additional variables
        while self.try_symbol(",")? {
            vars.push(self.expect_name()?);
        }
        
        self.expect_keyword("in")?;
        
        // Iterator expressions
        let _iter_slot = self.parse_expression()?;
        while self.try_symbol(",")? {
            self.parse_expression()?;
        }
        
        self.expect_keyword("do")?;
        
        // Define loop variables
        for var in vars {
            self.frame.define_local(var)?;
        }
        
        // TODO: Implement generic for loop bytecode
        // This is complex and involves iterator protocol
        
        self.break_patches.push(Vec::new());
        self.parse_block()?;
        self.expect_keyword("end")?;
        
        let break_patches = self.break_patches.pop().unwrap();
        let loop_end = self.bytecode.len();
        self.patch_jumps(&break_patches, loop_end);
        
        Ok(())
    }

    fn parse_do_block(&mut self) -> Result<(), ParseError<S::Error>> {
        self.skip_token(); // consume 'do'
        self.parse_block()?;
        self.expect_keyword("end")?;
        Ok(())
    }

    fn parse_local_statement(&mut self) -> Result<(), ParseError<S::Error>> {
        self.skip_token(); // consume 'local'
        
        if self.try_keyword("function")? {
            // local function name(...) ... end
            let name = self.expect_name()?;
            let slot = self.frame.define_local(name)?;
            
            // TODO: Parse function body and create closure
            self.parse_function_body()?;
            
            // For now, create nil
            self.bytecode.push(OP![KPri(slot, 0)]);
        } else {
            // local var1, var2, ... = exp1, exp2, ...
            let mut vars = vec![self.expect_name()?];
            
            while self.try_symbol(",")? {
                vars.push(self.expect_name()?);
            }
            
            let slots: Result<Vec<u8>, _> = vars.into_iter()
                .map(|name| self.frame.define_local(name))
                .collect();
            let slots = slots?;
            
            if self.try_symbol("=")? {
                // Parse expressions and assign to slots
                for slot in &slots {
                    let expr_slot = self.parse_expression()?;
                    self.bytecode.push(OP![Mov(*slot, expr_slot)]);
                    
                    if !self.try_symbol(",")? {
                        break;
                    }
                }
                
                // Initialize remaining variables to nil
                for slot in slots.iter().skip(1) {
                    self.bytecode.push(OP![KPri(*slot, 0)]); // nil
                }
            } else {
                // Initialize all to nil
                for slot in slots {
                    self.bytecode.push(OP![KPri(slot, 0)]);
                }
            }
        }
        
        Ok(())
    }

    fn parse_function_statement(&mut self) -> Result<(), ParseError<S::Error>> {
        self.skip_token(); // consume 'function'
        
        // Function name (can be qualified: a.b.c or a.b:c)
        let mut name_parts = vec![self.expect_name()?];
        
        while self.try_symbol(".")? {
            name_parts.push(self.expect_name()?);
        }
        
        let is_method = self.try_symbol(":")?;
        if is_method {
            name_parts.push(self.expect_name()?);
        }
        
        // TODO: Handle qualified function names properly
        // For now, just treat as simple assignment to first name
        let slot = if let Some(slot) = self.frame.get_local(&name_parts[0]) {
            slot
        } else {
            self.frame.define_local(name_parts[0].clone())?
        };
        
        self.parse_function_body()?;
        
        // For now, assign nil (TODO: actual function object)
        self.bytecode.push(OP![KPri(slot, 0)]);
        
        Ok(())
    }

    fn parse_function_body(&mut self) -> Result<(), ParseError<S::Error>> {
        self.expect_symbol("(")?;
        
        // Parameters
        if !self.try_symbol(")")? {
            loop {
                if self.try_symbol("...")? {
                    // Variadic function
                    break;
                } else {
                    let param = self.expect_name()?;
                    self.frame.define_local(param)?;
                    
                    if self.try_symbol(")")? {
                        break;
                    } else {
                        self.expect_symbol(",")?;
                    }
                }
            }
        }
        
        // Function body
        self.parse_block()?;
        self.expect_keyword("end")?;
        
        Ok(())
    }

    fn parse_assignment(&mut self) -> Result<(), ParseError<S::Error>> {
        // Parse left-hand side variables
        let mut lhs_slots = Vec::new();
        
        loop {
            // Parse variable reference (var, var.field, var[index])
            let slot = self.parse_lvalue()?;
            lhs_slots.push(slot);
            
            if self.try_symbol(",")? {
                continue;
            } else {
                break;
            }
        }
        
        self.expect_symbol("=")?;
        
        // Parse right-hand side expressions
        for slot in &lhs_slots {
            let expr_slot = self.parse_expression()?;
            self.bytecode.push(OP![Mov(*slot, expr_slot)]);
            
            if !self.try_symbol(",")? {
                break;
            }
        }
        
        Ok(())
    }

    fn parse_lvalue(&mut self) -> Result<u8, ParseError<S::Error>> {
        // Parse variable name
        let name = self.expect_name()?;
        
        if let Some(slot) = self.frame.get_local(&name) {
            // TODO: Handle field access and indexing
            Ok(slot)
        } else {
            // Global variable - for now treat as local
            self.frame.define_local(name)
        }
    }

    fn parse_expression_statement(&mut self) -> Result<(), ParseError<S::Error>> {
        // This should be a function call
        let _result_slot = self.parse_expression()?;
        Ok(())
    }

    fn parse_return_statement(&mut self) -> Result<(), ParseError<S::Error>> {
        // return [exp1, exp2, ...]
        
        if self.is_expression_start()? {
            let mut return_slots = Vec::new();
            
            loop {
                let expr_slot = self.parse_expression()?;
                return_slots.push(expr_slot);
                
                if !self.try_symbol(",")? {
                    break;
                }
            }
            
            if return_slots.is_empty() {
                self.bytecode.push(OP![Ret0(0, 0)]);
            } else if return_slots.len() == 1 {
                self.bytecode.push(OP![Ret1(return_slots[0], 0)]);
            } else {
                // Multiple returns - pack them starting from first slot
                let base = return_slots[0];
                for (i, &slot) in return_slots.iter().enumerate().skip(1) {
                    self.bytecode.push(OP![Mov((base + i as u8), slot)]);
                }
                self.bytecode.push(OP![Ret(base, return_slots.len())]);
            }
        } else {
            self.bytecode.push(OP![Ret0(0, 0)]);
        }
        
        Ok(())
    }

    fn is_expression_start(&mut self) -> Result<bool, ParseError<S::Error>> {
        match self.peek_token()? {
            Token::Keyword("nil" | "false" | "true" | "function") => Ok(true),
            Token::Symbol("..." | "{" | "(" | "not" | "-" | "#" | "~") => Ok(true),
            Token::Number(_) | Token::String(_) | Token::Name(_) => Ok(true),
            _ => Ok(false),
        }
    }

    // Expression parsing
    fn parse_expression(&mut self) -> Result<u8, ParseError<S::Error>> {
        self.parse_expression_with_precedence(0)
    }

    fn parse_expression_with_precedence(&mut self, base_precedence: u8) -> Result<u8, ParseError<S::Error>> {
        // Parse primary expression
        let mut result_slot = self.parse_primary_expression()?;

        // Handle binary operators
        while let Some((op, precedence)) = self.try_binary_operator(base_precedence)? {
            let right_slot = self.parse_expression_with_precedence(precedence)?;
            
            // Generate appropriate bytecode for operator
            result_slot = self.emit_binary_op(op, result_slot, right_slot)?;
        }

        Ok(result_slot)
    }

    fn parse_primary_expression(&mut self) -> Result<u8, ParseError<S::Error>> {
        let result_slot = self.frame.alloc_temp()?;

        match self.next_token()? {
            Token::Keyword("nil") => {
                self.bytecode.push(OP![KPri(result_slot, 0)]);
                Ok(result_slot)
            }
            Token::Keyword("false") => {
                self.bytecode.push(OP![KPri(result_slot, 1)]);
                Ok(result_slot)
            }
            Token::Keyword("true") => {
                self.bytecode.push(OP![KPri(result_slot, 2)]);
                Ok(result_slot)
            }
            Token::Number(n) => {
                match n {
                    Number::Integer(i) => {
                        if i >= i16::MIN as i64 && i <= i16::MAX as i64 {
                            self.bytecode.push(OP![KShort(result_slot, i)]);
                        } else {
                            let const_idx = self.constants.add_number(i as f64)?;
                            self.bytecode.push(OP![KNum(result_slot, const_idx)]);
                        }
                    }
                    Number::Float(f) => {
                        let const_idx = self.constants.add_number(f)?;
                        self.bytecode.push(OP![KNum(result_slot, const_idx)]);
                    }
                }
                Ok(result_slot)
            }
            Token::String(s) => {
                let const_idx = self.constants.add_string(s)?;
                self.bytecode.push(OP![KStr(result_slot, const_idx)]);
                Ok(result_slot)
            }
            Token::Name(name) => {
                if let Some(local_slot) = self.frame.get_local(&name) {
                    self.bytecode.push(OP![Mov(result_slot, local_slot)]);
                } else {
                    // Global variable access
                    let const_idx = self.constants.add_string(name.into_bytes())?;
                    self.bytecode.push(OP![GGet(result_slot, const_idx)]);
                }
                
                // Handle suffixed expressions (function calls, indexing, field access)
                self.parse_suffixed_expression(result_slot)
            }
            Token::Symbol("(") => {
                let expr_slot = self.parse_expression()?;
                self.expect_symbol(")")?;
                self.bytecode.push(OP![Mov(result_slot, expr_slot)]);
                Ok(result_slot)
            }
            Token::Symbol("{") => {
                self.parse_table_constructor(result_slot)
            }
            Token::Keyword("function") => {
                self.parse_function_expression(result_slot)
            }
            Token::Symbol("...") => {
                // Varargs
                // TODO: Implement varargs handling
                self.bytecode.push(OP![KPri(result_slot, 0)]); // placeholder
                Ok(result_slot)
            }
            // Unary operators
            Token::Keyword("not") => {
                let operand_slot = self.parse_expression_with_precedence(UnaryOp::PRECEDENCE_LEVEL)?;
                self.bytecode.push(OP![Not(result_slot, operand_slot)]);
                Ok(result_slot)
            }
            Token::Symbol("-") => {
                let operand_slot = self.parse_expression_with_precedence(UnaryOp::PRECEDENCE_LEVEL)?;
                self.bytecode.push(OP![UNM(result_slot, operand_slot)]);
                Ok(result_slot)
            }
            Token::Symbol("#") => {
                let operand_slot = self.parse_expression_with_precedence(UnaryOp::PRECEDENCE_LEVEL)?;
                self.bytecode.push(OP![Len(result_slot, operand_slot)]);
                Ok(result_slot)
            }
            Token::Symbol("~") => {
                let operand_slot = self.parse_expression_with_precedence(UnaryOp::PRECEDENCE_LEVEL)?;
                self.bytecode.push(OP![BNot(result_slot, operand_slot)]);
                Ok(result_slot)
            }
            t => Err(ParseError::UnexpectedToken {
                expected: "expression",
                got: format!("{:?}", t)
            }),
        }
    }

    fn parse_suffixed_expression(&mut self, mut base_slot: u8) -> Result<u8, ParseError<S::Error>> {
        loop {
            match self.peek_token()? {
                Token::Symbol("[") => {
                    // Indexing: base[index]
                    self.skip_token();
                    let index_slot = self.parse_expression()?;
                    self.expect_symbol("]")?;
                    
                    let result_slot = self.frame.alloc_temp()?;
                    self.bytecode.push(OP![TGetV(result_slot, base_slot, index_slot)]);
                    base_slot = result_slot;
                }
                Token::Symbol(".") => {
                    // Field access: base.field
                    self.skip_token();
                    let field_name = self.expect_name()?;
                    let const_idx = self.constants.add_string(field_name.into_bytes())?;
                    
                    let result_slot = self.frame.alloc_temp()?;
                    self.bytecode.push(OP![TGetS(result_slot, base_slot, const_idx)]);
                    base_slot = result_slot;
                }
                Token::Symbol(":") => {
                    // Method call: base:method(args)
                    self.skip_token();
                    let method_name = self.expect_name()?;
                    let args = self.parse_function_args()?;
                    
                    // TODO: Implement method call bytecode
                    let result_slot = self.frame.alloc_temp()?;
                    self.bytecode.push(OP![Call(base_slot, args.len() + 1, 1)]); // +1 for self
                    base_slot = result_slot;
                }
                Token::Symbol("(") | Token::Symbol("{") | Token::String(_) => {
                    // Function call: base(args)
                    let args = self.parse_function_args()?;
                    
                    let result_slot = self.frame.alloc_temp()?;
                    self.bytecode.push(OP![Call(base_slot, args.len(), 1)]);
                    base_slot = result_slot;
                }
                _ => break,
            }
        }
        
        Ok(base_slot)
    }

    fn parse_function_args(&mut self) -> Result<Vec<u8>, ParseError<S::Error>> {
        match self.peek_token()? {
            Token::Symbol("(") => {
                self.skip_token();
                let mut args = Vec::new();
                
                if !self.try_symbol(")")? {
                    loop {
                        let arg_slot = self.parse_expression()?;
                        args.push(arg_slot);
                        
                        if self.try_symbol(")")? {
                            break;
                        } else {
                            self.expect_symbol(",")?;
                        }
                    }
                }
                
                Ok(args)
            }
            Token::Symbol("{") => {
                // Table argument
                let table_slot = self.parse_table_constructor(self.frame.alloc_temp()?)?;
                Ok(vec![table_slot])
            }
            Token::String(_) => {
                // String argument
                let string_slot = self.parse_primary_expression()?;
                Ok(vec![string_slot])
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "function arguments",
                got: format!("{:?}", self.peek_token()?)
            }),
        }
    }

    fn parse_table_constructor(&mut self, result_slot: u8) -> Result<u8, ParseError<S::Error>> {
        self.expect_symbol("{")?;
        
        // Create empty table
        self.bytecode.push(OP![TNew(result_slot, 0)]); // TODO: size hint
        
        if !self.try_symbol("}")? {
            let mut array_index = 1u8;
            
            loop {
                // Parse field
                if self.try_symbol("[")? {
                    // [expr] = expr
                    let key_slot = self.parse_expression()?;
                    self.expect_symbol("]")?;
                    self.expect_symbol("=")?;
                    let value_slot = self.parse_expression()?;
                    
                    self.bytecode.push(OP![TSetV(value_slot, result_slot, key_slot)]);
                } else {
                    let expr_slot = self.parse_expression()?;
                    
                    // Check if this is name = expr
                    if let Some(Token::Symbol("=")) = self.peek_token().ok() {
                        // This was actually a name, not an expression
                        // TODO: This is a bit hacky, need better parsing
                        self.skip_token(); // consume '='
                        let value_slot = self.parse_expression()?;
                        
                        // Use expr_slot as string key (this is simplified)
                        self.bytecode.push(OP![TSetV(value_slot, result_slot, expr_slot)]);
                    } else {
                        // Array element
                        self.bytecode.push(OP![TSetB(expr_slot, result_slot, array_index)]);
                        array_index += 1;
                    }
                }
                
                if self.try_symbol("}")? {
                    break;
                } else if self.try_symbol(",")? || self.try_symbol(";")? {
                    if self.try_symbol("}")? {
                        break;
                    }
                } else {
                    self.expect_symbol("}")?;
                    break;
                }
            }
        }
        
        Ok(result_slot)
    }

    fn parse_function_expression(&mut self, result_slot: u8) -> Result<u8, ParseError<S::Error>> {
        // function(params) body end
        self.parse_function_body()?;
        
        // TODO: Create actual function closure
        self.bytecode.push(OP![KPri(result_slot, 0)]); // placeholder
        
        Ok(result_slot)
    }

    fn try_binary_operator(&mut self, base_precedence: u8) -> Result<Option<(InfixOp, u8)>, ParseError<S::Error>> {
        if let Ok(token) = self.peek_token() {
            let op_str = match token {
                Token::Symbol(s) => s,
                Token::Keyword(k) => k,
                _ => return Ok(None),
            };

            if let Some(op) = InfixOp::from_str(op_str) {
                let precedence = op.precedence_level();
                if precedence > base_precedence {
                    self.skip_token();
                    return Ok(Some((op, precedence)));
                }
            }
        }
        
        Ok(None)
    }

    fn emit_binary_op(&mut self, op: InfixOp, left_slot: u8, right_slot: u8) -> Result<u8, ParseError<S::Error>> {
        let result_slot = self.frame.alloc_temp()?;

        match op {
            // Arithmetic
            InfixOp::Add => self.bytecode.push(OP![AddVV(result_slot, left_slot, right_slot)]),
            InfixOp::Sub => self.bytecode.push(OP![SubVV(result_slot, left_slot, right_slot)]),
            InfixOp::Mul => self.bytecode.push(OP![MulVV(result_slot, left_slot, right_slot)]),
            InfixOp::Div => self.bytecode.push(OP![DivVV(result_slot, left_slot, right_slot)]),
            InfixOp::FloorDiv => self.bytecode.push(OP![IDivVV(result_slot, left_slot, right_slot)]),
            InfixOp::Mod => self.bytecode.push(OP![ModVV(result_slot, left_slot, right_slot)]),
            InfixOp::Pow => self.bytecode.push(OP![Pow(result_slot, left_slot, right_slot)]),
            
            // Bitwise
            InfixOp::BitAnd => self.bytecode.push(OP![BAndVV(result_slot, left_slot, right_slot)]),
            InfixOp::BitOr => self.bytecode.push(OP![BOrVV(result_slot, left_slot, right_slot)]),
            InfixOp::BitXor => self.bytecode.push(OP![BXorVV(result_slot, left_slot, right_slot)]),
            InfixOp::ShiftL => self.bytecode.push(OP![ShLVV(result_slot, left_slot, right_slot)]),
            InfixOp::ShiftR => self.bytecode.push(OP![ShRVV(result_slot, left_slot, right_slot)]),
            
            // Logical (these need special handling for short-circuiting)
            InfixOp::And => {
                // Short-circuit: if left is false, result = left, else result = right
                self.bytecode.push(OP![Mov(result_slot, left_slot)]);
                self.bytecode.push(OP![IsFC(result_slot, result_slot)]);
                let skip_jump = self.emit_jump(JumpType::Forward);
                self.bytecode.push(OP![Mov(result_slot, right_slot)]);
                self.patch_jump(skip_jump, self.bytecode.len());
            }
            InfixOp::Or => {
                // Short-circuit: if left is true, result = left, else result = right
                self.bytecode.push(OP![Mov(result_slot, left_slot)]);
                self.bytecode.push(OP![IsTC(result_slot, result_slot)]);
                let skip_jump = self.emit_jump(JumpType::Forward);
                self.bytecode.push(OP![Mov(result_slot, right_slot)]);
                self.patch_jump(skip_jump, self.bytecode.len());
            }
            
            // Comparison (these set up conditional jumps)
            InfixOp::Eq => {
                self.bytecode.push(OP![IsEqV(left_slot, right_slot)]);
                self.bytecode.push(OP![KPri(result_slot, 1)]); // false
                let skip_jump = self.emit_jump(JumpType::Forward);
                self.bytecode.push(OP![KPri(result_slot, 2)]); // true
                self.patch_jump(skip_jump, self.bytecode.len());
            }
            InfixOp::NotEq => {
                self.bytecode.push(OP![IsNeV(left_slot, right_slot)]);
                self.bytecode.push(OP![KPri(result_slot, 1)]); // false
                let skip_jump = self.emit_jump(JumpType::Forward);
                self.bytecode.push(OP![KPri(result_slot, 2)]); // true
                self.patch_jump(skip_jump, self.bytecode.len());
            }
            InfixOp::Less => {
                self.bytecode.push(OP![IsLt(left_slot, right_slot)]);
                self.bytecode.push(OP![KPri(result_slot, 1)]); // false
                let skip_jump = self.emit_jump(JumpType::Forward);
                self.bytecode.push(OP![KPri(result_slot, 2)]); // true
                self.patch_jump(skip_jump, self.bytecode.len());
            }
            InfixOp::Greater => {
                self.bytecode.push(OP![IsGt(left_slot, right_slot)]);
                self.bytecode.push(OP![KPri(result_slot, 1)]); // false
                let skip_jump = self.emit_jump(JumpType::Forward);
                self.bytecode.push(OP![KPri(result_slot, 2)]); // true
                self.patch_jump(skip_jump, self.bytecode.len());
            }
            InfixOp::LessEq => {
                self.bytecode.push(OP![IsLe(left_slot, right_slot)]);
                self.bytecode.push(OP![KPri(result_slot, 1)]); // false
                let skip_jump = self.emit_jump(JumpType::Forward);
                self.bytecode.push(OP![KPri(result_slot, 2)]); // true
                self.patch_jump(skip_jump, self.bytecode.len());
            }
            InfixOp::GreaterEq => {
                self.bytecode.push(OP![IsGe(left_slot, right_slot)]);
                self.bytecode.push(OP![KPri(result_slot, 1)]); // false
                let skip_jump = self.emit_jump(JumpType::Forward);
                self.bytecode.push(OP![KPri(result_slot, 2)]); // true
                self.patch_jump(skip_jump, self.bytecode.len());
            }
            
            // String concatenation
            InfixOp::Concat => {
                self.bytecode.push(OP![Cat(result_slot, left_slot, right_slot)]);
            }
        }

        Ok(result_slot)
    }

    // Utility functions
    pub fn finish(self) -> CompilationResult {
        CompilationResult {
            bytecode: self.bytecode,
            constants: self.constants,
        }
    }
}

/// Result of compilation
#[derive(Debug)]
pub struct CompilationResult {
    pub bytecode: Vec<Op>,
    pub constants: ConstantPool,
}

impl CompilationResult {
    pub fn print_disassembly(&self) {
        println!("=== BYTECODE DISASSEMBLY ===");
        for (i, op) in self.bytecode.iter().enumerate() {
            println!("{:04}: {:?}", i, op);
        }
        
        println!("\n=== CONSTANTS ===");
        println!("Strings: {:?}", self.constants.strings);
        println!("Numbers: {:?}", self.constants.numbers);
    }
}

// Helper functions for easy usage
pub fn compile_chunk(source: &str) -> Result<CompilationResult, ParseError<std::convert::Infallible>> {
    let mut lexer = Lexer::from_bytes(source);
    let mut parser = BytecodeParser::new(&mut lexer);
    
    parser.parse_chunk()?;
    Ok(parser.finish())
}

pub fn compile_expression(source: &str) -> Result<CompilationResult, ParseError<std::convert::Infallible>> {
    let mut lexer = Lexer::from_bytes(source);
    let mut parser = BytecodeParser::new(&mut lexer);
    
    parser.parse_expression()?;
    Ok(parser.finish())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_simple_assignment() {
        let result = compile_chunk("local x = 42").unwrap();
        result.print_disassembly();
        
        // Should generate: KShort(0, 42)
        assert!(!result.bytecode.is_empty());
    }

    #[test]
    fn test_arithmetic_expression() {
        let result = compile_chunk("local x = 10 + 20 * 30").unwrap();
        result.print_disassembly();
        
        // Should generate proper precedence handling
        assert!(!result.bytecode.is_empty());
    }

    #[test]
    fn test_if_statement() {
        let result = compile_chunk(r#"
            local x = 5
            if x > 0 then
                x = x + 1
            else
                x = 0
            end
        "#).unwrap();
        result.print_disassembly();
        
        assert!(!result.bytecode.is_empty());
    }

    #[test]
    fn test_while_loop() {
        let result = compile_chunk(r#"
            local i = 0
            while i < 10 do
                i = i + 1
            end
        "#).unwrap();
        result.print_disassembly();
        
        assert!(!result.bytecode.is_empty());
    }

    #[test]
    fn test_function_call() {
        let result = compile_chunk(r#"
            print("Hello, World!")
            local result = math.max(10, 20)
        "#).unwrap();
        result.print_disassembly();
        
        assert!(!result.bytecode.is_empty());
    }

    #[test]
    fn test_table_constructor() {
        let result = compile_chunk(r#"
            local t = {1, 2, 3, x = 10, [5] = "five"}
        "#).unwrap();
        result.print_disassembly();
        
        assert!(!result.bytecode.is_empty());
    }
}
