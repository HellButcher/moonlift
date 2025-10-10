use std::{convert::Infallible, vec};

use crate::{
    ast,
    parser::{ParseVisitor, ParseVisitorOutput},
};

pub enum Loop {
    None,
    While(Box<ast::Expression>),
    Repeat(Box<ast::Expression>),
    ForNum(String, Vec<ast::Expression>),
    ForEach(Vec<String>, Vec<ast::Expression>),
}

enum If {
    If(ast::Expression),
    Else(ast::Expression, ast::Block),
}

pub struct AstVisitor {
    blocks: Vec<ast::Block>,
    ifs: Vec<If>,
    functions: Vec<(bool, ast::Params)>,
}

impl Loop {
    fn set(&mut self, loop_type: Loop) {
        if !matches!(self, Self::None) {
            panic!("Loop condition already set");
        }
        *self = loop_type;
    }

    #[inline]
    fn while_cond(&mut self, expr: ast::Expression) {
        self.set(Self::While(Box::new(expr)));
    }

    #[inline]
    fn repeat_until_cond(&mut self, expr: ast::Expression) {
        self.set(Self::Repeat(Box::new(expr)));
    }

    #[inline]
    fn fornum(&mut self, var: String, exprs: Vec<ast::Expression>) {
        self.set(Self::ForNum(var, exprs));
    }

    #[inline]
    fn foreach(&mut self, vars: Vec<String>, exprs: Vec<ast::Expression>) {
        self.set(Self::ForEach(vars, exprs));
    }

    fn end(self, block: ast::Block) -> ast::Statement {
        match self {
            Self::None => panic!("No loop condition set"),
            Self::While(cond) => ast::Statement::While { cond, block },
            Self::Repeat(cond) => ast::Statement::Repeat { block, cond },
            Self::ForNum(var, exprs) => ast::Statement::ForNum { var, exprs, block },
            Self::ForEach(vars, exprs) => ast::Statement::ForEach { vars, exprs, block },
        }
    }
}

impl If {
    fn new(condition: ast::Expression) -> Self {
        Self::If(condition)
    }

    fn elsecase(&mut self, block: ast::Block) {
        let Self::If(old_condition) =
            std::mem::replace(self, Self::Else(ast::Expression::Nil, block))
        else {
            panic!("Else case already set");
        };
        let Self::Else(condition, ..) = self else {
            unreachable!()
        };
        *condition = old_condition;
    }

    fn end(self, mut block: ast::Block) -> ast::Statement {
        match self {
            Self::If(condition) => ast::Statement::If {
                ifcases: vec![(condition, block)],
                elsecase: ast::Block::new(),
            },
            Self::Else(condition, ifblock) => {
                if block.len() == 1 && matches!(block.first(), Some(ast::Statement::If { .. })) {
                    if let ast::Statement::If { ifcases, elsecase } = block.pop().unwrap() {
                        let mut all_ifs = vec![(condition, ifblock)];
                        all_ifs.extend(ifcases);
                        ast::Statement::If {
                            ifcases: all_ifs,
                            elsecase,
                        }
                    } else {
                        unreachable!()
                    }
                } else {
                    ast::Statement::If {
                        ifcases: vec![(condition, ifblock)],
                        elsecase: block,
                    }
                }
            }
        }
    }
}

impl AstVisitor {
    pub fn new() -> Self {
        Self {
            blocks: vec![ast::Block::new()],
            ifs: Vec::new(),
            functions: Vec::new(),
        }
    }
    pub fn enter_block(&mut self) {
        self.blocks.push(ast::Block::new());
    }
    pub fn leave_block(&mut self) -> ast::Block {
        self.blocks.pop().unwrap()
    }
    fn current_block_mut(&mut self) -> &mut ast::Block {
        self.blocks.last_mut().unwrap()
    }
    fn push_stmt(&mut self, stmt: ast::Statement) {
        self.current_block_mut().push(stmt);
    }
    pub fn last_stmt(&self) -> Option<&ast::Statement> {
        self.blocks.last()?.last()
    }
    pub fn last_stmt_mut(&mut self) -> Option<&mut ast::Statement> {
        self.blocks.last_mut()?.last_mut()
    }
}

impl ParseVisitorOutput for AstVisitor {
    type Output = ast::Block;
    fn start(&mut self) -> Result<(), Self::Error> {
        self.enter_block();
        Ok(())
    }
    fn done(&mut self) -> Result<Self::Output, Self::Error> {
        Ok(self.leave_block())
    }
}

impl ParseVisitor for AstVisitor {
    type Error = Infallible;
    type Expr = ast::Expression;
    type ExprList = Vec<ast::Expression>;
    type ExprCall = ast::FunctionCall;
    type ExprTable = Vec<ast::Field>;
    type Loop = Loop;
    type Proto = ast::Proto;

    fn stmt_label(&mut self, label: String) {
        self.push_stmt(ast::Statement::Label(label));
    }

    fn stmt_goto(&mut self, label: String) {
        self.push_stmt(ast::Statement::Goto(label));
    }

    fn stmt_break(&mut self) {
        self.stmt_goto("break".to_string());
    }

    fn stmt_if(&mut self, condition: Self::Expr) {
        self.ifs.push(If::new(condition));
        self.enter_block();
    }

    fn stmt_else(&mut self) {
        let block = self.leave_block();
        self.ifs.last_mut().unwrap().elsecase(block);
        self.enter_block();
    }

    fn stmt_endif(&mut self) {
        let block = self.leave_block();
        let stmt = self.ifs.pop().expect("no if to end").end(block);
        self.push_stmt(stmt);
    }

    fn stmt_loop_begin(&mut self) -> Self::Loop {
        self.enter_block();
        Loop::None
    }

    fn stmt_loop_while(&mut self, current_loop: &mut Self::Loop, condition: Self::Expr) {
        current_loop.while_cond(condition);
    }

    fn stmt_loop_repeat_until(&mut self, current_loop: &mut Self::Loop, condition: Self::Expr) {
        current_loop.repeat_until_cond(condition);
    }

    fn stmt_loop_for_begin(&mut self, var: String, exprs: Self::ExprList) -> Self::Loop {
        self.enter_block();
        Loop::ForNum(var, exprs)
    }

    fn stmt_loop_foreach_begin(&mut self, vars: Vec<String>, exprs: Self::ExprList) -> Self::Loop {
        self.enter_block();
        Loop::ForEach(vars, exprs)
    }

    fn stmt_loop_end(&mut self, current_loop: Self::Loop) {
        let block = self.leave_block();
        self.push_stmt(current_loop.end(block));
    }

    fn stmt_do(&mut self) {
        self.enter_block();
    }

    fn stmt_enddo(&mut self) {
        let block = self.leave_block();
        self.push_stmt(ast::Statement::Do(block));
    }

    fn stmt_locals_uninit(&mut self, vars: Vec<(String, String)>) {
        self.push_stmt(ast::Statement::Local { vars, exprs: None });
    }

    fn stmt_locals_multi(&mut self, vars: Vec<(String, String)>, expr: Self::Expr) {
        self.push_stmt(ast::Statement::Local {
            vars,
            exprs: Some(Box::new(expr)),
        });
    }

    fn stmt_local(&mut self, name: String, attribs: String, expr: Self::Expr) {
        self.push_stmt(ast::Statement::Local {
            vars: vec![(name, attribs)],
            exprs: Some(Box::new(expr)),
        });
    }

    fn stmt_return(&mut self, exprs: Self::ExprList) {
        self.push_stmt(ast::Statement::Return(exprs));
    }

    fn stmt_function(&mut self, name: ast::FuncName, proto: ast::Proto) {
        self.push_stmt(ast::Statement::Function { name, proto });
    }

    fn stmt_local_function(&mut self, name: String, proto: ast::Proto) {
        self.push_stmt(ast::Statement::Local {
            vars: vec![(name, String::new())],
            exprs: Some(Box::new(ast::Expression::FunctDef(proto))),
        });
    }

    fn stmt_assignment(&mut self, var: Self::Expr, rhs: Self::Expr) {
        self.push_stmt(ast::Statement::Assign {
            vars: vec![var],
            expr: Box::new(rhs),
        });
    }

    fn stmt_assignment_multi(&mut self, vars: Vec<Self::Expr>, rhs: Self::Expr) {
        self.push_stmt(ast::Statement::Assign {
            vars,
            expr: Box::new(rhs),
        });
    }

    fn stmt_expression(&mut self, expr: Self::Expr) {
        self.push_stmt(ast::Statement::Expression(Box::new(expr)));
    }

    fn expr_list_begin(&mut self) -> Self::ExprList {
        Vec::new()
    }

    fn expr_list_item(&mut self, list: &mut Self::ExprList, expr: Self::Expr) {
        list.push(expr);
    }

    fn expr_number(&mut self, n: ast::Number) -> Self::Expr {
        ast::Expression::Number(n)
    }
    fn expr_string(&mut self, s: Box<[u8]>) -> Self::Expr {
        ast::Expression::String(s)
    }
    fn expr_boolean(&mut self, b: bool) -> Self::Expr {
        ast::Expression::Boolean(b)
    }
    fn expr_nil(&mut self) -> Self::Expr {
        ast::Expression::Nil
    }
    fn expr_ellipsis(&mut self) -> Self::Expr {
        ast::Expression::Ellipsis
    }
    fn expr_function(&mut self, proto: ast::Proto) -> Self::Expr {
        ast::Expression::FunctDef(proto)
    }
    fn expr_prefix(&mut self, op: ast::UnaryOp, expr: Self::Expr) -> Self::Expr {
        ast::Expression::Unary(op, Box::new(expr))
    }
    fn expr_infix(&mut self, lhs: Self::Expr, _op: ast::InfixOp) -> Self::Expr {
        lhs
    }
    fn expr_postfix(&mut self, infix: Self::Expr, op: ast::InfixOp, rhs: Self::Expr) -> Self::Expr {
        ast::Expression::Infix(Box::new(infix), op, Box::new(rhs))
    }
    fn expr_var(&mut self, name: String) -> Self::Expr {
        ast::Expression::Var(name)
    }
    fn expr_index(&mut self, expr: Self::Expr, index: Self::Expr) -> Self::Expr {
        ast::Expression::Index(Box::new(expr), Box::new(index))
    }
    fn expr_field(&mut self, expr: Self::Expr, name: String) -> Self::Expr {
        ast::Expression::Field(Box::new(expr), name)
    }

    fn expr_call_begin(&mut self, prefix: Self::Expr, method: Option<String>) -> Self::ExprCall {
        ast::FunctionCall {
            prefix,
            method,
            args: Vec::new(),
        }
    }
    fn expr_call_arg(&mut self, call: &mut Self::ExprCall, arg: Self::Expr) {
        call.args.push(arg);
    }
    fn expr_call_end(&mut self, call: Self::ExprCall) -> Self::Expr {
        ast::Expression::FunctCall(Box::new(call))
    }

    fn expr_table_begin(&mut self) -> Self::ExprTable {
        Vec::new()
    }
    fn expr_table_field_index(
        &mut self,
        table: &mut Self::ExprTable,
        key: Self::Expr,
        value: Self::Expr,
    ) {
        table.push(ast::Field::Index(key, value));
    }
    fn expr_table_field_named(
        &mut self,
        table: &mut Self::ExprTable,
        name: String,
        value: Self::Expr,
    ) {
        table.push(ast::Field::Named(name, value));
    }
    fn expr_table_field_exp(&mut self, table: &mut Self::ExprTable, expr: Self::Expr) {
        let exp_count = table
            .iter()
            .filter(|f| matches!(f, ast::Field::Exp(_, _)))
            .count();
        table.push(ast::Field::Exp(exp_count + 1, expr));
    }
    fn expr_table_end(&mut self, table: Self::ExprTable) -> Self::Expr {
        ast::Expression::Table(table)
    }

    fn enter_function(&mut self, is_method: bool, is_variadic: bool, args: Vec<String>) {
        self.enter_block();
        self.functions.push((
            is_method,
            ast::Params {
                names: args,
                variadic: is_variadic,
            },
        ));
    }

    fn leave_function(&mut self) -> Self::Proto {
        let block = self.leave_block();
        let (is_method, params) = self.functions.pop().expect("not in function");
        ast::Proto {
            method: is_method,
            params,
            body: block,
        }
    }
}
