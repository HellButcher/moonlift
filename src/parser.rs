use std::{collections::VecDeque, convert::Infallible, fmt::Debug, io};

use crate::{
    ast,
    lexer::{Lexer, LexerError, Position, Token},
    source::Source,
    Error, ErrorWithPosition,
};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Unexpected token {got}: expected {expected}, got {got}")]
    UnexpectedToken {
        expected: &'static str,
        got: &'static str,
    },
    #[error("Unexpected token {got}: expected {expected} to close {opening} at {opening_pos}")]
    UnexpectedClosingToken {
        expected: &'static str,
        got: &'static str,
        opening: &'static str,
        opening_pos: Position,
    },
}

pub struct Parser<'a, S, V: ?Sized> {
    lex: &'a mut Lexer<S>,
    lookahead: Vec<Token>,
    visitor: &'a mut V,
}

pub trait ParseVisitor {
    type Error;
    type Expr;
    type Proto;

    fn enter_scope(&mut self) {}
    fn leave_scope(&mut self) {}

    fn enter_expr(&mut self) {}
    fn leave_expr(&mut self) {}

    fn expr_number(&mut self, n: ast::Number) -> Self::Expr;
    fn expr_string(&mut self, s: Box<[u8]>) -> Self::Expr;
    fn expr_boolean(&mut self, b: bool) -> Self::Expr;
    fn expr_nil(&mut self) -> Self::Expr;
    fn expr_ellipsis(&mut self) -> Self::Expr;
    fn expr_function(&mut self, params: Self::Proto) -> Self::Expr;
    fn expr_var(&mut self, name: String) -> Self::Expr;
    fn expr_index(&mut self, expr: Self::Expr, index: Self::Expr) -> Self::Expr;
    fn expr_field(&mut self, expr: Self::Expr, name: String) -> Self::Expr;
    fn expr_self(&mut self, prefix: Self::Expr, method: String) -> Self::Expr;
    fn expr_call(
        &mut self,
        prefix: Self::Expr,
        args: Vec<Self::Expr>,
        is_method: bool,
    ) -> Self::Expr;

    fn expr_prefix(&mut self, op: ast::UnaryOp, expr: Self::Expr) -> Self::Expr;
    // infix first emitted as `let tmp = expr_infix(lhs, op);`
    // then rhs is emitted as `let expr = expr_infix(tmp, op, rhs);`
    fn expr_infix(&mut self, lhs: Self::Expr, op: ast::InfixOp) -> Self::Expr;
    fn expr_postfix(&mut self, infix: Self::Expr, op: ast::InfixOp, rhs: Self::Expr) -> Self::Expr;

    fn expr_table_begin(&mut self);
    fn expr_table_field_index(&mut self, key: Self::Expr, value: Self::Expr);
    fn expr_table_field_named(&mut self, name: String, value: Self::Expr);
    fn expr_table_field_exp(&mut self, expr: Self::Expr);
    fn expr_table_end(&mut self) -> Self::Expr;

    fn enter_function(&mut self, is_method: bool, is_variadic: bool, args: Vec<String>);
    fn leave_function(&mut self) -> Self::Proto;

    fn stmt_label(&mut self, label: String);
    fn stmt_goto(&mut self, label: String);
    fn stmt_break(&mut self) {
        self.stmt_goto(String::from("break"));
    }

    fn stmt_if(&mut self, condition: Self::Expr);
    fn stmt_else(&mut self);
    fn stmt_endif(&mut self);

    fn stmt_loop(&mut self);
    fn stmt_loop_while(&mut self, condition: Self::Expr);
    fn stmt_loop_repeat_until(&mut self, condition: Self::Expr);
    fn stmt_loop_for(&mut self, var: String, exprs: Vec<Self::Expr>);
    fn stmt_loop_foreach(&mut self, vars: Vec<String>, exprs: Vec<Self::Expr>);
    fn stmt_endloop(&mut self);

    fn stmt_do(&mut self) {}
    fn stmt_enddo(&mut self) {}

    fn stmt_locals_uninit(&mut self, names: Vec<(String, String)>);
    fn stmt_locals_multi(&mut self, names: Vec<(String, String)>, expr: Self::Expr);
    fn stmt_local(&mut self, var: String, attribs: String, expr: Self::Expr);

    fn stmt_return(&mut self, exprs: Vec<Self::Expr>);
    fn stmt_function(&mut self, name: ast::FuncName, proto: Self::Proto);
    fn stmt_local_function(&mut self, name: String, proto: Self::Proto);
    fn stmt_assignment(&mut self, lhs: Self::Expr, rhs: Self::Expr);
    fn stmt_assignment_multi(&mut self, lhs: Vec<Self::Expr>, rhs: Self::Expr);
    fn stmt_expression(&mut self, expr: Self::Expr);
}

#[cfg(debug_assertions)]
fn print_position_when_unwinding<S, V, F, R>(parser: &mut Parser<S, V>, f: F) -> R
where
    S: Source,
    V: Debug + ?Sized,
    F: FnOnce(&mut Parser<S, V>) -> R + std::panic::UnwindSafe,
{
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(parser))) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Panic while parsing position {}", parser.lex.position());
            eprintln!("Lookahead: {:#?}", parser.lookahead);
            eprintln!("Visitor state: {:#?}", parser.visitor);
            std::panic::resume_unwind(e);
        }
    }
}

#[cfg(not(debug_assertions))]
fn print_position_when_unwinding<S, V, F, R>(parser: &mut Parser<S, V>, f: F) -> R
where
    S: Source,
    V: ?Sized,
    F: FnOnce(&mut Parser<S, V>) -> R + std::panic::UnwindSafe,
{
    f(parser)
}

pub trait ParseVisitorOutput: ParseVisitor {
    type Output;
    fn start(&mut self) -> Result<(), Self::Error>;
    fn done(&mut self) -> Result<Self::Output, Self::Error>;

    fn parse_bytes(
        &mut self,
        bytes: impl AsRef<[u8]>,
    ) -> Result<Self::Output, ErrorWithPosition<Infallible, Self::Error>> {
        let mut lexer = Lexer::from_bytes(bytes);
        let mut parser = Parser::new(&mut lexer, self);
        parser.parse_with_err_pos()
    }

    fn parse_read(
        &mut self,
        read: impl io::Read,
    ) -> Result<Self::Output, ErrorWithPosition<io::Error, Self::Error>> {
        let mut lexer = Lexer::read(read);
        let mut parser = Parser::new(&mut lexer, self);
        parser.parse_with_err_pos()
    }

    fn parse_bytes_with_debug(
        &mut self,
        bytes: impl AsRef<[u8]>,
    ) -> Result<Self::Output, ErrorWithPosition<Infallible, Self::Error>>
    where
        Self: Debug,
    {
        let mut lexer = Lexer::from_bytes(bytes);
        let mut parser = Parser::new(&mut lexer, self);
        print_position_when_unwinding(&mut parser, |parser| parser.parse_with_err_pos())
    }

    fn parse_read_with_debug(
        &mut self,
        read: impl io::Read,
    ) -> Result<Self::Output, ErrorWithPosition<io::Error, Self::Error>>
    where
        Self: Debug,
    {
        let mut lexer = Lexer::read(read);
        let mut parser = Parser::new(&mut lexer, self);
        print_position_when_unwinding(&mut parser, |parser| parser.parse_with_err_pos())
    }
}

impl<'a, S: Source, V: ?Sized> Parser<'a, S, V> {
    pub fn new(lex: &'a mut Lexer<S>, visitor: &'a mut V) -> Self {
        Self {
            lex,
            lookahead: Vec::new(),
            visitor,
        }
    }

    #[inline]
    fn peek_token(&mut self) -> Result<&Token, LexerError<S::Error>> {
        if !self.lookahead.is_empty() {
            return Ok(self.lookahead.last().unwrap());
        }
        let token = self.lex.next_token()?;
        self.lookahead.push(token);
        Ok(self.lookahead.last().unwrap())
    }

    #[inline]
    fn next_token(&mut self) -> Result<Token, LexerError<S::Error>> {
        if let Some(t) = self.lookahead.pop() {
            Ok(t)
        } else {
            self.lex.next_token()
        }
    }

    #[inline]
    fn pop_token(&mut self) -> Token {
        self.lookahead.pop().expect("token was not peeked!")
    }

    #[inline]
    fn put_back_token(&mut self, token: Token) {
        self.lookahead.push(token);
    }

    fn try_symbol(&mut self, sym: &str) -> Result<bool, LexerError<S::Error>> {
        match self.peek_token()? {
            Token::Symbol(s) if *s == sym => {
                self.pop_token();
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn try_keyword(&mut self, kw: &str) -> Result<bool, LexerError<S::Error>> {
        match self.peek_token()? {
            Token::Keyword(k) if *k == kw => {
                self.pop_token();
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    #[inline]
    fn expect_keyword<E>(&mut self, kw: &'static str) -> Result<(), Error<S::Error, E>> {
        match self.next_token()? {
            Token::Keyword(k) if k == kw => Ok(()),
            e => Err(Error::ParseError(ParseError::UnexpectedToken {
                got: e.name(),
                expected: kw,
            })),
        }
    }

    #[inline]
    fn expect_symbol<E>(&mut self, sym: &'static str) -> Result<(), Error<S::Error, E>> {
        match self.next_token()? {
            Token::Symbol(s) if s == sym => Ok(()),
            e => Err(Error::ParseError(ParseError::UnexpectedToken {
                got: e.name(),
                expected: sym,
            })),
        }
    }

    fn expect_match<E>(
        &mut self,
        close: &'static str,
        opened_by: &'static str,
        opened_pos: Position,
    ) -> Result<(), Error<S::Error, E>> {
        match self.next_token()? {
            Token::Symbol(s) | Token::Keyword(s) if s == close => Ok(()),
            e => Err(Error::ParseError(ParseError::UnexpectedClosingToken {
                got: e.name(),
                expected: close,
                opening: opened_by,
                opening_pos: opened_pos,
            })),
        }
    }

    #[inline]
    fn expect_name<E>(&mut self) -> Result<String, Error<S::Error, E>> {
        match self.next_token()? {
            Token::Name(name) => Ok(name),
            e => Err(Error::ParseError(ParseError::UnexpectedToken {
                got: e.name(),
                expected: "<Name>",
            })),
        }
    }

    #[inline]
    fn expect_string<E>(&mut self) -> Result<Box<[u8]>, Error<S::Error, E>> {
        match self.next_token()? {
            Token::String(s) => Ok(s),
            e => Err(Error::ParseError(ParseError::UnexpectedToken {
                got: e.name(),
                expected: "<StringLiteral>",
            })),
        }
    }

    #[inline]
    fn expect_number<E>(&mut self) -> Result<ast::Number, Error<S::Error, E>> {
        match self.next_token()? {
            Token::Number(n) => Ok(n),
            e => Err(Error::ParseError(ParseError::UnexpectedToken {
                got: e.name(),
                expected: "<Numeral>",
            })),
        }
    }
}

impl<'a, S: Source, V: ParseVisitorOutput + ?Sized> Parser<'a, S, V> {
    pub fn parse(&mut self) -> Result<V::Output, Error<S::Error, V::Error>> {
        self.visitor.start();
        self.parse_block()?;
        match self.next_token()? {
            Token::Eof => Self::wrap_visitor_error(self.visitor.done()),
            t => Err(Error::ParseError(ParseError::UnexpectedToken {
                got: t.name(),
                expected: "EOF (end of file)",
            })),
        }
    }

    pub fn parse_with_err_pos(
        &mut self,
    ) -> Result<V::Output, ErrorWithPosition<S::Error, V::Error>> {
        match self.parse() {
            Ok(output) => Ok(output),
            Err(e) => Err(ErrorWithPosition {
                error: e,
                position: self.lex.position(),
            }),
        }
    }
}

impl<'a, S: Source, V: ParseVisitor + ?Sized> Parser<'a, S, V> {
    fn wrap_visitor_error<T>(res: Result<T, V::Error>) -> Result<T, Error<S::Error, V::Error>> {
        res.map_err(Error::CodegenError)
    }

    /// Parses a Lua `block`.
    ///
    /// Grammar:
    /// ```ebnf
    /// block ::= {stat} [retstat]
    /// ```
    fn parse_block(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        self.visitor.enter_scope();
        self.parse_statement_list()?;
        self.visitor.leave_scope();
        Ok(())
    }

    /// Parses a a list of statements. like block, but without scope management.
    ///
    /// Grammar:
    /// ```ebnf
    /// block ::= {stat} [retstat]
    /// ```
    fn parse_statement_list(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        while !self.is_block_follow(true)? {
            self.parse_statement()?;
        }
        Ok(())
    }

    fn is_block_follow<E>(&mut self, with_until: bool) -> Result<bool, Error<S::Error, E>> {
        let t = self.peek_token()?;
        Ok(
            matches!(t, Token::Eof | Token::Keyword("end" | "else" | "elseif"))
                || (with_until && matches!(t, Token::Keyword("until"))),
        )
    }

    /// Parses a single Lua statement.
    ///
    /// Grammar:
    /// ```ebnf
    /// stat ::=  ';' |
    ///           varlist '=' explist |
    ///           functioncall |
    ///           label |
    ///           'break' |
    ///           'goto' Name |
    ///           'do' block 'end' |
    ///           'while' exp 'do' block 'end' |
    ///           'repeat' block 'until' exp |
    ///           'if' exp 'then' block {'elseif' exp 'then' block} ['else' block] 'end' |
    ///           'for' Name '=' exp ',' exp [',' exp] 'do' block 'end' |
    ///           'for' namelist 'in' explist 'do' block 'end' |
    ///           'function' funcname funcbody |
    ///           'local' function Name funcbody |
    ///           'local' attnamelist ['=' explist]
    /// retstat ::= return [explist] [‘;’]
    /// ```
    fn parse_statement(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        match self.peek_token()? {
            // stat ::= ';'
            Token::Symbol(";") => {
                self.pop_token();
                Ok(())
            }
            // stat ::= label
            Token::Symbol("::") => self.parse_labelstat(),
            // stat ::= 'break'
            Token::Keyword("break") => self.parse_breakstat(),
            // stat ::= 'goto' Name
            Token::Keyword("goto") => self.parse_gotostat(),
            // stat ::= 'if' exp 'then' block {'elseif' exp 'then' block} ['else' block] 'end'
            Token::Keyword("if") => self.parse_ifstat(),
            // stat ::= 'do' block 'end'
            Token::Keyword("do") => self.parse_dostat(),
            // stat ::= 'while' exp 'do' block 'end'
            Token::Keyword("while") => self.parse_whilestat(),
            // stat ::= 'repeat' block 'until' exp
            Token::Keyword("repeat") => self.parse_repeatstat(),
            // stat ::= 'for' Name '=' exp ',' exp [',' exp] 'do' block 'end'
            // stat ::= 'for' namelist 'in' explist 'do' block 'end'
            Token::Keyword("for") => self.parse_forstat(),
            // stat ::= 'function' funcname funcbody
            Token::Keyword("function") => self.parse_functionstat(),
            // stat ::= 'local' function Name funcbody
            // stat ::= 'local' attnamelist ['=' explist]
            Token::Keyword("local") => {
                self.pop_token();
                if matches!(self.peek_token()?, Token::Keyword("function")) {
                    self.parse_localfunctionstat()
                } else {
                    self.parse_localvarstat()
                }
            }
            // retstat ::= return [explist] [‘;’]
            Token::Keyword("return") => self.parse_retstat(),
            // stat ::= varlist '=' explist
            // stat ::= functioncall
            _ => self.parse_expressionstat(),
        }
    }

    /// Parses a label statement.
    ///
    /// Grammar:
    /// ```ebnf
    /// label ::= ‘::’ Name ‘::’
    /// ```
    fn parse_labelstat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        self.expect_symbol("::")?;
        let label = self.expect_name()?;
        self.expect_symbol("::")?;
        self.visitor.stmt_label(label);
        Ok(())
    }

    /// Parses a `break` statement.
    ///
    /// Grammar:
    /// ```ebnf
    /// stat ::=  'break'
    /// ```
    fn parse_breakstat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        self.expect_keyword("break")?;
        self.visitor.stmt_break();
        Ok(())
    }

    /// Parses a `goto` statement.
    ///
    /// Grammar:
    /// ```ebnf
    /// stat ::=  'goto' Name
    /// ```
    fn parse_gotostat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        self.expect_keyword("goto")?;
        let label = self.expect_name()?;
        self.visitor.stmt_goto(label);
        Ok(())
    }

    /// Parses a `return` statement.
    ///
    /// Grammar:
    /// ```ebnf
    /// retstat ::= return [explist] [‘;’]
    /// ```
    fn parse_retstat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        self.expect_keyword("return")?;
        let mut exprs = Vec::new();
        if !self.is_block_follow(true)? && !self.try_symbol(";")? {
            exprs.push(self.parse_expression()?);
            while self.try_symbol(",")? {
                exprs.push(self.parse_expression()?);
            }
            self.try_symbol(";")?;
        }
        self.visitor.stmt_return(exprs);
        Ok(())
    }

    /// Parses an `if` statement.
    ///
    /// Grammar:
    /// ```ebnf
    /// stat ::= 'if' exp 'then' block {'elseif' exp 'then' block} ['else' block] 'end'
    /// ```
    fn parse_ifstat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        self.expect_keyword("if")?;
        let if_pos = self.lex.position();
        self.parse_test_then_block()?;
        let mut num_ifs = 1;
        let mut elsecase = false;
        loop {
            match self.peek_token()? {
                Token::Keyword("elseif") if !elsecase => {
                    self.pop_token();
                    self.visitor.stmt_else();
                    num_ifs += 1;
                    self.parse_test_then_block()?;
                }
                Token::Keyword("else") if !elsecase => {
                    elsecase = true;
                    self.pop_token();
                    self.visitor.stmt_else();
                    self.parse_block()?;
                }
                Token::Keyword("end") => {
                    self.pop_token();
                    while num_ifs > 0 {
                        self.visitor.stmt_endif();
                        num_ifs -= 1;
                    }
                    return Ok(());
                }
                _ => self.expect_match("end", "if", if_pos)?,
            }
        }
    }

    /// Parses a test-then block, used in if and elseif statements
    /// (without the `if` or `elseif` keywords).
    ///
    /// Grammar:
    /// ```ebnf
    /// test_then_block ::= ~'if'~ exp 'then' block
    /// test_then_block ::= ~'elseif'~ exp 'then' block
    /// ```
    fn parse_test_then_block(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        let cond = self.parse_expression()?;
        self.expect_keyword("then")?;
        self.visitor.stmt_if(cond);
        self.parse_block()?;
        Ok(())
    }

    /// Parses a `do` statement.
    ///
    /// Grammar:
    /// ```ebnf
    /// stat ::= 'do' block 'end'
    /// ```
    fn parse_dostat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        self.expect_keyword("do")?;
        let do_pos = self.lex.position();
        self.visitor.stmt_do();
        self.parse_block()?;
        self.visitor.stmt_enddo();
        self.expect_match("end", "do", do_pos)?;
        Ok(())
    }

    /// Parses a `while` statement.
    ///
    /// Grammar:
    /// ```ebnf
    /// stat ::= 'while' exp 'do' block 'end'
    /// ```
    fn parse_whilestat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        self.expect_keyword("while")?;
        let while_pos = self.lex.position();
        self.visitor.stmt_loop();
        let cond = self.parse_expression()?;
        self.visitor.stmt_loop_while(cond);
        self.expect_keyword("do")?;
        self.parse_block()?;
        self.expect_match("end", "while", while_pos)?;
        self.visitor.stmt_endloop();
        Ok(())
    }

    /// Parses a `repeat` statement.
    ///
    /// Grammar:
    /// ```ebnf
    /// stat ::= 'repeat' block 'until' exp
    /// ```
    fn parse_repeatstat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        self.expect_keyword("repeat")?;
        let repeat_pos = self.lex.position();
        self.visitor.stmt_loop();
        self.visitor.enter_scope();
        self.parse_statement_list()?;
        self.expect_match("until", "repeat", repeat_pos)?;
        // evaluate condition inside inner scope
        let cond = self.parse_expression()?;
        self.visitor.stmt_loop_repeat_until(cond);
        self.visitor.leave_scope();
        self.visitor.stmt_endloop();
        Ok(())
    }

    /// Parses a `for` statement.
    ///
    /// Grammar:
    /// ```ebnf
    /// stat ::= 'for' Name '=' exp ',' exp [',' exp] 'do' block 'end'
    /// stat ::= 'for' namelist 'in' explist 'do' block 'end'
    /// ```
    fn parse_forstat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        self.expect_keyword("for")?;
        let for_pos = self.lex.position();
        let var = self.expect_name()?;
        if self.try_symbol("=")? {
            // stat ::= 'for' Name '=' exp ',' exp [',' exp] 'do' block 'end'
            self.visitor.enter_scope();
            let start = self.parse_expression()?;
            self.expect_symbol(",")?;
            let end = self.parse_expression()?;
            let mut exprs = vec![start, end];
            if self.try_symbol(",")? {
                exprs.push(self.parse_expression()?);
            }
            self.visitor.stmt_loop();
            self.visitor.stmt_loop_for(var, exprs);
            self.expect_keyword("do")?;
            self.parse_block()?;
            self.expect_match("end", "for", for_pos)?;
            self.visitor.stmt_endloop();
            self.visitor.leave_scope();
        } else {
            // stat ::= 'for' namelist 'in' explist 'do' block 'end'
            self.visitor.enter_scope();
            let mut vars = vec![var];
            while self.try_symbol(",")? {
                vars.push(self.expect_name()?);
            }
            self.expect_keyword("in")?;
            let mut exprs = vec![self.parse_expression()?];
            while self.try_symbol(",")? {
                exprs.push(self.parse_expression()?);
            }
            self.visitor.stmt_loop();
            self.visitor.stmt_loop_foreach(vars, exprs);
            self.expect_keyword("do")?;
            self.parse_block()?;
            self.expect_match("end", "for", for_pos)?;
            self.visitor.stmt_endloop();
            self.visitor.leave_scope();
        }
        Ok(())
    }

    /// Parses a `function` statement.
    ///
    /// Grammar:
    /// ```ebnf
    /// stat ::= 'function' funcname funcbody
    /// ```
    fn parse_functionstat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        self.expect_keyword("function")?;
        let func_pos = self.lex.position();
        let name = self.parse_funcname()?;
        let proto = self.parse_funcbody(func_pos, name.method)?;
        self.visitor.stmt_function(name, proto);
        Ok(())
    }

    /// Parses a local `function` statement. after the 'local' keyword.
    ///
    /// Grammar:
    /// ```ebnf
    /// stat ::= ~'local'~ function Name funcbody
    /// ```
    fn parse_localfunctionstat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        self.expect_keyword("function")?;
        let func_pos = self.lex.position();
        let name = self.expect_name()?;
        let proto = self.parse_funcbody(func_pos, false)?;
        self.visitor.stmt_local_function(name, proto);
        Ok(())
    }

    /// Parses a local definition statement. after the 'local' keyword.
    ///
    /// Grammar:
    /// ```ebnf
    /// stat ::= ~'local'~ attnamelist ['=' explist]
    /// ```
    fn parse_localvarstat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        let mut vars = vec![self.parse_nameattrib()?];
        while self.try_symbol(",")? {
            vars.push(self.parse_nameattrib()?);
        }
        if self.try_symbol("=")? {
            let mut vars = VecDeque::from(vars);
            let mut last_expr = self.parse_expression()?;
            while self.try_symbol(",")? {
                if let Some((var, attribs)) = vars.pop_front() {
                    self.visitor.stmt_local(var, attribs, last_expr);
                } else {
                    // extra expressions are evaluated but not assigned
                    self.visitor.stmt_expression(last_expr);
                }
                last_expr = self.parse_expression()?;
            }
            if vars.is_empty() {
                // extra expressions are evaluated but not assigned
                self.visitor.stmt_expression(last_expr);
            } else if vars.len() == 1 {
                let (var, attribs) = vars.pop_front().unwrap();
                self.visitor.stmt_local(var, attribs, last_expr);
            } else {
                self.visitor.stmt_locals_multi(vars.into(), last_expr);
            }
        } else {
            self.visitor.stmt_locals_uninit(vars);
        }
        Ok(())
    }

    /// Parses an expression statement.
    ///
    /// Grammar:
    /// ```ebnf
    /// stat ::= varlist '=' explist
    /// stat ::= functioncall
    /// ```
    fn parse_expressionstat(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        let expr = self.parse_prefixexpr()?;
        let token = self.peek_token()?;
        if matches!(token, Token::Symbol("=" | ",")) {
            // stat ::= varlist '=' explist
            let mut vars = vec![expr];
            while self.try_symbol(",")? {
                vars.push(self.parse_prefixexpr()?);
            }
            let mut vars = VecDeque::from(vars);
            self.expect_symbol("=")?;
            let mut last_expr = self.parse_expression()?;
            while self.try_symbol(",")? {
                if let Some(var) = vars.pop_front() {
                    self.visitor.stmt_assignment(var, last_expr);
                } else {
                    // extra expressions are evaluated but not assigned
                    self.visitor.stmt_expression(last_expr);
                }
                last_expr = self.parse_expression()?;
            }
            if vars.is_empty() {
                // extra expressions are evaluated but not assigned
                self.visitor.stmt_expression(last_expr);
            } else if vars.len() == 1 {
                let var = vars.pop_front().unwrap();
                self.visitor.stmt_assignment(var, last_expr);
            } else {
                self.visitor.stmt_assignment_multi(vars.into(), last_expr);
            }
        } else {
            self.visitor.stmt_expression(expr);
        }
        Ok(())
    }

    /// Parses a Lua expression.
    ///
    /// Grammar:
    /// ```ebnf
    /// exp ::= 'nil' |
    ///         'false' |
    ///         'true' |
    ///         Numeral |
    ///         LiteralString |
    ///         '...' |
    ///         functiondef |
    ///         prefixexp |
    ///         tableconstructor |
    ///         exp binop exp |
    ///         unop exp
    /// ```
    #[inline]
    fn parse_expression(&mut self) -> Result<V::Expr, Error<S::Error, V::Error>> {
        self.parse_expression_with_precedence(0)
    }

    fn is_ll1_expression(token: &Token) -> bool {
        matches!(
            token,
            Token::Keyword("nil" | "false" | "true" | "function")
                | Token::Symbol("..." | "{" | "(")
                | Token::Number(_)
                | Token::String(_)
                | Token::Name(_)
        ) || matches!(token, Token::Keyword(s) | Token::Symbol(s) if ast::UnaryOp::from_str(s).is_some())
    }

    /// Parses a Lua expression with operator precedence.
    ///
    /// Grammar:
    /// ```ebnf
    /// exp ::= ... (see `parse_expression`; with binop/unop precedence)
    /// ```
    fn parse_expression_with_precedence(
        &mut self,
        base_precedence: u8,
    ) -> Result<V::Expr, Error<S::Error, V::Error>> {
        self.visitor.enter_expr();

        let mut e = if let Some(op) = self.try_unary_op()? {
            // exp ::= unop exp
            let exp = self.parse_expression_with_precedence(ast::UnaryOp::PRECEDENCE_LEVEL)?;
            self.visitor.expr_prefix(op, exp)
        } else {
            // exp ::= ... (see `parse_simpleexpr`)
            self.parse_simpleexpr()?
        };

        // exp ::= exp binop exp
        while let Some((op, mut precedence)) = self.try_infix_op(base_precedence)? {
            if op.is_right_associative() {
                precedence -= 1;
            }
            let tmp = self.visitor.expr_infix(e, op);
            let rhs = self.parse_expression_with_precedence(precedence)?;
            e = self.visitor.expr_postfix(tmp, op, rhs);
        }

        self.visitor.leave_expr();

        Ok(e)
    }

    fn try_infix_op(
        &mut self,
        base_precedence: u8,
    ) -> Result<Option<(ast::InfixOp, u8)>, LexerError<S::Error>> {
        if let Token::Symbol(s) | Token::Keyword(s) = self.peek_token()? {
            let s = *s;
            if let Some(op) = ast::InfixOp::from_str(s) {
                let precedence = op.precedence_level();
                if precedence > base_precedence {
                    self.pop_token();
                    return Ok(Some((op, precedence)));
                }
            }
        }
        Ok(None)
    }

    fn try_unary_op(&mut self) -> Result<Option<ast::UnaryOp>, LexerError<S::Error>> {
        if let Token::Symbol(s) | Token::Keyword(s) = self.peek_token()? {
            let s = *s;
            if let Some(op) = ast::UnaryOp::from_str(s) {
                self.pop_token();
                return Ok(Some(op));
            }
        }
        Ok(None)
    }

    /// Parses a simple Lua expression (no operators).
    ///
    /// Grammar:
    /// ```ebnf
    /// exp ::= 'nil' |
    ///         'false' |
    ///         'true' |
    ///         Numeral |
    ///         LiteralString |
    ///         '...' |
    ///         functiondef |
    ///         prefixexp |
    ///         tableconstructor
    /// ```
    fn parse_simpleexpr(&mut self) -> Result<V::Expr, Error<S::Error, V::Error>> {
        match self.peek_token()? {
            // exp ::= nil
            Token::Keyword("nil") => {
                self.pop_token();
                Ok(self.visitor.expr_nil())
            }
            // exp ::= 'false'
            Token::Keyword("false") => {
                self.pop_token();
                Ok(self.visitor.expr_boolean(false))
            }
            // exp ::= 'true'
            Token::Keyword("true") => {
                self.pop_token();
                Ok(self.visitor.expr_boolean(true))
            }
            // exp ::= Numeral
            Token::Number(_) => {
                let n = self.expect_number()?;
                Ok(self.visitor.expr_number(n))
            }
            // exp ::= LiteralString
            Token::String(_) => {
                let s = self.expect_string()?;
                Ok(self.visitor.expr_string(s))
            }
            // exp ::= '...'
            Token::Symbol("...") => {
                self.pop_token();
                Ok(self.visitor.expr_ellipsis())
            }
            // exp ::= functiondef
            Token::Keyword("function") => {
                self.pop_token();
                let func_pos = self.lex.position();
                let proto = self.parse_funcbody(func_pos, false)?;
                Ok(self.visitor.expr_function(proto))
            }
            // exp ::= tableconstructor
            Token::Symbol("{") => self.parse_table(),
            // exp ::= prefixexp
            Token::Symbol("(") | Token::Name(_) => self.parse_prefixexpr(),
            // error branch
            t => Err(Error::ParseError(ParseError::UnexpectedToken {
                got: t.name(),
                expected: "expression",
            })),
        }
    }

    /// Parses a Lua `prefixexp`, `functioncall` and `var`
    ///
    /// Grammar:
    /// ```ebnf
    /// prefixexp ::= var |
    ///               functioncall |
    ///               '(' exp ')'
    /// functioncall ::=  prefixexp args |
    ///                   prefixexp ‘:’ Name args
    /// var ::=  Name |
    ///          prefixexp ‘[’ exp ‘]’ |
    ///          prefixexp ‘.’ Name
    /// ```
    fn parse_prefixexpr(&mut self) -> Result<V::Expr, Error<S::Error, V::Error>> {
        // Name | '(' exp ')'
        let mut e = self.parse_primaryexpr()?;
        // Suffixes: '[', '.', ':', args
        loop {
            match self.peek_token()? {
                // prefixexp ::= var
                // var ::= prefixexp ‘[’ exp ‘]’
                Token::Symbol("[") => {
                    self.pop_token();
                    let open_pos = self.lex.position();
                    let i = self.parse_expression()?;
                    self.expect_match("]", "[", open_pos)?;
                    e = self.visitor.expr_index(e, i)
                }
                // prefixexp ::= var
                // var ::= prefixexp ‘.’ Name
                Token::Symbol(".") => {
                    self.pop_token();
                    let n = self.expect_name()?;
                    e = self.visitor.expr_field(e, n)
                }
                // prefixexp ::= functioncall
                // functioncall ::= prefixexp ‘:’ Name args
                Token::Symbol(":") => {
                    self.pop_token();
                    let method = self.expect_name()?;
                    e = self.visitor.expr_self(e, method);
                    let args = self.parse_args()?;
                    e = self.visitor.expr_call(e, args, true)
                }
                // prefixexp ::= functioncall
                // functioncall ::= prefixexp args
                Token::Symbol("(" | "{") | Token::String(_) => {
                    let args = self.parse_args()?;
                    e = self.visitor.expr_call(e, args, false)
                }
                // end of prefixexp
                _ => return Ok(e),
            }
        }
    }

    /// Parses a Lua primary `prefixexp` (`Name` and `'(' exp ')'`).
    ///
    /// Grammar:
    /// ```ebnf
    /// var ::=  Name
    /// prefixexp ::= '(' exp ')'
    /// ```
    fn parse_primaryexpr(&mut self) -> Result<V::Expr, Error<S::Error, V::Error>> {
        match self.peek_token()? {
            Token::Symbol("(") => {
                self.pop_token();
                let open_pos = self.lex.position();
                let exp = self.parse_expression()?;
                self.expect_match(")", "(", open_pos)?;
                Ok(exp)
            }
            Token::Name(_) => {
                let n = self.expect_name()?;
                Ok(self.visitor.expr_var(n))
            }
            t => Err(Error::ParseError(ParseError::UnexpectedToken {
                got: t.name(),
                expected: "primary expression",
            })),
        }
    }

    /// Parses a Lua `tableconstructor`.
    ///
    /// Grammar:
    /// ```ebnf
    /// tableconstructor ::= '{' [fieldlist] '}'
    /// fieldlist ::= field {fieldsep field} [fieldsep]
    /// ```
    fn parse_table(&mut self) -> Result<V::Expr, Error<S::Error, V::Error>> {
        self.expect_symbol("{")?;
        let open_pos = self.lex.position();
        self.visitor.expr_table_begin();
        let mut fields = Vec::new();
        while !self.try_symbol("}")? {
            // fieldlist ::= field {fieldsep field} [fieldsep]
            fields.push(self.parse_field()?);
            // fieldsep ::= ',' | ';'
            if matches!(self.peek_token()?, Token::Symbol("," | ";")) {
                self.pop_token();
            } else {
                self.expect_match("}", "{", open_pos)?;
                break;
            }
        }
        let e = self.visitor.expr_table_end();
        Ok(e)
    }

    /// Parses a Lua field (table entry).
    ///
    /// Grammar:
    /// ```ebnf
    /// field ::= '[' exp ']' '=' exp |
    ///           Name '=' exp |
    ///           exp
    /// ```
    fn parse_field(&mut self) -> Result<(), Error<S::Error, V::Error>> {
        match self.peek_token()? {
            // field ::= '[' exp ']' '=' exp
            Token::Symbol("[") => {
                self.pop_token();
                let open_pos = self.lex.position();
                let e1 = self.parse_expression()?;
                self.expect_match("]", "[", open_pos)?;
                self.expect_symbol("=")?;
                let e2 = self.parse_expression()?;
                self.visitor.expr_table_field_index(e1, e2);
                return Ok(());
            }
            // field ::= Name '=' exp
            Token::Name(_) => {
                let name_token = self.pop_token();
                if self.try_symbol("=")? {
                    let Token::Name(n) = name_token else {
                        unreachable!()
                    };
                    let e2 = self.parse_expression()?;
                    self.visitor.expr_table_field_named(n, e2);
                    return Ok(());
                } else {
                    self.put_back_token(name_token);
                }
            }
            _ => {}
        }
        // field ::= exp
        let e = self.parse_expression()?;
        self.visitor.expr_table_field_exp(e);
        Ok(())
    }

    /// Parses a Lua argument list.
    ///
    /// Grammar:
    /// ```ebnf
    /// args ::= '(' [explist] ')'
    /// explist ::= exp {',' exp}
    /// ```
    fn parse_arglist(&mut self) -> Result<Vec<V::Expr>, Error<S::Error, V::Error>> {
        self.expect_symbol("(")?;
        let open_pos = self.lex.position();
        if !self.try_symbol(")")? {
            let args = self.parse_explist()?;
            self.expect_match(")", "(", open_pos)?;
            Ok(args)
        } else {
            Ok(Vec::new())
        }
    }

    /// Parses a Lua argument list.
    ///
    /// Grammar:
    /// ```ebnf
    /// explist ::= exp {',' exp}
    /// ``
    fn parse_explist(&mut self) -> Result<Vec<V::Expr>, Error<S::Error, V::Error>> {
        let mut args = Vec::new();
        args.push(self.parse_expression()?);
        while self.try_symbol(",")? {
            args.push(self.parse_expression()?);
        }
        Ok(args)
    }

    /// Parses Lua function call arguments.
    ///
    /// Grammar:
    /// ```ebnf
    /// args ::= '(' [explist] ')' | tableconstructor | LiteralString
    /// ```
    fn parse_args(&mut self) -> Result<Vec<V::Expr>, Error<S::Error, V::Error>> {
        match self.peek_token()? {
            // args ::= '(' [explist] ')'
            Token::Symbol("(") => self.parse_arglist(),
            // args ::= tableconstructor
            Token::Symbol("{") => {
                let e = self.parse_table()?;
                Ok(vec![e])
            }
            // args ::= LiteralString
            Token::String(_) => {
                let s = self.expect_string()?;
                Ok(vec![self.visitor.expr_string(s)])
            }
            // error branch
            t => Err(Error::ParseError(ParseError::UnexpectedToken {
                got: t.name(),
                expected: "arguments",
            })),
        }
    }

    /// Parses a Lua funcname.
    ///
    /// Grammar:
    /// ```ebnf
    /// funcname ::= Name {'.' Name} [':' Name]
    /// ```
    fn parse_funcname(&mut self) -> Result<ast::FuncName, Error<S::Error, V::Error>> {
        let mut qname = vec![self.expect_name()?];
        // {'.' Name}
        while self.try_symbol(".")? {
            qname.push(self.expect_name()?);
        }
        // [':' Name]
        let method = self.try_symbol(":")?;
        if method {
            qname.push(self.expect_name()?);
        }
        Ok(ast::FuncName { qname, method })
    }

    /// Parses a Lua parlist (function parameters).
    ///
    /// Grammar:
    /// ```ebnf
    /// parlist ::= namelist [',' '...'] | '...'
    /// namelist ::= Name {',' Name}
    /// ```
    fn parse_parlist(&mut self) -> Result<ast::Params, Error<S::Error, V::Error>> {
        let mut names = Vec::new();
        loop {
            match self.peek_token()? {
                // parlist ::= '...'
                Token::Symbol("...") => {
                    self.pop_token();
                    return Ok(ast::Params {
                        names,
                        variadic: true,
                    });
                }
                // parlist ::= namelist [',' '...']
                Token::Name(_) => {
                    let name = self.expect_name()?;
                    names.push(name);
                    // {',' Name}
                    if !self.try_symbol(",")? {
                        return Ok(ast::Params {
                            names,
                            variadic: false,
                        });
                    }
                }
                // end of parlist
                _ => {
                    return Ok(ast::Params {
                        names,
                        variadic: false,
                    });
                }
            }
        }
    }

    /// Parses a Lua funcbody.
    ///
    /// Grammar:
    /// ```ebnf
    /// funcbody ::= '(' [parlist] ')' block end
    /// ```
    fn parse_funcbody(
        &mut self,
        begin_pos: Position,
        is_method: bool,
    ) -> Result<V::Proto, Error<S::Error, V::Error>> {
        self.expect_symbol("(")?;
        // [parlist]
        let params = self.parse_parlist()?;
        self.expect_symbol(")")?;
        self.visitor
            .enter_function(is_method, params.variadic, params.names);
        self.parse_block()?;
        self.expect_match("end", "function", begin_pos)?;
        Ok(self.visitor.leave_function())
    }

    /// Parses a Lua name with optional attribute.
    ///
    /// Grammar:
    /// ```ebnf
    /// attnamelist ::= Name attrib {',' Name attrib}
    /// attrib ::= ['<' Name '>']
    /// ```
    fn parse_nameattrib(&mut self) -> Result<(String, String), Error<S::Error, V::Error>> {
        // Name
        let name = self.expect_name()?;
        // attrib ::= ['<' Name '>']
        let attrib = if self.try_symbol("<")? {
            let attrib = self.expect_name()?;
            self.expect_symbol(">")?;
            attrib
        } else {
            String::new()
        };
        Ok((name, attrib))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::*, lexer::Lexer, parser_ast::AstVisitor};

    #[test]
    fn expr1() {
        let mut visitor = AstVisitor::new();
        let mut lexer = Lexer::from_bytes("not a and b or c and d > -e");
        let expr = Parser::new(&mut lexer, &mut visitor).parse_expression();
        assert_eq!(
            Ok(Expression::Infix(
                Box::new(Expression::Infix(
                    Box::new(Expression::Unary(
                        UnaryOp::Not,
                        Box::new(Expression::Var("a".to_string()))
                    )),
                    InfixOp::And,
                    Box::new(Expression::Var("b".to_string()))
                )),
                InfixOp::Or,
                Box::new(Expression::Infix(
                    Box::new(Expression::Var("c".to_owned())),
                    InfixOp::And,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("d".to_owned())),
                        InfixOp::Greater,
                        Box::new(Expression::Unary(
                            UnaryOp::Minus,
                            Box::new(Expression::Var("e".to_owned()))
                        ))
                    ))
                ))
            )),
            expr
        );
    }

    #[test]
    fn expr2() {
        let mut visitor = AstVisitor::new();
        let mut lexer = Lexer::from_bytes("t1.n == (t2.n or #t2) + 1");
        let expr = Parser::new(&mut lexer, &mut visitor).parse_expression();
        assert_eq!(
            Ok(Expression::Infix(
                Box::new(Expression::Field(
                    Box::new(Expression::Var("t1".to_string())),
                    "n".to_string()
                )),
                InfixOp::Eq,
                Box::new(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Field(
                            Box::new(Expression::Var("t2".to_string())),
                            "n".to_string()
                        )),
                        InfixOp::Or,
                        Box::new(Expression::Unary(
                            UnaryOp::Len,
                            Box::new(Expression::Var("t2".to_string()))
                        ))
                    )),
                    InfixOp::Add,
                    Box::new(Expression::Number(Number::Integer(1)))
                ))
            )),
            expr
        );
    }

    #[test]
    fn stmt1() {
        let mut lexer = Lexer::from_bytes(
            "
          assert(t1.n == 1)
          for i = 2, t1.n do assert(true) end
        ",
        );
        let mut visitor = AstVisitor::new();
        let mut parser = Parser::new(&mut lexer, &mut visitor);
        let a = parser.parse_statement();
        assert_eq!(Ok(()), a);
        assert_eq!(
            Some(&Statement::Expression(Box::new(Expression::FunctCall(
                Box::new(FunctionCall {
                    prefix: Expression::Var("assert".to_string()),
                    method: String::new(),
                    args: vec![Expression::Infix(
                        Box::new(Expression::Field(
                            Box::new(Expression::Var("t1".to_string())),
                            "n".to_string()
                        )),
                        InfixOp::Eq,
                        Box::new(Expression::Number(Number::Integer(1)))
                    )],
                })
            )))),
            parser.visitor.last_stmt()
        );
        let b = parser.parse_statement();
        assert_eq!(Ok(()), b);
        assert_eq!(
            Some(&Statement::ForNum {
                var: "i".to_string(),
                exprs: vec![
                    Expression::Number(Number::Integer(2)),
                    Expression::Field(Box::new(Expression::Var("t1".to_string())), "n".to_string())
                ],
                block: vec![Statement::Expression(Box::new(Expression::FunctCall(
                    Box::new(FunctionCall {
                        prefix: Expression::Var("assert".to_string()),
                        method: String::new(),
                        args: vec![Expression::Boolean(true)]
                    })
                )))],
            }),
            parser.visitor.last_stmt()
        );
    }
}
