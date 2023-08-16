use std::convert::Infallible;

use crate::{
    ast::*,
    lexer::{Lexer, LexerError, Source, Token},
};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError<E = Infallible> {
    #[error(transparent)]
    LexerError(#[from] LexerError<E>),
    UnexpectedTokenError(&'static str, &'static str),
}

pub struct Parser<'a, S> {
    lex: &'a mut Lexer<S>,
    last_token: Option<Token>,
}

impl<'a, S: Source> Parser<'a, S> {
    pub fn new(lex: &'a mut Lexer<S>) -> Self {
        Self {
            lex,
            last_token: None,
        }
    }

    #[inline]
    fn peek_token(&mut self) -> Result<&Token, LexerError<S::Error>> {
        if self.last_token.is_none() {
            self.last_token = Some(self.lex.next_token()?);
        }
        Ok(self.last_token.as_ref().unwrap())
    }

    fn next_token(&mut self) -> Result<Token, LexerError<S::Error>> {
        if let Some(t) = self.last_token.take() {
            Ok(t)
        } else {
            self.lex.next_token()
        }
    }
    #[inline]
    fn skip_token(&mut self) {
        self.last_token = None;
    }

    fn try_symbol(&mut self, sym: &str) -> Result<bool, LexerError<S::Error>> {
        match self.peek_token()? {
            Token::Symbol(s) if *s == sym => {
                self.skip_token();
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn try_keyword(&mut self, kw: &str) -> Result<bool, LexerError<S::Error>> {
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
            e => Err(ParseError::UnexpectedTokenError(e.name(), kw)),
        }
    }

    fn expect_symbol(&mut self, sym: &'static str) -> Result<(), ParseError<S::Error>> {
        match self.next_token()? {
            Token::Symbol(s) if s == sym => Ok(()),
            e => Err(ParseError::UnexpectedTokenError(e.name(), sym)),
        }
    }

    fn expect_name(&mut self) -> Result<String, ParseError<S::Error>> {
        match self.next_token()? {
            Token::Name(name) => Ok(name),
            e => Err(ParseError::UnexpectedTokenError(e.name(), "<Name>")),
        }
    }

    pub fn parse(&mut self) -> Result<Block, ParseError<S::Error>> {
        let block = self.parse_block()?;
        match self.next_token()? {
            Token::Eof => Ok(block),
            t => Err(ParseError::UnexpectedTokenError(
                t.name(),
                "EOF (end of file)",
            )),
        }
    }

    pub fn parse_block(&mut self) -> Result<Block, ParseError<S::Error>> {
        let mut block = Block::new();
        while let Some(s) = self.try_parse_statement()? {
            block.push(s);
        }
        if self.try_keyword("return")? {
            let mut exprs = Vec::new();
            if Self::is_ll1_expression(self.peek_token()?) {
                exprs.push(self.parse_expression()?);
                while self.try_symbol(",")? {
                    exprs.push(self.parse_expression()?);
                }
            }
            block.push(Statement::Return(exprs));
            self.try_symbol(";")?;
        }
        Ok(block)
    }

    fn try_parse_statement(&mut self) -> Result<Option<Statement>, ParseError<S::Error>> {
        loop {
            match self.peek_token()? {
                Token::Symbol(";") => {
                    self.skip_token();
                }
                Token::Symbol("::") => {
                    self.skip_token();
                    let label = self.expect_name()?;
                    self.expect_symbol("::")?;
                    return Ok(Some(Statement::Label(label)));
                }
                Token::Keyword("break") => {
                    self.skip_token();
                    // Break statement. Semantically equivalent to "goto break"
                    return Ok(Some(Statement::Break));
                }
                Token::Keyword("goto") => {
                    self.skip_token();
                    let label = self.expect_name()?;
                    return Ok(Some(Statement::Goto(label)));
                }
                Token::Keyword("if") => {
                    self.skip_token();
                    let cond = self.parse_expression()?;
                    self.expect_keyword("then")?;
                    let block = self.parse_block()?;
                    let mut ifcases = vec![(cond, block)];
                    let mut elsecase = None;
                    loop {
                        match self.peek_token()? {
                            Token::Keyword("elseif") if elsecase.is_none() => {
                                self.skip_token();
                                let cond = self.parse_expression()?;
                                self.expect_keyword("then")?;
                                let block = self.parse_block()?;
                                ifcases.push((cond, block));
                            }
                            Token::Keyword("else") if elsecase.is_none() => {
                                self.skip_token();
                                elsecase = Some(self.parse_block()?);
                            }
                            Token::Keyword("end") => {
                                self.skip_token();
                                return Ok(Some(Statement::If {
                                    ifcases,
                                    elsecase: elsecase.unwrap_or_default(),
                                }));
                            }
                            t if elsecase.is_none() => {
                                return Err(ParseError::UnexpectedTokenError(
                                    t.name(),
                                    "\"elseif\", \"else\" or \"end\"",
                                ))
                            }
                            t => return Err(ParseError::UnexpectedTokenError(t.name(), "end")),
                        }
                    }
                }
                Token::Keyword("do") => {
                    self.skip_token();
                    let block = self.parse_block()?;
                    self.expect_keyword("end")?;
                    return Ok(Some(Statement::Do(block)));
                }
                Token::Keyword("while") => {
                    self.skip_token();
                    let cond = Box::new(self.parse_expression()?);
                    self.expect_keyword("do")?;
                    let block = self.parse_block()?;
                    self.expect_keyword("end")?;
                    return Ok(Some(Statement::While { cond, block }));
                }
                Token::Keyword("repeat") => {
                    self.skip_token();
                    let block = self.parse_block()?;
                    self.expect_keyword("until")?;
                    let cond = Box::new(self.parse_expression()?);
                    return Ok(Some(Statement::Repeat { block, cond }));
                }
                Token::Keyword("for") => {
                    self.skip_token();
                    let var = self.expect_name()?;
                    if self.try_symbol("=")? {
                        let a = self.parse_expression()?;
                        self.expect_symbol(",")?;
                        let b = self.parse_expression()?;
                        let mut exprs = vec![a, b];
                        if self.try_symbol(",")? {
                            exprs.push(self.parse_expression()?);
                        }
                        self.expect_keyword("do")?;
                        let block = self.parse_block()?;
                        self.expect_keyword("end")?;
                        return Ok(Some(Statement::For { var, exprs, block }));
                    } else {
                        let mut vars = vec![var];
                        while self.try_symbol(",")? {
                            vars.push(self.expect_name()?);
                        }
                        self.expect_keyword("in")?;
                        let mut exprs = vec![self.parse_expression()?];
                        while self.try_symbol(",")? {
                            exprs.push(self.parse_expression()?);
                        }
                        self.expect_keyword("do")?;
                        let block = self.parse_block()?;
                        self.expect_keyword("end")?;
                        return Ok(Some(Statement::ForEach { vars, exprs, block }));
                    }
                }
                Token::Keyword("function") => {
                    self.skip_token();
                    let name = self.parse_funcname()?;
                    let (params, body) = self.parse_funcbody()?;
                    return Ok(Some(Statement::Function { name, params, body }));
                }
                Token::Keyword("local") => {
                    self.skip_token();
                    if matches!(self.peek_token()?, Token::Keyword("function")) {
                        self.skip_token();
                        let name = self.expect_name()?;
                        let (params, block) = self.parse_funcbody()?;
                        return Ok(Some(Statement::Local {
                            vars: vec![(name, String::new())],
                            exprs: vec![Expression::FunctDef(params, block)],
                        }));
                    } else {
                        let mut vars = vec![self.parse_nameattrib()?];
                        while self.try_symbol(",")? {
                            vars.push(self.parse_nameattrib()?);
                        }
                        let mut exprs = Vec::new();
                        if self.try_symbol("=")? {
                            exprs.push(self.parse_expression()?);
                            while self.try_symbol(",")? {
                                exprs.push(self.parse_expression()?);
                            }
                        }
                        return Ok(Some(Statement::Local { vars, exprs }));
                    }
                }
                Token::Symbol("(") | Token::Name(_) => match self.parse_prefixexpr()? {
                    Expression::FunctCall(fc) => {
                        return Ok(Some(Statement::FunctCall(fc)));
                    }
                    e if e.is_lvalue() => {
                        let mut vars = vec![e];
                        loop {
                            match self.peek_token()? {
                                Token::Symbol(",") => {
                                    self.skip_token();
                                    let e = self.parse_prefixexpr()?;
                                    if !e.is_lvalue() {
                                        return Err(ParseError::UnexpectedTokenError(
                                            "Espression / r-value",
                                            "Variable / l-value",
                                        ));
                                    } else {
                                        vars.push(e);
                                    }
                                }
                                Token::Symbol("=") => {
                                    self.skip_token();
                                    break;
                                }
                                t => {
                                    return Err(ParseError::UnexpectedTokenError(
                                        t.name(),
                                        "<Assignment>",
                                    ))
                                }
                            }
                        }
                        let e = self.parse_expression()?;
                        let mut exprs = vec![e];
                        while matches!(self.peek_token()?, Token::Symbol(",")) {
                            self.skip_token();
                            let e = self.parse_expression()?;
                            exprs.push(e);
                        }
                        return Ok(Some(Statement::Assign { vars, exprs }));
                    }
                    _ => {
                        return Err(ParseError::UnexpectedTokenError(
                            "<Expression>",
                            "<Statement>",
                        ))
                    }
                },
                _ => return Ok(None),
            }
        }
    }

    #[inline]
    fn parse_expression(&mut self) -> Result<Expression, ParseError<S::Error>> {
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
        ) || matches!(token, Token::Keyword(s) | Token::Symbol(s) if UnaryOp::from_str(s).is_some())
    }

    fn parse_expression_with_precedence(
        &mut self,
        base_precedence: u8,
    ) -> Result<Expression, ParseError<S::Error>> {
        let mut e = match self.peek_token()? {
            Token::Keyword("nil") => {
                self.skip_token();
                Expression::Nil
            }
            Token::Keyword("false") => {
                self.skip_token();
                Expression::Boolean(false)
            }
            Token::Keyword("true") => {
                self.skip_token();
                Expression::Boolean(true)
            }
            Token::Number(_) => {
                let Some(Token::Number(n)) = self.last_token.take() else { unreachable!() };
                Expression::Number(n)
            }
            Token::String(_) => {
                let Some(Token::String(s)) = self.last_token.take() else { unreachable!() };
                Expression::String(s)
            }
            Token::Symbol("...") => {
                self.skip_token();
                Expression::Ellipsis
            }
            Token::Keyword("function") => {
                self.skip_token();
                let (params, block) = self.parse_funcbody()?;
                Expression::FunctDef(params, block)
            }
            Token::Symbol("{") => {
                let fields = self.parse_table()?;
                Expression::Table(fields)
            }
            Token::Symbol("(") | Token::Name(_) => self.parse_prefixexpr()?,
            Token::Keyword(s) | Token::Symbol(s) => {
                if let Some(op) = UnaryOp::from_str(s) {
                    self.skip_token();
                    let exp = self.parse_expression_with_precedence(UnaryOp::PRECEDENCE_LEVEL)?;
                    Expression::Unary(op, Box::new(exp))
                } else {
                    return Err(ParseError::UnexpectedTokenError(s, "expression"));
                }
            }
            t => return Err(ParseError::UnexpectedTokenError(t.name(), "expression")),
        };

        while let Some((s, op, precedence)) = self.try_infix_op(base_precedence)? {
            let e2 = self.parse_expression_with_precedence(precedence)?;
            let mut operands = vec![e, e2];
            while self.try_symbol(s)? {
                operands.push(self.parse_expression_with_precedence(precedence)?);
            }
            e = Expression::Infix(op, operands);
        }
        Ok(e)
    }

    fn try_infix_op(
        &mut self,
        base_precedence: u8,
    ) -> Result<Option<(&'static str, InfixOp, u8)>, LexerError<S::Error>> {
        if let Token::Symbol(s) | Token::Keyword(s) = self.peek_token()? {
            let s = *s;
            if let Some(op) = InfixOp::from_str(s) {
                let precedence = op.precedence_level();
                if precedence > base_precedence {
                    self.skip_token();
                    return Ok(Some((s, op, precedence)));
                }
            }
        }
        Ok(None)
    }

    fn parse_prefixexpr(&mut self) -> Result<Expression, ParseError<S::Error>> {
        let mut e = match self.peek_token()? {
            Token::Symbol("(") => {
                self.skip_token();
                let exp = self.parse_expression()?;
                self.expect_symbol(")")?;
                exp
            }
            Token::Name(_) => {
                let Some(Token::Name(n)) = self.last_token.take() else { unreachable!() };
                Expression::Var(n)
            }
            t => {
                return Err(ParseError::UnexpectedTokenError(
                    t.name(),
                    "prefix expression",
                ))
            }
        };
        loop {
            match self.peek_token()? {
                Token::Symbol("[") => {
                    self.skip_token();
                    let i = self.parse_expression()?;
                    self.expect_symbol("]")?;
                    e = Expression::Index(Box::new(e), Box::new(i))
                }
                Token::Symbol(".") => {
                    self.skip_token();
                    let n = self.expect_name()?;
                    e = Expression::Field(Box::new(e), n)
                }
                Token::Symbol(":") => {
                    self.skip_token();
                    let method = self.expect_name()?;
                    let args = self.parse_args()?;
                    e = Expression::FunctCall(Box::new(FunctionCall {
                        prefix: e,
                        method,
                        args,
                    }))
                }
                Token::Symbol("(" | "{") | Token::String(_) => {
                    let args = self.parse_args()?;
                    e = Expression::FunctCall(Box::new(FunctionCall {
                        prefix: e,
                        method: String::new(),
                        args,
                    }))
                }
                _ => return Ok(e),
            }
        }
    }

    fn parse_table(&mut self) -> Result<Vec<Field>, ParseError<S::Error>> {
        self.expect_symbol("{")?;
        let mut fields = Vec::new();
        while !self.try_symbol("}")? {
            fields.push(self.parse_field()?);
            if matches!(self.peek_token()?, Token::Symbol("," | ";")) {
                self.skip_token();
            } else {
                self.expect_symbol("}")?;
                break;
            }
        }
        Ok(fields)
    }

    fn parse_field(&mut self) -> Result<Field, ParseError<S::Error>> {
        match self.peek_token()? {
            Token::Symbol("[") => {
                self.skip_token();
                let e1 = self.parse_expression()?;
                self.expect_symbol("]")?;
                self.expect_symbol("=")?;
                let e2 = self.parse_expression()?;
                return Ok(Field::Index(e1, e2));
            }
            _ => {
                let e = self.parse_expression()?;
                match e {
                    Expression::Var(name) => {
                        if self.try_symbol("=")? {
                            let e = self.parse_expression()?;
                            return Ok(Field::Named(name, e));
                        } else {
                            return Ok(Field::Exp(Expression::Var(name)));
                        }
                    }
                    e => return Ok(Field::Exp(e)),
                }
            }
        }
    }

    fn parse_arglist(&mut self) -> Result<Vec<Expression>, ParseError<S::Error>> {
        self.expect_symbol("(")?;
        let mut args = Vec::new();
        if !self.try_symbol(")")? {
            args.push(self.parse_expression()?);
            while self.try_symbol(",")? {
                args.push(self.parse_expression()?);
            }
            self.expect_symbol(")")?;
        }
        Ok(args)
    }

    fn parse_args(&mut self) -> Result<Vec<Expression>, ParseError<S::Error>> {
        match self.peek_token()? {
            Token::Symbol("(") => {
                return self.parse_arglist();
            }
            Token::Symbol("{") => {
                let fields = self.parse_table()?;
                return Ok(vec![Expression::Table(fields)]);
            }
            Token::String(_) => {
                let Some(Token::String(s)) = self.last_token.take() else { unreachable!() };
                return Ok(vec![Expression::String(s)]);
            }
            t => return Err(ParseError::UnexpectedTokenError(t.name(), "arguments")),
        }
    }

    fn parse_funcname(&mut self) -> Result<FuncName, ParseError<S::Error>> {
        let mut qname = vec![self.expect_name()?];
        while self.try_symbol(".")? {
            qname.push(self.expect_name()?);
        }
        let method = self.try_symbol(":")?;
        if method {
            qname.push(self.expect_name()?);
        }
        Ok(FuncName { qname, method })
    }

    fn parse_parlist(&mut self) -> Result<Params, ParseError<S::Error>> {
        let mut names = Vec::new();
        loop {
            match self.peek_token()? {
                Token::Symbol("...") => {
                    self.skip_token();
                    return Ok(Params {
                        names,
                        variadic: true,
                    });
                }
                Token::Name(_) => {
                    let Some(Token::Name(n)) = self.last_token.take() else {unreachable!()};
                    names.push(n);
                    if !self.try_symbol(",")? {
                        return Ok(Params {
                            names,
                            variadic: false,
                        });
                    }
                }
                _ => {
                    return Ok(Params {
                        names,
                        variadic: false,
                    });
                }
            }
        }
    }

    fn parse_funcbody(&mut self) -> Result<(Params, Block), ParseError<S::Error>> {
        self.expect_symbol("(")?;
        let params = self.parse_parlist()?;
        self.expect_symbol(")")?;
        let block = self.parse_block()?;
        self.expect_keyword("end")?;
        Ok((params, block))
    }

    fn parse_nameattrib(&mut self) -> Result<(String, String), ParseError<S::Error>> {
        let name = self.expect_name()?;
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
    use crate::lexer::Lexer;

    #[test]
    fn expr1() {
        let mut lexer = Lexer::from_bytes("not a and b or c and d > -e");
        let expr = Parser::new(&mut lexer).parse_expression();
        assert_eq!(
            Ok(Expression::Infix(
                InfixOp::Or,
                vec![
                    Expression::Infix(
                        InfixOp::And,
                        vec![
                            Expression::Unary(
                                UnaryOp::Not,
                                Box::new(Expression::Var("a".to_string()))
                            ),
                            Expression::Var("b".to_string())
                        ]
                    ),
                    Expression::Infix(
                        InfixOp::And,
                        vec![
                            Expression::Var("c".to_owned()),
                            Expression::Infix(
                                InfixOp::Greater,
                                vec![
                                    Expression::Var("d".to_owned()),
                                    Expression::Unary(
                                        UnaryOp::Minus,
                                        Box::new(Expression::Var("e".to_owned()))
                                    )
                                ]
                            )
                        ]
                    )
                ]
            )),
            expr
        );
    }

    #[test]
    fn expr2() {
        let mut lexer = Lexer::from_bytes("t1.n == (t2.n or #t2) + 1");
        let expr = Parser::new(&mut lexer).parse_expression();
        assert_eq!(
            Ok(Expression::Infix(
                InfixOp::Eq,
                vec![
                    Expression::Field(Box::new(Expression::Var("t1".to_string())), "n".to_string()),
                    Expression::Infix(
                        InfixOp::Add,
                        vec![
                            Expression::Infix(
                                InfixOp::Or,
                                vec![
                                    Expression::Field(
                                        Box::new(Expression::Var("t2".to_string())),
                                        "n".to_string()
                                    ),
                                    Expression::Unary(
                                        UnaryOp::Len,
                                        Box::new(Expression::Var("t2".to_string()))
                                    )
                                ]
                            ),
                            Expression::Number(Number::Integer(1))
                        ]
                    )
                ]
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
        let mut parser = Parser::new(&mut lexer);
        let a = parser.try_parse_statement();
        assert_eq!(
            Ok(Some(Statement::FunctCall(Box::new(FunctionCall {
                prefix: Expression::Var("assert".to_string()),
                method: String::new(),
                args: vec![Expression::Infix(
                    InfixOp::Eq,
                    vec![
                        Expression::Field(
                            Box::new(Expression::Var("t1".to_string())),
                            "n".to_string()
                        ),
                        Expression::Number(Number::Integer(1))
                    ]
                )],
            })))),
            a
        );
        let b = parser.try_parse_statement();
        assert_eq!(
            Ok(Some(Statement::For {
                var: "i".to_string(),
                exprs: vec![
                    Expression::Number(Number::Integer(2)),
                    Expression::Field(Box::new(Expression::Var("t1".to_string())), "n".to_string())
                ],
                block: vec![Statement::FunctCall(Box::new(FunctionCall {
                    prefix: Expression::Var("assert".to_string()),
                    method: String::new(),
                    args: vec![Expression::Boolean(true)]
                }))],
            })),
            b
        );
    }
}
