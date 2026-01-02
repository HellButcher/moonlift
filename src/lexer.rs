use std::{fmt, fmt::Display, io, str::Utf8Error};

use crate::{
    ast::Number,
    source::{BytesSource, ReadSource, Source},
};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum LexerError<E> {
    #[error(transparent)]
    IoError(#[from] E),
    #[error(transparent)]
    Utf8Error(Utf8Error),
    #[error("Unexpected EOF when reading {0}")]
    UnexpectedEOF(&'static str),
    #[error("Unexpected Character {0:?} when reading {1}")]
    UnexpectedCharacter(char, &'static str),
    #[error("Invalid escape sequence {0:?}")]
    InvalidEscapeSequence(char),
    #[error("Invalid number")]
    InvalidNumber,
    #[error("Unexpected Line break")]
    UnexpectedLineBreak,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Keyword(&'static str),
    Name(String),
    String(Box<[u8]>),
    Number(Number),
    Whitespace,
    Comment,
    Symbol(&'static str),
    Eof,
}

const KEYWORDS: &[&str] = &[
    // SORTED!
    "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if", "in",
    "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while",
];

impl Token {
    fn is_ignored(&self) -> bool {
        matches!(self, Self::Comment | Self::Whitespace)
    }
    pub fn name(&self) -> &'static str {
        match self {
            Self::Keyword(kw) => kw,
            Self::Symbol(s) => s,
            Self::Name(_) => "<Name>",
            Self::Number(_) => "<Number>",
            Self::String(_) => "<String>",
            Self::Whitespace => "<WS>",
            Self::Comment => "<Comment>",
            Self::Eof => "<EOF>",
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pos: usize,
    line: usize,
    line_pos: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line(), self.column())
    }
}

impl Position {
    #[inline]
    pub const fn pos(&self) -> usize {
        self.pos
    }
    #[inline]
    pub const fn line(&self) -> usize {
        self.line
    }
    #[inline]
    pub const fn line_pos(&self) -> usize {
        self.line_pos
    }
    #[inline]
    pub const fn column(&self) -> usize {
        self.pos - self.line_pos
    }
}

mod utf8 {
    // Simple more relaxed utf8 encoding functions for escape sequences.
    const TAG_CONT: u8 = 0b1000_0000;
    const TAG_TWO_B: u8 = 0b1100_0000;
    const TAG_THREE_B: u8 = 0b1110_0000;
    const TAG_FOUR_B: u8 = 0b1111_0000;
    const MAX_ONE_B: u32 = 0x80;
    const MAX_TWO_B: u32 = 0x800;
    const MAX_THREE_B: u32 = 0x10000;

    #[inline]
    #[must_use]
    const fn len_utf8(code: u32) -> usize {
        match code {
            ..MAX_ONE_B => 1,
            ..MAX_TWO_B => 2,
            ..MAX_THREE_B => 3,
            _ => 4,
        }
    }

    #[inline]
    pub fn encode_utf8_raw(code: u32, dst: &mut [u8; 4]) -> &[u8] {
        let len = len_utf8(code);
        match len {
            1 => {
                dst[0] = code as u8;
                &dst[0..1]
            }
            2 => {
                dst[0] = (code >> 6 & 0x1F) as u8 | TAG_TWO_B;
                dst[1] = (code & 0x3F) as u8 | TAG_CONT;
                &dst[0..2]
            }
            3 => {
                dst[0] = (code >> 12 & 0x0F) as u8 | TAG_THREE_B;
                dst[1] = (code >> 6 & 0x3F) as u8 | TAG_CONT;
                dst[2] = (code & 0x3F) as u8 | TAG_CONT;
                &dst[0..3]
            }
            4 => {
                dst[0] = (code >> 18 & 0x07) as u8 | TAG_FOUR_B;
                dst[1] = (code >> 12 & 0x3F) as u8 | TAG_CONT;
                dst[2] = (code >> 6 & 0x3F) as u8 | TAG_CONT;
                dst[3] = (code & 0x3F) as u8 | TAG_CONT;
                &dst[0..4]
            }
            _ => unreachable!(),
        }
    }
}

#[inline]
fn hex_digit_value(c: u8) -> Option<u8> {
    match c {
        b'0'..=b'9' => Some(c - b'0'),
        b'a'..=b'f' => Some(c - b'a' + 10),
        b'A'..=b'F' => Some(c - b'A' + 10),
        _ => None,
    }
}

pub struct Lexer<S> {
    source: S,
    line: usize,
    line_pos: usize,
    value: Vec<u8>,
}

impl<R: io::Read> Lexer<ReadSource<R>> {
    pub const fn read(read: R) -> Self {
        Self::new(ReadSource::new(read))
    }
}

impl<T: AsRef<[u8]>> Lexer<BytesSource<T>> {
    pub const fn from_bytes(data: T) -> Self {
        Self::new(BytesSource::new(data))
    }
}

pub struct NumLexer<S> {
    source: S,
    neg: bool,
}

impl<S: Source> NumLexer<S> {
    #[inline]
    pub const fn new(source: S) -> Self {
        Self { source, neg: false }
    }

    pub fn next_number(&mut self) -> Result<Number, LexerError<S::Error>> {
        let Some(c) = self.source.read_next()? else {
            return Err(LexerError::UnexpectedEOF("number"));
        };
        match c {
            b'-' => {
                if let Some(c2) = self.source.read_next()? {
                    match self.next_number_continue(c2) {
                        Ok(Number::Integer(i)) => Ok(Number::Integer(-i)),
                        Ok(Number::Float(f)) => Ok(Number::Float(-f)),
                        Err(e) => Err(e),
                    }
                } else {
                    Err(LexerError::UnexpectedEOF("number"))
                }
            }
            b'+' => {
                if let Some(c2) = self.source.read_next()? {
                    self.next_number_continue(c2)
                } else {
                    Err(LexerError::UnexpectedEOF("number"))
                }
            }
            _ => self.next_number_continue(c),
        }
    }

    fn next_number_continue(&mut self, c: u8) -> Result<Number, LexerError<S::Error>> {
        match c {
            b'.' => match self.source.read_next()? {
                Some(c @ (b'0'..=b'9')) => self.next_frac_continue(c),
                _ => Err(LexerError::UnexpectedCharacter(c as char, "number")),
            },
            b'0'..=b'9' => {
                if c == b'0' && matches!(self.source.read_next()?, Some(b'x' | b'X')) {
                    let i = self.read_hex_integer()?;
                    let mut c = self.source.read_next()?;
                    if !matches!(c, Some(b'.' | b'e' | b'E' | b'p' | b'P')) {
                        self.source.unwind();
                        return Ok(Number::Integer(i));
                    }
                    let mut f = i as f64;
                    if matches!(c, Some(b'.')) {
                        let mut div = 1f64;
                        loop {
                            c = self.source.read_next()?;
                            let Some(v) = c.and_then(hex_digit_value) else {
                                break;
                            };
                            div *= 16f64;
                            f *= 16f64;
                            f += v as f64;
                        }
                        f /= div;
                    }

                    match c {
                        Some(b'e' | b'E') => {
                            f *= 10f64.powi(self.read_exponent()?);
                        }
                        Some(b'p' | b'P') => {
                            f *= 2f64.powi(self.read_exponent()?);
                        }
                        Some(_) => self.source.unwind(),
                        None => {}
                    }

                    Ok(Number::Float(f))
                } else {
                    self.source.unwind();
                    let n = self.read_decimal_integer()?;
                    let mut c = self.source.read_next()?;
                    if !matches!(c, Some(b'.' | b'e' | b'E')) {
                        self.source.unwind();
                        return Ok(n);
                    }
                    let mut f = n.into_f64();

                    if matches!(c, Some(b'.')) {
                        let mut div = 1f64;
                        loop {
                            c = self.source.read_next()?;
                            let Some(v) = c.and_then(Self::decimal_digit_value) else {
                                break;
                            };
                            div *= 10f64;
                            f *= 10f64;
                            f += v as f64;
                        }
                        f /= div;
                    }

                    match c {
                        Some(b'e' | b'E') => {
                            f *= 10f64.powi(self.read_exponent()?);
                        }
                        Some(_) => self.source.unwind(),
                        None => {}
                    }

                    Ok(Number::Float(f))
                }
            }
            _ => Err(LexerError::UnexpectedCharacter(c as char, "number")),
        }
    }

    fn next_frac_continue(&mut self, c: u8) -> Result<Number, LexerError<S::Error>> {
        let mut div = 10f64;
        let mut f = (c - b'0') as f64;
        let mut c;
        loop {
            c = self.source.read_next()?;
            let Some(v) = c.and_then(Self::decimal_digit_value) else {
                break;
            };
            div *= 10f64;
            f *= 10f64;
            f += v as f64;
        }
        f /= div;

        match c {
            Some(b'e' | b'E') => {
                f *= 10f64.powi(self.read_exponent()?);
            }
            Some(_) => self.source.unwind(),
            None => {}
        }

        Ok(Number::Float(f))
    }

    #[inline]
    fn decimal_digit_value(c: u8) -> Option<u8> {
        match c {
            b'0'..=b'9' => Some(c - b'0'),
            _ => None,
        }
    }

    fn read_decimal_integer(&mut self) -> Result<Number, LexerError<S::Error>> {
        let mut result = 0i64;
        while let Some(v) = self.source.read_next()?.and_then(Self::decimal_digit_value) {
            if let Some(r) = result.checked_mul(10_i64) {
                if let Some(r) = r.checked_add(v as i64) {
                    result = r;
                    continue;
                }
            }
            // integer overflow: vonvert to float
            let mut result = result as f64;
            result *= 10f64;
            result += v as f64;

            while let Some(v) = self.source.read_next()?.and_then(Self::decimal_digit_value) {
                result *= 10f64;
                result += v as f64;
            }

            self.source.unwind();
            return Ok(Number::Float(result));
        }
        self.source.unwind();
        Ok(Number::Integer(result))
    }

    fn read_exponent(&mut self) -> Result<i32, LexerError<S::Error>> {
        let mut c = self.source.read_next()?;
        let negative = match c {
            Some(b'-') => {
                c = self.source.read_next()?;
                true
            }
            Some(b'+') => {
                c = self.source.read_next()?;
                false
            }
            Some(b'0'..=b'9') => false,
            Some(_) => {
                self.source.unwind();
                return Err(LexerError::InvalidNumber);
            }
            None => return Err(LexerError::UnexpectedEOF("number")),
        };
        let mut result = c
            .and_then(Self::decimal_digit_value)
            .ok_or(LexerError::InvalidNumber)? as i32;
        while let Some(v) = self.source.read_next()?.and_then(Self::decimal_digit_value) {
            result *= 10;
            result += v as i32;
        }
        self.source.unwind();
        if negative {
            result *= -1;
        }
        Ok(result)
    }

    fn read_hex_integer(&mut self) -> Result<i64, LexerError<S::Error>> {
        let mut result = 0u64;
        while let Some(v) = self.source.read_next()?.and_then(hex_digit_value) {
            result <<= 4;
            result |= v as u64;
        }
        self.source.unwind();
        Ok(result as i64)
    }
}

impl Number {
    pub fn coerce_from_string(s: &[u8]) -> Option<Self> {
        NumLexer::new(BytesSource::new(s)).next_number().ok()
    }
}

impl<S: Source> Lexer<S> {
    pub const fn new(source: S) -> Self {
        Self {
            source,
            line: 1,
            line_pos: 0,
            value: Vec::new(),
        }
    }
}

impl<S: Source> Lexer<S> {
    pub fn position(&self) -> Position {
        let pos = self.source.pos();
        Position {
            pos,
            line: self.line,
            line_pos: self.line_pos,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError<S::Error>> {
        loop {
            let t = self.next_token_ws()?;
            if !t.is_ignored() {
                return Ok(t);
            }
        }
    }

    fn next_token_ws(&mut self) -> Result<Token, LexerError<S::Error>> {
        let Some(c) = self.source.read_next()? else {
            return Ok(Token::Eof);
        };
        match c {
            b'-' => match self.source.read_next()? {
                Some(b'-') => self.read_comment(),
                _ => {
                    self.source.unwind();
                    Ok(Token::Symbol("-"))
                }
            },
            b'[' => {
                self.value.clear();
                if self.read_long_string_block()? {
                    //let s = String::from_utf8(std::mem::take(&mut self.value))
                    //    .map_err(|e| LexerError::Utf8Error(e.utf8_error()))?;
                    let s: Box<[u8]> = self.value.as_slice().into();
                    Ok(Token::String(s))
                } else {
                    Ok(Token::Symbol("["))
                }
            }
            b'"' | b'\'' => {
                self.value.clear();
                self.read_until_end_of_string(c)?;
                //let s = String::from_utf8(std::mem::take(&mut self.value))
                //    .map_err(|e| LexerError::Utf8Error(e.utf8_error()))?;
                let s: Box<[u8]> = self.value.as_slice().into();
                Ok(Token::String(s))
            }
            b':' => {
                if matches!(self.source.read_next()?, Some(b':')) {
                    Ok(Token::Symbol("::"))
                } else {
                    self.source.unwind();
                    Ok(Token::Symbol(":"))
                }
            }
            b'~' => {
                if matches!(self.source.read_next()?, Some(b'=')) {
                    Ok(Token::Symbol("~="))
                } else {
                    self.source.unwind();
                    Ok(Token::Symbol("~"))
                }
            }
            b'/' => {
                if matches!(self.source.read_next()?, Some(b'/')) {
                    Ok(Token::Symbol("//"))
                } else {
                    self.source.unwind();
                    Ok(Token::Symbol("/"))
                }
            }
            b'<' => match self.source.read_next()? {
                Some(b'<') => Ok(Token::Symbol("<<")),
                Some(b'=') => Ok(Token::Symbol("<=")),
                _ => {
                    self.source.unwind();
                    Ok(Token::Symbol("<"))
                }
            },
            b'>' => match self.source.read_next()? {
                Some(b'>') => Ok(Token::Symbol(">>")),
                Some(b'=') => Ok(Token::Symbol(">=")),
                _ => {
                    self.source.unwind();
                    Ok(Token::Symbol(">"))
                }
            },
            b'=' => {
                if matches!(self.source.read_next()?, Some(b'=')) {
                    Ok(Token::Symbol("=="))
                } else {
                    self.source.unwind();
                    Ok(Token::Symbol("="))
                }
            }
            b'.' => match self.source.read_next()? {
                Some(b'.') => {
                    if matches!(self.source.read_next()?, Some(b'.')) {
                        Ok(Token::Symbol("..."))
                    } else {
                        self.source.unwind();
                        Ok(Token::Symbol(".."))
                    }
                }
                Some(c @ (b'0'..=b'9')) => NumLexer::new(&mut self.source)
                    .next_frac_continue(c)
                    .map(Token::Number),
                _ => {
                    self.source.unwind();
                    Ok(Token::Symbol("."))
                }
            },
            b'+' => Ok(Token::Symbol("+")),
            b'*' => Ok(Token::Symbol("*")),
            b'%' => Ok(Token::Symbol("%")),
            b'^' => Ok(Token::Symbol("^")),
            b'#' => {
                if self.line == 1 && self.source.pos() == 1 {
                    // comment in first line (if file starts with '#').
                    // For she-bang `#!/usr/bin/...`
                    self.read_until_end_of_line()?;
                    return Ok(Token::Comment);
                }
                Ok(Token::Symbol("#"))
            }
            b'&' => Ok(Token::Symbol("&")),
            b'|' => Ok(Token::Symbol("|")),
            b'(' => Ok(Token::Symbol("(")),
            b')' => Ok(Token::Symbol(")")),
            b'{' => Ok(Token::Symbol("{")),
            b'}' => Ok(Token::Symbol("}")),
            b';' => Ok(Token::Symbol(";")),
            b',' => Ok(Token::Symbol(",")),
            b']' => Ok(Token::Symbol("]")),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                self.value.clear();
                self.value.push(c);
                loop {
                    match self.source.read_next()? {
                        Some(c @ (b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9')) => {
                            self.value.push(c)
                        }
                        _ => {
                            self.source.unwind();
                            break;
                        }
                    }
                }
                if let Ok(i) =
                    KEYWORDS.binary_search_by_key(&self.value.as_slice(), |k| k.as_bytes())
                {
                    return Ok(Token::Keyword(KEYWORDS[i]));
                }
                // SAFETY: is UTF-8 [a-zA-Z_0-9]
                let s = unsafe { String::from_utf8_unchecked(std::mem::take(&mut self.value)) };
                Ok(Token::Name(s))
            }
            b'0'..=b'9' => NumLexer::new(&mut self.source)
                .next_number_continue(c)
                .map(Token::Number),
            b' ' | b'\t' | b'\r' | b'\n' => {
                let mut c = c;
                loop {
                    match c {
                        b'\n' => {
                            if !matches!(self.source.read_next()?, Some(b'\r')) {
                                self.source.unwind();
                            }
                            self.line += 1;
                            self.line_pos = self.source.pos();
                        }
                        b'\r' => {
                            if !matches!(self.source.read_next()?, Some(b'\n')) {
                                self.source.unwind();
                            }
                            self.line += 1;
                            self.line_pos = self.source.pos();
                        }
                        b' ' | b'\t' => {}
                        _ => {
                            self.source.unwind();
                            break;
                        }
                    }
                    if let Some(c2) = self.source.read_next()? {
                        c = c2;
                    } else {
                        break;
                    }
                }
                Ok(Token::Whitespace)
            }
            _ => Err(LexerError::UnexpectedCharacter(c as char, "token")),
        }
    }

    fn read_comment(&mut self) -> Result<Token, LexerError<S::Error>> {
        if matches!(self.source.read_next()?, Some(b'[')) {
            let mut count = 0;
            loop {
                match self.source.read_next()? {
                    Some(b'=') => count += 1,
                    Some(b'[') => {
                        self.read_until_end_of_long_string_block::<false>(count)?;
                        return Ok(Token::Comment);
                    }
                    _ => break,
                }
            }
        }
        self.source.unwind();
        self.read_until_end_of_line()?;
        Ok(Token::Comment)
    }

    fn read_until_end_of_line(&mut self) -> Result<(), LexerError<S::Error>> {
        while let Some(c) = self.source.read_next()? {
            match c {
                b'\n' => {
                    if !matches!(self.source.read_next()?, Some(b'\r')) {
                        self.source.unwind();
                    }
                    self.line += 1;
                    self.line_pos = self.source.pos();
                    return Ok(());
                }
                b'\r' => {
                    if !matches!(self.source.read_next()?, Some(b'\n')) {
                        self.source.unwind();
                    }
                    self.line += 1;
                    self.line_pos = self.source.pos();
                    return Ok(());
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn read_long_string_block(&mut self) -> Result<bool, LexerError<S::Error>> {
        // assume, the first '[' is already read.
        let mut count = 0;
        loop {
            match self.source.read_next()? {
                Some(b'=') => count += 1,
                Some(b'[') => {
                    self.read_until_end_of_long_string_block::<true>(count)?;
                    return Ok(true);
                }
                None if count > 0 => {
                    return Err(LexerError::UnexpectedEOF("long string ([===[...]===])"))
                }
                Some(c) if count > 0 => {
                    return Err(LexerError::UnexpectedCharacter(
                        c as char,
                        "long string ([===[...]===])",
                    ))
                }
                None => return Ok(false),
                Some(_) => {
                    self.source.unwind();
                    return Ok(false);
                }
            }
        }
    }

    fn read_until_end_of_long_string_block<const FILL: bool>(
        &mut self,
        count: usize,
    ) -> Result<(), LexerError<S::Error>> {
        let mut count2 = !0;
        let mut first_nl_seen = false;
        loop {
            match self.source.read_next()? {
                None => return Err(LexerError::UnexpectedEOF("long string ([===[...]===])")),
                Some(b']') if count2 == 0 => {
                    if FILL {
                        // remove the end-marker from the value
                        self.value.truncate(self.value.len() - count - 1);
                    }
                    return Ok(());
                }
                Some(b']') => {
                    count2 = count;
                    if FILL {
                        self.value.push(b']');
                    }
                }
                Some(b'=') if count2 > 0 => {
                    count2 -= 1;
                    if FILL {
                        self.value.push(b'=');
                    }
                }
                Some(c) => {
                    count2 = !0;
                    match c {
                        b'\n' => {
                            if !matches!(self.source.read_next()?, Some(b'\r')) {
                                self.source.unwind();
                            }
                            if FILL && (!self.value.is_empty() || first_nl_seen) {
                                self.value.push(b'\n');
                            }
                            first_nl_seen = true;
                            self.line += 1;
                            self.line_pos = self.source.pos();
                        }
                        b'\r' => {
                            if !matches!(self.source.read_next()?, Some(b'\n')) {
                                self.source.unwind();
                            }
                            if FILL && (!self.value.is_empty() || first_nl_seen) {
                                self.value.push(b'\n');
                            }
                            first_nl_seen = true;
                            self.line += 1;
                            self.line_pos = self.source.pos();
                        }
                        _ => {
                            if FILL {
                                self.value.push(c);
                            }
                        }
                    }
                }
            }
        }
    }

    fn read_until_end_of_string(&mut self, until: u8) -> Result<(), LexerError<S::Error>> {
        while let Some(c) = self.source.read_next()? {
            if c == until {
                return Ok(());
            } else if c == b'\\' {
                let Some(c) = self.source.read_next()? else {
                    break;
                };
                self.value.push(match c {
                    b'a' => 0x07,
                    b'0' => 0x00,
                    b'b' => 0x08,
                    b'f' => 0x0C,
                    b'n' => b'\n',
                    b'r' => b'\r',
                    b't' => b'\t',
                    b'v' => 0x0B,
                    b'\'' => b'\'',
                    b'\"' => b'\"',
                    b'\\' => b'\\',
                    b'\n' => {
                        if !matches!(self.source.read_next()?, Some(b'\r')) {
                            self.source.unwind();
                        }
                        self.line += 1;
                        self.line_pos = self.source.pos();
                        b'\n'
                    }
                    b'\r' => {
                        if !matches!(self.source.read_next()?, Some(b'\n')) {
                            self.source.unwind();
                        }
                        self.line += 1;
                        self.line_pos = self.source.pos();
                        b'\n'
                    }
                    b'0'..=b'9' => {
                        // decimal (up to 3 digits)
                        let mut v = c - b'0';
                        if let Some(c @ (b'0'..=b'9')) = self.source.read_next()? {
                            v *= 10;
                            v += c - b'0';
                            if let Some(c @ (b'0'..=b'9')) = self.source.read_next()? {
                                v *= 10;
                                v += c - b'0';
                            } else {
                                self.source.unwind();
                            }
                        } else {
                            self.source.unwind();
                        }
                        v
                    }
                    b'x' => {
                        // hex
                        let Some(v) = self.source.read_next()?.and_then(hex_digit_value) else {
                            return Err(LexerError::InvalidEscapeSequence(c as char));
                        };
                        let Some(v2) = self.source.read_next()?.and_then(hex_digit_value) else {
                            return Err(LexerError::InvalidEscapeSequence(c as char));
                        };
                        v << 4 | v2
                    }
                    b'u' => {
                        // unicode as utf-8 (\u{XXX})
                        if !matches!(self.source.read_next()?, Some(b'{')) {
                            #[cfg(debug_assertions)]
                            eprintln!("Invalid unicode escape sequence: missing '{{'");
                            return Err(LexerError::InvalidEscapeSequence(c as char));
                        }
                        let mut v =
                            if let Some(v) = self.source.read_next()?.and_then(hex_digit_value) {
                                v as u32
                            } else {
                                #[cfg(debug_assertions)]
                                eprintln!("Invalid unicode escape sequence: missing hex digit");
                                return Err(LexerError::InvalidEscapeSequence(c as char));
                            };
                        for _ in 1..8 {
                            let Some(v2) = self.source.read_next()?.and_then(hex_digit_value)
                            else {
                                self.source.unwind();
                                break;
                            };
                            v <<= 4;
                            v |= v2 as u32;
                        }
                        if !matches!(self.source.read_next()?, Some(b'}')) {
                            #[cfg(debug_assertions)]
                            eprintln!("Invalid unicode escape sequence: missing '}}'");
                            return Err(LexerError::InvalidEscapeSequence(c as char));
                        }
                        let mut buf = [0u8; 4];
                        for c in utf8::encode_utf8_raw(v, &mut buf).iter().copied() {
                            self.value.push(c);
                        }
                        continue;
                    }
                    b'z' => {
                        // skip whitespaces
                        loop {
                            match self.source.read_next()? {
                                Some(b'\n') => {
                                    if !matches!(self.source.read_next()?, Some(b'\r')) {
                                        self.source.unwind();
                                    }
                                    self.line += 1;
                                    self.line_pos = self.source.pos();
                                }
                                Some(b'\r') => {
                                    if !matches!(self.source.read_next()?, Some(b'\n')) {
                                        self.source.unwind();
                                    }
                                    self.line += 1;
                                    self.line_pos = self.source.pos();
                                }
                                Some(b' ' | b'\t') => {}
                                _ => {
                                    self.source.unwind();
                                    break;
                                }
                            }
                        }
                        continue;
                    }
                    _ => return Err(LexerError::InvalidEscapeSequence(c as char)),
                });
            } else if c == b'\r' || c == b'\n' {
                return Err(LexerError::UnexpectedLineBreak);
            } else {
                self.value.push(c);
            }
        }
        Err(LexerError::UnexpectedEOF("string"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_match {
        ($p:pat $(if $c:expr)?, $e:expr) => {
            match $e {
                $p $(if $c)?=> {}
                t => panic!("expected {} but got {:?}", stringify!($p $(if $c)?), t),
            }
        };
    }

    macro_rules! assert_tokens {
        ($lexer:expr => { $($p:pat $(if $c:expr)?),+ $(,)?}) => {
            let lexer = &mut $lexer;
            $(
                assert_match!(Ok($p) $(if $c)?, lexer.next_token());
            )+
            assert_match!(Ok(Token::Eof), lexer.next_token());
        };
    }

    #[test]
    fn keywords() {
        let mut lexer = Lexer::from_bytes("and break do else a_name elseif ");
        assert_tokens!(lexer => {
            Token::Keyword("and"),
            Token::Keyword("break"),
            Token::Keyword("do"),
            Token::Keyword("else"),
            Token::Name(n) if n == "a_name",
            Token::Keyword("elseif"),
        });
    }

    #[test]
    fn symbols() {
        let mut lexer =
            Lexer::from_bytes("+-*///~====~<--Comment\n[]{}()]][[\nfoo]]<<<=<>>>=>%:::.....,.;#");
        assert_tokens!(lexer => {
            Token::Symbol("+"),
            Token::Symbol("-"),
            Token::Symbol("*"),
            Token::Symbol("//"),
            Token::Symbol("/"),
            Token::Symbol("~="),
            Token::Symbol("=="),
            Token::Symbol("="),
            Token::Symbol("~"),
            Token::Symbol("<"),
            // Token::Comment, -- ignored
            Token::Symbol("["),
            Token::Symbol("]"),
            Token::Symbol("{"),
            Token::Symbol("}"),
            Token::Symbol("("),
            Token::Symbol(")"),
            Token::Symbol("]"),
            Token::Symbol("]"),
            Token::String(s) if s.as_ref() == b"foo",
            Token::Symbol("<<"),
            Token::Symbol("<="),
            Token::Symbol("<"),
            Token::Symbol(">>"),
            Token::Symbol(">="),
            Token::Symbol(">"),
            Token::Symbol("%"),
            Token::Symbol("::"),
            Token::Symbol(":"),
            Token::Symbol("..."),
            Token::Symbol(".."),
            Token::Symbol(","),
            Token::Symbol("."),
            Token::Symbol(";"),
            Token::Symbol("#"),
        });
    }

    #[test]
    fn strings() {
        let mut lexer = Lexer::from_bytes(
            "\"str1\"\"str2\"'str3'\"with\\nescape\\\\s\\\"t\"[[\n\"long\" string]][===[long [==[ one]===][[long ]==] two]]",
        );
        assert_tokens!(lexer => {
            Token::String(s) if s.as_ref() == b"str1",
            Token::String(s) if s.as_ref() == b"str2",
            Token::String(s) if s.as_ref() == b"str3",
            Token::String(s) if s.as_ref() == b"with\nescape\\s\"t",
            Token::String(s) if s.as_ref() == b"\"long\" string",
            Token::String(s) if s.as_ref() == b"long [==[ one",
            Token::String(s) if s.as_ref() == b"long ]==] two",
        });
    }

    #[test]
    fn numbers() {
        // TODO: Floats not yet implemented
        let mut lexer =
            Lexer::from_bytes("1337/0x1337ff,0xffffffffffffffff12,28446744073709551615]");
        assert_tokens!(lexer => {
            Token::Number(Number::Integer(i)) if i == 1337,
            Token::Symbol("/"),
            Token::Number(Number::Integer(i)) if i == 0x1337ff,
            Token::Symbol(","),
            Token::Number(Number::Integer(i)) if i == 0xffffffffffffff12u64 as i64, // ind overflow
            Token::Symbol(","),
            Token::Number(Number::Float(f)) if f == 28446744073709551615f64, // int overflow (converted to float)
            Token::Symbol("]"),
        });
    }

    #[test]
    fn test_comments() {
        let mut lexer = Lexer::from_bytes(
            "and --line comment\n\n or --[[ long[==[ \n comment ]] for --[=[ \n ]] long2 ]=] local",
        );
        assert_tokens!(lexer => {
            Token::Keyword("and"),
            Token::Keyword("or"),
            Token::Keyword("for"),
            Token::Keyword("local"),
        });
    }
}
