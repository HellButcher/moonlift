use std::{convert::Infallible, fmt, fmt::Display, io, str::Utf8Error};

use crate::ast::Number;

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

pub trait Source {
    type Error: std::error::Error;
    fn read_next(&mut self) -> Result<Option<u8>, Self::Error>;
    fn unwind(&mut self);
    fn pos(&self) -> usize;
}

const BUF_SIZE: usize = 512; // needs to be a power of two!
const POS_EOF: usize = !0;

pub struct ReadSource<R> {
    read: R,
    pos: usize,
    buf_pos: usize,
    buf_end: usize,
    buf: [u8; BUF_SIZE],
}

impl<R> ReadSource<R> {
    #[inline]
    pub const fn new(read: R) -> Self {
        Self {
            read,
            pos: 0,
            buf_pos: 0,
            buf_end: 0,
            buf: [0; BUF_SIZE],
        }
    }
    #[inline]
    pub fn into_inner(self) -> R {
        self.read
    }
}

impl<R: io::Read> Source for ReadSource<R> {
    type Error = io::Error;

    #[inline]
    fn read_next(&mut self) -> io::Result<Option<u8>> {
        if self.pos == POS_EOF {
            return Ok(None);
        }
        if self.buf_pos >= self.buf_end {
            // re-fill buffer
            self.buf_end = self.read.read(&mut self.buf)?;
            self.buf_pos = 0;
            if self.buf_end == 0 {
                self.pos = POS_EOF;
                return Ok(None);
            }
        }

        // SAFETY: bounds checked
        let c = unsafe { *self.buf.get_unchecked(self.buf_pos) };
        self.pos += 1;
        self.buf_pos += 1;
        return Ok(Some(c));
    }

    #[inline]
    fn unwind(&mut self) {
        if self.pos != POS_EOF {
            debug_assert_ne!(self.buf_pos, 0);
            self.pos -= 1;
            self.buf_pos -= 1;
        }
    }

    #[inline]
    fn pos(&self) -> usize {
        self.pos
    }
}

pub struct BytesSource<T> {
    data: T,
    pos: usize,
}

impl<T: AsRef<[u8]>> BytesSource<T> {
    #[inline]
    pub const fn new(data: T) -> Self {
        Self { data, pos: 0 }
    }
    #[inline]
    pub fn into_inner(self) -> T {
        self.data
    }
}

impl<T: AsRef<[u8]>> Source for BytesSource<T> {
    type Error = Infallible;

    #[inline]
    fn read_next(&mut self) -> Result<Option<u8>, Self::Error> {
        let data = self.data.as_ref();
        let len = data.len();
        if self.pos < len {
            // SAFETY: bounds checked
            let c = unsafe { *data.get_unchecked(self.pos) };
            self.pos += 1;
            Ok(Some(c))
        } else {
            self.pos = POS_EOF;
            Ok(None)
        }
    }
    #[inline]
    fn unwind(&mut self) {
        if self.pos != POS_EOF {
            debug_assert_ne!(self.pos, 0);
            self.pos -= 1;
        }
    }

    #[inline]
    fn pos(&self) -> usize {
        self.pos
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

impl<S: Source> Lexer<S> {
    pub const fn new(source: S) -> Self {
        Self {
            source,
            line: 1,
            line_pos: 0,
            value: Vec::new(),
        }
    }

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
                Some(b'-') => return self.read_comment(),
                _ => {
                    self.source.unwind();
                    return Ok(Token::Symbol("-"));
                }
            },
            b'[' => {
                self.value.clear();
                if self.read_long_string_block()? {
                    //let s = String::from_utf8(std::mem::take(&mut self.value))
                    //    .map_err(|e| LexerError::Utf8Error(e.utf8_error()))?;
                    let s: Box<[u8]> = self.value.as_slice().into();
                    return Ok(Token::String(s));
                } else {
                    return Ok(Token::Symbol("["));
                }
            }
            b'"' | b'\'' => {
                self.value.clear();
                self.read_until_end_of_string(c)?;
                //let s = String::from_utf8(std::mem::take(&mut self.value))
                //    .map_err(|e| LexerError::Utf8Error(e.utf8_error()))?;
                let s: Box<[u8]> = self.value.as_slice().into();
                return Ok(Token::String(s));
            }
            b':' => {
                if matches!(self.source.read_next()?, Some(b':')) {
                    return Ok(Token::Symbol("::"));
                } else {
                    self.source.unwind();
                    return Ok(Token::Symbol(":"));
                }
            }
            b'~' => {
                if matches!(self.source.read_next()?, Some(b'=')) {
                    return Ok(Token::Symbol("~="));
                } else {
                    self.source.unwind();
                    return Ok(Token::Symbol("~"));
                }
            }
            b'/' => {
                if matches!(self.source.read_next()?, Some(b'/')) {
                    return Ok(Token::Symbol("//"));
                } else {
                    self.source.unwind();
                    return Ok(Token::Symbol("/"));
                }
            }
            b'<' => match self.source.read_next()? {
                Some(b'<') => {
                    return Ok(Token::Symbol("<<"));
                }
                Some(b'=') => {
                    return Ok(Token::Symbol("<="));
                }
                _ => {
                    self.source.unwind();
                    return Ok(Token::Symbol("<"));
                }
            },
            b'>' => match self.source.read_next()? {
                Some(b'>') => {
                    return Ok(Token::Symbol(">>"));
                }
                Some(b'=') => {
                    return Ok(Token::Symbol(">="));
                }
                _ => {
                    self.source.unwind();
                    return Ok(Token::Symbol(">"));
                }
            },
            b'=' => {
                if matches!(self.source.read_next()?, Some(b'=')) {
                    return Ok(Token::Symbol("=="));
                } else {
                    self.source.unwind();
                    return Ok(Token::Symbol("="));
                }
            }
            b'.' => match self.source.read_next()? {
                Some(b'.') => {
                    if matches!(self.source.read_next()?, Some(b'.')) {
                        return Ok(Token::Symbol("..."));
                    } else {
                        self.source.unwind();
                        return Ok(Token::Symbol(".."));
                    }
                }
                Some(c @ (b'0'..=b'9')) => {
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

                    return Ok(Token::Number(Number::Float(f)));
                }
                _ => {
                    self.source.unwind();
                    return Ok(Token::Symbol("."));
                }
            },
            b'+' => {
                return Ok(Token::Symbol("+"));
            }
            b'*' => {
                return Ok(Token::Symbol("*"));
            }
            b'%' => {
                return Ok(Token::Symbol("%"));
            }
            b'^' => {
                return Ok(Token::Symbol("^"));
            }
            b'#' => {
                if self.line == 1 && self.source.pos() == 1 {
                    // comment in first line (if file starts with '#').
                    // For she-bang `#!/usr/bin/...`
                    self.read_until_end_of_line()?;
                    return Ok(Token::Comment);
                }
                return Ok(Token::Symbol("#"));
            }
            b'&' => {
                return Ok(Token::Symbol("&"));
            }
            b'|' => {
                return Ok(Token::Symbol("|"));
            }
            b'(' => {
                return Ok(Token::Symbol("("));
            }
            b')' => {
                return Ok(Token::Symbol(")"));
            }
            b'{' => {
                return Ok(Token::Symbol("{"));
            }
            b'}' => {
                return Ok(Token::Symbol("}"));
            }
            b';' => {
                return Ok(Token::Symbol(";"));
            }
            b',' => {
                return Ok(Token::Symbol(","));
            }
            b']' => {
                return Ok(Token::Symbol("]"));
            }
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
                    return Ok(Token::Keyword(&KEYWORDS[i]));
                }
                // SAFETY: is UTF-8 [a-zA-Z_0-9]
                let s = unsafe { String::from_utf8_unchecked(std::mem::take(&mut self.value)) };
                return Ok(Token::Name(s));
            }
            b'0'..=b'9' => {
                if c == b'0' && matches!(self.source.read_next()?, Some(b'x' | b'X')) {
                    let i = self.read_hex_integer()?;
                    let mut c = self.source.read_next()?;
                    if !matches!(c, Some(b'.' | b'e' | b'E' | b'p' | b'P')) {
                        self.source.unwind();
                        return Ok(Token::Number(Number::Integer(i)));
                    }
                    let mut f = i as f64;
                    if matches!(c, Some(b'.')) {
                        let mut div = 1f64;
                        loop {
                            c = self.source.read_next()?;
                            let Some(v) = c.and_then(Self::hex_digit_value) else {
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

                    return Ok(Token::Number(Number::Float(f)));
                } else {
                    self.source.unwind();
                    let n = self.read_decimal_integer()?;
                    let mut c = self.source.read_next()?;
                    if !matches!(c, Some(b'.' | b'e' | b'E')) {
                        self.source.unwind();
                        return Ok(Token::Number(n));
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

                    return Ok(Token::Number(Number::Float(f)));
                }
            }
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
                return Ok(Token::Whitespace);
            }
            _ => return Err(LexerError::UnexpectedCharacter(c as char, "token")),
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
        return Ok(Token::Comment);
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
                        let Some(v) = self.source.read_next()?.and_then(Self::hex_digit_value) else {
                            return Err(LexerError::InvalidEscapeSequence(c as char));
                        };
                        let Some(v2) = self.source.read_next()?.and_then(Self::hex_digit_value) else {
                            return Err(LexerError::InvalidEscapeSequence(c as char));
                        };
                        v << 4 | v2
                    }
                    b'u' => {
                        // unicode as utf-8 (\u{XXX})
                        if !matches!(self.source.read_next()?, Some(b'{')) {
                            return Err(LexerError::InvalidEscapeSequence(c as char));
                        }
                        let mut v = if let Some(v)= self.source.read_next()?.and_then(Self::hex_digit_value) {
                            v as u32
                        } else {
                            return Err(LexerError::InvalidEscapeSequence(c as char));
                        };
                        for _ in 1..8 {
                            let Some(v2) = self.source.read_next()?.and_then(Self::hex_digit_value) else {
                                self.source.unwind();
                                break;
                            };
                            v <<= 4;
                            v |= v2 as u32;
                        }
                        if !matches!(self.source.read_next()?, Some(b'}')) {
                            return Err(LexerError::InvalidEscapeSequence(c as char));
                        }
                        let Some(chr) = char::from_u32(v) else {
                            return Err(LexerError::InvalidEscapeSequence(c as char));
                        };
                        let mut buf = [0u8;4];
                        for c in chr.encode_utf8(&mut buf).as_bytes().iter().copied() {
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
            if let Some(r) = result.checked_mul(10 as i64) {
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

    #[inline]
    fn hex_digit_value(c: u8) -> Option<u8> {
        match c {
            b'0'..=b'9' => Some(c - b'0'),
            b'a'..=b'f' => Some(c - b'a' + 10),
            b'A'..=b'F' => Some(c - b'A' + 10),
            _ => None,
        }
    }

    fn read_hex_integer(&mut self) -> Result<i64, LexerError<S::Error>> {
        let mut result = 0u64;
        while let Some(v) = self.source.read_next()?.and_then(Self::hex_digit_value) {
            result <<= 4;
            result |= v as u64;
        }
        self.source.unwind();
        Ok(result as i64)
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
