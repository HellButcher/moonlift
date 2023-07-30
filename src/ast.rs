#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Number {
    Integer(u64),
    Float(f64),
}

#[derive(Debug)]
pub enum Statement {
    Empty,
    Expr(Expression),
    Return(Expression),
    Break,
    Assign {
        vars: Vec<Expression>,
        exprs: Vec<Expression>,
    },
    Label(String),
    Goto(String),
    Do(Block),
    While {
        cond: Expression,
        block: Block,
    },
    Repeat {
        block: Block,
        cond: Expression,
    },
    If {
        ifcases: Vec<(Expression, Block)>,
        elsecase: Option<Block>,
    },
    For {
        var: String,
        exprs: Vec<Expression>,
        block: Block,
    },
    ForEach {
        vars: Vec<String>,
        exprs: Vec<Expression>,
        block: Block,
    },
    Function {
        name: FuncName,
        params: Params,
        body: Block,
    },
    Local {
        vars: Vec<(String, String)>,
        exprs: Vec<Expression>,
    },
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum UnaryOp {
    Not,
    BitNot,
    Minus,
    Len,
}

impl UnaryOp {
    pub const PRECEDENCE_LEVEL: u8 = 11;
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "not" => Some(Self::Not),
            "~" => Some(Self::BitNot),
            "-" => Some(Self::Minus),
            "#" => Some(Self::Len),
            _ => None,
        }
    }
    pub const fn to_str(self) -> &'static str {
        match self {
            Self::Not => "not",
            Self::BitNot => "~",
            Self::Minus => "-",
            Self::Len => "#",
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Mod,
    Pow,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    ShiftL,
    ShiftR,
    Eq,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Concat,
}

impl InfixOp {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            "//" => Some(Self::FloorDiv),
            "%" => Some(Self::Mod),
            "^" => Some(Self::Pow),
            "and" => Some(Self::And),
            "or" => Some(Self::Or),
            "&" => Some(Self::BitAnd),
            "|" => Some(Self::BitOr),
            "~" => Some(Self::BitXor),
            "<<" => Some(Self::ShiftL),
            ">>" => Some(Self::ShiftR),
            "==" => Some(Self::Eq),
            "~=" => Some(Self::NotEq),
            "<" => Some(Self::Less),
            ">" => Some(Self::Greater),
            "<=" => Some(Self::LessEq),
            ">=" => Some(Self::GreaterEq),
            ".." => Some(Self::Concat),
            _ => None,
        }
    }
    pub const fn to_str(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::FloorDiv => "//",
            Self::Mod => "%",
            Self::Pow => "^",
            Self::And => "and",
            Self::Or => "or",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "~",
            Self::ShiftL => "<<",
            Self::ShiftR => ">>",
            Self::Eq => "==",
            Self::NotEq => "~=",
            Self::Less => "<",
            Self::Greater => ">",
            Self::LessEq => "<=",
            Self::GreaterEq => ">=",
            Self::Concat => "..",
        }
    }

    pub const fn precedence_level(self) -> u8 {
        match self {
            Self::Or => 1,
            Self::And => 2,
            Self::Less
            | Self::Greater
            | Self::LessEq
            | Self::GreaterEq
            | Self::NotEq
            | Self::Eq => 3,
            Self::BitOr => 4,
            Self::BitXor => 5,
            Self::BitAnd => 6,
            Self::ShiftL | Self::ShiftR => 7,
            Self::Concat => 8,
            Self::Add | Self::Sub => 9,
            Self::Mul | Self::FloorDiv | Self::Div | Self::Mod => 10,
            // unary-operators: 11,
            Self::Pow => 12,
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Nil,
    Ellipsis,
    Boolean(bool),
    Number(Number),
    String(String),
    Var(String),
    FunctDef(Params, Block),
    FunctCall(Box<Expression>, String, Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Field(Box<Expression>, String),
    Unary(UnaryOp, Box<Expression>),
    Infix(InfixOp, Vec<Expression>),
    Table(Vec<Field>),
}

impl Expression {
    pub fn is_lvalue(&self) -> bool {
        matches!(self, Self::Var(_) | Self::Index(..) | Self::Field(..))
    }
}

#[derive(Debug)]
pub enum Field {
    Exp(Expression),
    Named(String, Expression),
    Index(Expression, Expression),
}

pub type Block = Vec<Statement>;
#[derive(Debug)]
pub struct FuncName {
    pub qname: Vec<String>,
    pub method: bool,
}

#[derive(Debug)]
pub struct Params {
    pub names: Vec<String>,
    pub variadic: bool,
}
