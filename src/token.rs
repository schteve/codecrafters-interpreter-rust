use std::fmt::Display;

#[derive(Clone)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String(String),
    Number(f64),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // Control
    Eof,
}

impl TokenType {
    pub fn matches(&self, other: &Self) -> bool {
        #[allow(clippy::match_like_matches_macro)]
        match (self, other) {
            (Self::LeftParen, Self::LeftParen) => true,
            (Self::RightParen, Self::RightParen) => true,
            (Self::LeftBrace, Self::LeftBrace) => true,
            (Self::RightBrace, Self::RightBrace) => true,
            (Self::Comma, Self::Comma) => true,
            (Self::Dot, Self::Dot) => true,
            (Self::Minus, Self::Minus) => true,
            (Self::Plus, Self::Plus) => true,
            (Self::Semicolon, Self::Semicolon) => true,
            (Self::Slash, Self::Slash) => true,
            (Self::Star, Self::Star) => true,
            (Self::Bang, Self::Bang) => true,
            (Self::BangEqual, Self::BangEqual) => true,
            (Self::Equal, Self::Equal) => true,
            (Self::EqualEqual, Self::EqualEqual) => true,
            (Self::Greater, Self::Greater) => true,
            (Self::GreaterEqual, Self::GreaterEqual) => true,
            (Self::Less, Self::Less) => true,
            (Self::LessEqual, Self::LessEqual) => true,
            (Self::Identifier, Self::Identifier) => true,
            (Self::String(_), Self::String(_)) => true,
            (Self::Number(_), Self::Number(_)) => true,
            (Self::And, Self::And) => true,
            (Self::Class, Self::Class) => true,
            (Self::Else, Self::Else) => true,
            (Self::False, Self::False) => true,
            (Self::Fun, Self::Fun) => true,
            (Self::For, Self::For) => true,
            (Self::If, Self::If) => true,
            (Self::Nil, Self::Nil) => true,
            (Self::Or, Self::Or) => true,
            (Self::Print, Self::Print) => true,
            (Self::Return, Self::Return) => true,
            (Self::Super, Self::Super) => true,
            (Self::This, Self::This) => true,
            (Self::True, Self::True) => true,
            (Self::Var, Self::Var) => true,
            (Self::While, Self::While) => true,
            (Self::Eof, Self::Eof) => true,
            _ => false,
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::LeftParen => "LEFT_PAREN",
            Self::RightParen => "RIGHT_PAREN",
            Self::LeftBrace => "LEFT_BRACE",
            Self::RightBrace => "RIGHT_BRACE",
            Self::Comma => "COMMA",
            Self::Dot => "DOT",
            Self::Minus => "MINUS",
            Self::Plus => "PLUS",
            Self::Semicolon => "SEMICOLON",
            Self::Slash => "SLASH",
            Self::Star => "STAR",
            Self::Bang => "BANG",
            Self::BangEqual => "BANG_EQUAL",
            Self::Equal => "EQUAL",
            Self::EqualEqual => "EQUAL_EQUAL",
            Self::Greater => "GREATER",
            Self::GreaterEqual => "GREATER_EQUAL",
            Self::Less => "LESS",
            Self::LessEqual => "LESS_EQUAL",
            Self::Identifier => "IDENTIFIER",
            Self::String(_) => "STRING",
            Self::Number(_) => "NUMBER",
            Self::And => "AND",
            Self::Class => "CLASS",
            Self::Else => "ELSE",
            Self::False => "FALSE",
            Self::Fun => "FUN",
            Self::For => "FOR",
            Self::If => "IF",
            Self::Nil => "NIL",
            Self::Or => "OR",
            Self::Print => "PRINT",
            Self::Return => "RETURN",
            Self::Super => "SUPER",
            Self::This => "THIS",
            Self::True => "TRUE",
            Self::Var => "VAR",
            Self::While => "WHILE",
            Self::Eof => "EOF",
        };

        write!(f, "{}", s)
    }
}

#[derive(Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    //pub line: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} ", self.ttype, self.lexeme)?;

        match &self.ttype {
            TokenType::String(s) => write!(f, "{}", s),
            TokenType::Number(n) => write!(f, "{:?}", n), // Dbg formatter needed to get trailing '.0' that is needed for some reason
            _ => write!(f, "null"),
        }
    }
}
