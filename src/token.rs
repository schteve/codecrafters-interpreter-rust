use std::fmt::Display;

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
    // Slash,
    Star,

    // One or two character tokens
    // Bang,
    // BangEqual,
    // Equal,
    // EqualEqual,
    // Greater,
    // GreaterEqual,
    // Less,
    // LessEqual,

    // Literals
    // Identifier,
    // String,
    // Number,

    // Keywords
    // And,
    // Class,
    // Else,
    // False,
    // Fun,
    // For,
    // If,
    // Nil,
    // Or,
    // Print,
    // Return,
    // Super,
    // This,
    // True,
    // Var,
    // While,

    // Control
    Eof,
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
            // Self::Slash => "SLASH",
            Self::Star => "STAR",
            // Self::Bang => "BANG",
            // Self::BangEqual => "BANG_EQUAL",
            // Self::Equal => "EQUAL",
            // Self::EqualEqual => "EQUAL_EQUAL",
            // Self::Greater => "GREATER",
            // Self::GreaterEqual => "GREATER_EQUAL",
            // Self::Less => "LESS",
            // Self::LessEqual => "LESS_EQUAL",
            // Self::Identifier => "IDENTIFIER",
            // Self::String => "STRING",
            // Self::Number => "NUMBER",
            // Self::And => "AND",
            // Self::Class => "CLASS",
            // Self::Else => "ELSE",
            // Self::False => "FALSE",
            // Self::Fun => "FUN",
            // Self::For => "FOR",
            // Self::If => "IF",
            // Self::Nil => "NIL",
            // Self::Or => "OR",
            // Self::Print => "PRINT",
            // Self::Return => "RETURN",
            // Self::Super => "SUPER",
            // Self::This => "THIS",
            // Self::True => "TRUE",
            // Self::Var => "VAR",
            // Self::While => "WHILE",
            Self::Eof => "EOF",
        };

        write!(f, "{}", s)
    }
}

pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    //pub line: usize,
}
