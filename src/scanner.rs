use crate::token::{Token, TokenType};

pub struct Scanner {
    source: Vec<char>,

    start: usize,
    current: usize,
    line: usize,

    had_error: bool,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
            had_error: false,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(c) = self.advance() {
            let token = match c {
                ' ' | '\t' | '\r' => None,
                '\n' => {
                    self.line += 1;
                    None
                }

                '(' => Some(self.create_token(TokenType::LeftParen)),
                ')' => Some(self.create_token(TokenType::RightParen)),
                '{' => Some(self.create_token(TokenType::LeftBrace)),
                '}' => Some(self.create_token(TokenType::RightBrace)),
                ',' => Some(self.create_token(TokenType::Comma)),
                '.' => Some(self.create_token(TokenType::Dot)),
                '-' => Some(self.create_token(TokenType::Minus)),
                '+' => Some(self.create_token(TokenType::Plus)),
                ';' => Some(self.create_token(TokenType::Semicolon)),
                '/' => {
                    if self.advance_if(|c| c == '/') {
                        while self.advance_if(|c| c != '\n') {}
                        None
                    } else {
                        Some(self.create_token(TokenType::Slash))
                    }
                }
                '*' => Some(self.create_token(TokenType::Star)),

                '!' => {
                    if self.advance_if(|c| c == '=') {
                        Some(self.create_token(TokenType::BangEqual))
                    } else {
                        Some(self.create_token(TokenType::Bang))
                    }
                }
                '=' => {
                    if self.advance_if(|c| c == '=') {
                        Some(self.create_token(TokenType::EqualEqual))
                    } else {
                        Some(self.create_token(TokenType::Equal))
                    }
                }
                '>' => {
                    if self.advance_if(|c| c == '=') {
                        Some(self.create_token(TokenType::GreaterEqual))
                    } else {
                        Some(self.create_token(TokenType::Greater))
                    }
                }
                '<' => {
                    if self.advance_if(|c| c == '=') {
                        Some(self.create_token(TokenType::LessEqual))
                    } else {
                        Some(self.create_token(TokenType::Less))
                    }
                }

                '"' => {
                    while self.advance_if(|c| c != '"') {}

                    if self.advance_if(|c| c == '"') {
                        let s: String = self.source[self.start + 1..self.current - 1]
                            .iter()
                            .collect();
                        Some(self.create_token(TokenType::String(s)))
                    } else {
                        self.error("Unterminated string.");
                        None
                    }
                }

                x if x.is_ascii_digit() => {
                    while self.advance_if(|c| c.is_ascii_digit() ) {}

                    if self.peek_nth(0) == Some('.') {
                        if let Some(c1) = self.peek_nth(1) {
                            if c1.is_ascii_digit() {
                                // Only consume dot and following digits when we're sure
                                self.advance();
                                self.advance();
                                while self.advance_if(|c| c.is_ascii_digit() ) {}
                            }
                        }
                    }

                    let s: String = self.source[self.start..self.current].iter().collect();
                    let n = s.parse().unwrap();
                    Some(self.create_token(TokenType::Number(n)))
                }

                _ => {
                    self.error_with_source("Unexpected character");
                    None
                }
            };

            if let Some(t) = token {
                tokens.push(t)
            }

            self.start = self.current;
        }

        // Add EOF
        let token = self.create_token(TokenType::Eof);
        tokens.push(token);

        tokens
    }

    fn peek(&self) -> Option<char> {
        self.peek_nth(0)
    }

    fn peek_nth(&self, nth: usize) -> Option<char> {
        let idx = self.current + nth;
        self.source.get(idx).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek();
        if c.is_some() {
            self.current += 1;
        }
        c
    }

    fn advance_if<F>(&mut self, f: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        let b = self.peek().map(f).unwrap_or(false);
        if b {
            self.current += 1;
        }
        b
    }

    fn create_token(&self, ttype: TokenType) -> Token {
        Token {
            ttype,
            lexeme: self.source[self.start..self.current].iter().collect(),
            //line: self.line,
        }
    }

    fn error(&mut self, msg: &str) {
        eprintln!("[line {}] Error: {}", self.line, msg);
        self.had_error = true;
    }

    fn error_with_source(&mut self, msg: &str) {
        let source: String = self.source[self.start..self.current].iter().collect();
        eprintln!("[line {}] Error: {}: {}", self.line, msg, source);
        self.had_error = true;
    }

    pub fn had_error(&self) -> bool {
        self.had_error
    }
}
