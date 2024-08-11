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

                _ => {
                    self.error("Unexpected character");
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

    fn at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn peek(&self) -> Option<char> {
        if self.at_end() {
            None
        } else {
            self.source.get(self.current).copied()
        }
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
        let source: String = self.source[self.start..self.current].iter().collect();
        eprintln!("[line {}] Error: {}: {}", self.line, msg, source);
        self.had_error = true;
    }

    pub fn had_error(&self) -> bool {
        self.had_error
    }
}
