use crate::token::{Token, TokenType};

pub struct Scanner {
    source: Vec<char>,

    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            start: 0,
            current: 0,
            line: 0,
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

                _ => {
                    println!("Unexpected character on line {}: {c}", self.line);
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

    fn advance(&mut self) -> Option<char> {
        if self.at_end() {
            None
        } else {
            let c = self.source.get(self.current).copied();
            self.current += 1;
            c
        }
    }

    fn create_token(&self, ttype: TokenType) -> Token {
        Token {
            ttype,
            lexeme: self.source[self.start..self.current].iter().collect(),
            //line: self.line,
        }
    }
}
