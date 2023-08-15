use paragon_span::Span;
use paragon_token::{Token, TokenKind, IntRadix};
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Lexer<'src> {
    input: &'src str,
    curr_offset: usize,
}

#[derive(Debug, Error)]
pub enum LexError {
    #[error("unexpected character found: {ch}")]
    UnexpectedCharacterFound { ch: char, span: Span },
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<Token<'src>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.advance_token()
    }
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self { input, curr_offset: 0 }
    }

    pub fn advance_token(&mut self) -> Option<Result<Token<'src>, LexError>> {
        macro_rules! one_char_token {
            ($kind:expr) => {
                {
                    let start = self.curr_offset;
                    self.advance_offset();
                    let span = Span::new(start, self.curr_offset);
                    Some(Ok(Token::new($kind, span)))
                }
            };
        }

        macro_rules! two_char_token {
            ($kind:expr) => {
                {
                    let start = self.curr_offset;
                    self.advance_offset();
                    self.advance_offset();
                    let span = Span::new(start, self.curr_offset);
                    Some(Ok(Token::new($kind, span)))
                }
            };
        }

        self.advance_offset_while(|c| c.is_ascii_whitespace() && c != '\n');

        let curr = self.curr_char()?;
        let next = self.next_char().unwrap_or('\0');

        match (curr, next) {
            ('+', _)   => one_char_token!(TokenKind::Plus),
            ('-', _)   => one_char_token!(TokenKind::Minus),
            ('*', _)   => one_char_token!(TokenKind::Star),
            ('/', _)   => one_char_token!(TokenKind::Slash),
            ('&', '&') => two_char_token!(TokenKind::And),
            ('&', _)   => one_char_token!(TokenKind::Ampersand),
            ('|', '|') => two_char_token!(TokenKind::Or),
            ('|', _)   => one_char_token!(TokenKind::Vertical),
            ('^', _)   => one_char_token!(TokenKind::Circumflex),
            ('<', '<') => two_char_token!(TokenKind::LShift),
            ('<', '=') => two_char_token!(TokenKind::LE),
            ('<', _)   => one_char_token!(TokenKind::LT),
            ('>', '>') => two_char_token!(TokenKind::RShift),
            ('>', '=') => two_char_token!(TokenKind::GE),
            ('>', _)   => one_char_token!(TokenKind::GT),
            ('!', '=') => two_char_token!(TokenKind::NE),
            ('!', _)   => one_char_token!(TokenKind::Not),
            ('=', '=') => two_char_token!(TokenKind::EQ),
            ('~', _)   => one_char_token!(TokenKind::Tilde),

            ('\n', _)  => one_char_token!(TokenKind::Newline),

            (ch, _) if ch.is_ascii_alphabetic() || ch == '_' => {
                let start = self.curr_offset;
                self.advance_offset_while(|ch| ch.is_ascii_alphanumeric() || ch == '_');
                let ident = &self.input[start..self.curr_offset];
                let kind = TokenKind::Identifier(ident);
                Some(Ok(Token::new(kind, Span::new(start, self.curr_offset))))
            }
            ('0', 'x') => {
                let start = self.curr_offset;
                self.advance_offset();
                self.advance_offset();
                let body_start = self.curr_offset;
                self.advance_offset_while(|c| c.is_digit(16));
                let body = &self.input[body_start..self.curr_offset];
                let kind = TokenKind::Integer(body, IntRadix::Hexadecimal);
                Some(Ok(Token::new(kind, Span::new(start, self.curr_offset))))
            }
            ('$', _) => {
                let start = self.curr_offset;
                self.advance_offset();
                self.advance_offset();
                let body_start = self.curr_offset;
                self.advance_offset_while(|c| c.is_digit(16));
                let body = &self.input[body_start..self.curr_offset];
                let kind = TokenKind::Integer(body, IntRadix::Hexadecimal);
                Some(Ok(Token::new(kind, Span::new(start, self.curr_offset))))
            }
            ('0', 'b') => {
                let start = self.curr_offset;
                self.advance_offset();
                self.advance_offset();
                let body_start = self.curr_offset;
                self.advance_offset_while(|c| c.is_digit(2));
                let body = &self.input[body_start..self.curr_offset];
                let kind = TokenKind::Integer(body, IntRadix::Binary);
                Some(Ok(Token::new(kind, Span::new(start, self.curr_offset))))
            }
            ('%', ch) if ch.is_digit(2) => {
                let start = self.curr_offset;
                self.advance_offset();
                self.advance_offset();
                let body_start = self.curr_offset;
                self.advance_offset_while(|c| c.is_digit(2));
                let body = &self.input[body_start..self.curr_offset];
                let kind = TokenKind::Integer(body, IntRadix::Binary);
                Some(Ok(Token::new(kind, Span::new(start, self.curr_offset))))
            }
            ('%', _)   => one_char_token!(TokenKind::Percent),
            ('0', 'o') | ('0', 'O') => {
                let start = self.curr_offset;
                self.advance_offset();
                self.advance_offset();
                let body_start = self.curr_offset;
                self.advance_offset_while(|c| c.is_digit(8));
                let body = &self.input[body_start..self.curr_offset];
                let kind = TokenKind::Integer(body, IntRadix::Octadecimal);
                Some(Ok(Token::new(kind, Span::new(start, self.curr_offset))))
            }
            (ch, _) if ch.is_digit(10) => {
                let start = self.curr_offset;
                self.advance_offset_while(|c| c.is_digit(10));
                let body = &self.input[start..self.curr_offset];
                let kind = TokenKind::Integer(body, IntRadix::Decimal);
                Some(Ok(Token::new(kind, Span::new(start, self.curr_offset))))
            }
            (ch, _) => {
                let start = self.curr_offset;
                self.advance_offset();
                Some(Err(LexError::UnexpectedCharacterFound {
                    ch, span: Span::new(start, self.curr_offset)
                }))
            }
        }
    }
}

impl<'src> Lexer<'src> {
    fn next_offset(&self) -> Option<usize> {
        let substr = &self.input[self.curr_offset..];
        let curr_char_len = substr.chars().next()?.len_utf8();
        Some(self.curr_offset + curr_char_len)
    }

    fn curr_char(&self) -> Option<char> {
        self.input[self.curr_offset..].chars().next()
    }

    fn next_char(&self) -> Option<char> {
        self.input[self.curr_offset..].chars().nth(1)
    }
    
    fn advance_offset(&mut self) {
        self.curr_offset = if let Some(offset) = self.next_offset() {
            offset
        } else {
            self.curr_offset
        }
    }

    fn advance_offset_while<P: Fn(char) -> bool>(&mut self, predict: P) {
        while let Some(ch) = self.curr_char() {
            if predict(ch) {
                self.advance_offset();
            } else {
                break;
            }
        }
    }
}
