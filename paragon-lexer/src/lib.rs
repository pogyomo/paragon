use paragon_span::Span;
use paragon_token::{Token, TokenKind, IntRadix};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexError {
    #[error("unexpected character found")]
    UnexpectedCharacterFound { span: Span },
    #[error("integer body is empty")]
    EmptyIntegerBody { span: Span },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Lexer<'src> {
    input: &'src str,
    offset: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self { input, offset: 0 }
    }

    pub fn lex(mut self) -> Result<Vec<Token<'src>>, Vec<LexError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        while let Some(token) = self.advance_token() {
            match token {
                Ok(token)  => tokens.push(token),
                Err(error) => errors.push(error),
            }
        }
        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }
}

impl<'src> Lexer<'src> {
    fn advance_token(&mut self) -> Option<Result<Token<'src>, LexError>> {
        let mut comment_found = false;
        loop {
            self.skip_whitespaces();
            comment_found |= self.skip_oneline_comment();
            comment_found |= self.skip_multiline_comment();
            self.skip_whitespaces();
            if !comment_found {
                break;
            } else {
                comment_found = false;
            }
        }

        let curr = self.curr_char()?;
        let next = self.next_char().unwrap_or('\0');

        match (curr, next) {
            // Literal
            (ch, _) if ch.is_ascii_alphabetic() || ch == '_' => self.identifier(),
            ('0', 'x') | ('0', 'X')     => self.integer(IntRadix::Hexadecimal, 2),
            ('$', _)                    => self.integer(IntRadix::Hexadecimal, 1),
            (ch, _) if ch.is_digit(10)  => self.integer(IntRadix::Decimal, 0),
            ('0', 'o') | ('0', 'O')     => self.integer(IntRadix::Octadecimal, 2),
            ('0', 'b') | ('0', 'B')     => self.integer(IntRadix::Binary, 2),
            ('%', ch) if ch.is_digit(2) => self.integer(IntRadix::Binary, 1),

            // Operators
            ('+', _)   => self.one_char(TokenKind::Plus),
            ('-', _)   => self.one_char(TokenKind::Minus),
            ('*', _)   => self.one_char(TokenKind::Star),
            ('/', _)   => self.one_char(TokenKind::Slash),
            ('%', _)   => self.one_char(TokenKind::Percent),
            ('&', '&') => self.two_char(TokenKind::And),
            ('&', _)   => self.one_char(TokenKind::Ampersand),
            ('|', '|') => self.two_char(TokenKind::Or),
            ('|', _)   => self.one_char(TokenKind::Vertical),
            ('^', _)   => self.one_char(TokenKind::Circumflex),
            ('<', '<') => self.two_char(TokenKind::LShift),
            ('<', '=') => self.two_char(TokenKind::LE),
            ('<', _)   => self.one_char(TokenKind::LT),
            ('>', '>') => self.two_char(TokenKind::RShift),
            ('>', '=') => self.two_char(TokenKind::GE),
            ('>', _)   => self.one_char(TokenKind::GT),
            ('!', '=') => self.two_char(TokenKind::NE),
            ('!', _)   => self.one_char(TokenKind::Not),
            ('=', '=') => self.two_char(TokenKind::EQ),
            ('=', _)   => self.one_char(TokenKind::Equal),

            // Symbols
            ('(', _) => self.one_char(TokenKind::LParen),
            (')', _) => self.one_char(TokenKind::RParen),
            ('[', _) => self.one_char(TokenKind::LSquare),
            (']', _) => self.one_char(TokenKind::RSquare),
            (',', _) => self.one_char(TokenKind::Comma),

            // Special
            ('\n', _) => self.one_char(TokenKind::Newline),

            // Unexpected character
            _ => {
                let start = self.curr_offset();
                self.advance_offset();
                let end = self.curr_offset();
                Some(Err(LexError::UnexpectedCharacterFound {
                    span: Span::new(start, end)
                }))
            }
        }
    }

    fn skip_whitespaces(&mut self) {
        while let Some(ch) = self.curr_char() {
            if ch.is_ascii_whitespace() {
                self.advance_offset();
            } else {
                break;
            }
        }
    }

    fn skip_oneline_comment(&mut self) -> bool {
        match (self.curr_char(), self.next_char()) {
            (Some('/'), Some('/')) => {
                while let Some(ch) = self.curr_char() {
                    if ch == '\n' {
                        break;
                    } else {
                        self.advance_offset();
                    }
                }
                true
            }
            _ => false,
        }
    }

    fn skip_multiline_comment(&mut self) -> bool {
        match (self.curr_char(), self.next_char()) {
            (Some('/'), Some('*')) => {
                self.advance_offset();
                self.advance_offset();
                loop {
                    match (self.curr_char(), self.next_char()) {
                        (Some('/'), Some('*')) => {
                            self.skip_multiline_comment();
                        }
                        (Some('*'), Some('/')) => {
                            self.advance_offset();
                            self.advance_offset();
                            break;
                        }
                        _ => self.advance_offset(),
                    }
                }
                true
            }
            _ => false
        }
    }

    fn identifier(&mut self) -> Option<Result<Token<'src>, LexError>> {
        let start = self.curr_offset();
        while let Some(ch) = self.curr_char() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                self.advance_offset();
            } else {
                break;
            }
        }
        let end = self.curr_offset();
        let ident = TokenKind::Identifier(&self.input[start..end]);
        let span = Span::new(start, end);
        Some(Ok(Token::new(ident, span)))
    }

    fn integer(&mut self, radix: IntRadix, prefix_len: usize) -> Option<Result<Token<'src>, LexError>> {
        let start = self.curr_offset();
        for _ in 0..prefix_len {
            self.advance_offset();
        }
        let body_start = self.curr_offset();
        while let Some(ch) = self.curr_char() {
            if ch.is_digit(radix as u32) {
                self.advance_offset();
            } else {
                break;
            }
        }
        let end = self.curr_offset();
        let body = &self.input[body_start..end];
        if body.is_empty() {
            return Some(Err(LexError::EmptyIntegerBody {
                span: Span::new(start, end),
            }))
        }
        let kind = TokenKind::Integer(body, radix);
        let span = Span::new(start, end);
        Some(Ok(Token::new(kind, span)))
    }

    fn one_char(&mut self, kind: TokenKind<'src>) -> Option<Result<Token<'src>, LexError>> {
        let start = self.curr_offset();
        self.advance_offset();
        let end = self.curr_offset();
        Some(Ok(Token::new(kind, Span::new(start, end))))
    }

    fn two_char(&mut self, kind: TokenKind<'src>) -> Option<Result<Token<'src>, LexError>> {
        let start = self.curr_offset();
        self.advance_offset();
        self.advance_offset();
        let end = self.curr_offset();
        Some(Ok(Token::new(kind, Span::new(start, end))))
    }
}

impl<'src> Lexer<'src> {
    fn curr_offset(&self) -> usize {
        self.offset
    }

    fn next_offset(&self) -> usize {
        self.input[self.offset..]
            .char_indices()
            .nth(1)
            .map(|(offset, _)| self.offset + offset)
            .unwrap_or(self.input.len())
    }

    fn advance_offset(&mut self) {
        self.offset = self.next_offset();
    }

    fn curr_char(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn next_char(&self) -> Option<char> {
        self.input[self.offset..].chars().nth(1)
    }
}
