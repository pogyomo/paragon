// TODO: add tests

use paragon_cache::FileId;
use paragon_token::{Token, TokenKind, IntRadix};
use paragon_span::Span;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexError {
    #[error("integer with empty body")]
    IntegerWithEmptyBody { span: Span },
    #[error("unexpected character found")]
    UnexpectedCharacterFound { span: Span },
}

pub(crate) struct Lexer {
    id: FileId,
    input: Vec<(usize, char)>,
    position: usize,
}

impl Lexer {
    pub fn new(id: FileId, input: &str) -> Self {
        Self {
            id,
            input: input.char_indices().collect(),
            position: 0,
        }
    }

    pub fn lex(mut self) -> Result<Vec<Token>, Vec<LexError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        while let Some(tk) = self.advance_token() {
            match tk {
                Ok(tk) => tokens.push(tk),
                Err(e) => errors.push(e),
            }
        }
        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }
}

impl Lexer {
    fn advance_token(&mut self) -> Option<Result<Token, LexError>> {
        let mut is_comment_skipped = false;
        loop {
            self.skip_whitespaces();
            is_comment_skipped |= self.skip_oneline_comment();
            is_comment_skipped |= self.skip_multiline_comment();
            self.skip_whitespaces();
            if !is_comment_skipped {
                break;
            } else {
                is_comment_skipped = false;
            }
        }

        let curr = self.curr_char()?;
        let next = self.next_char().unwrap_or('\0');
        match (curr, next) {
            // Literals
            (ch, _) if ch.is_ascii_alphabetic() || ch == '_' => self.identifier(),
            ('0', 'x') | ('0', 'X')      => self.integer(IntRadix::Hexadecimal, 2),
            ('$', ch) if ch.is_digit(16) => self.integer(IntRadix::Hexadecimal, 1),
            ('0', 'b') | ('0', 'B')      => self.integer(IntRadix::Binary, 2),
            ('%', ch) if ch.is_digit(2)  => self.integer(IntRadix::Binary, 2),
            ('0', 'o') | ('0', 'O')      => self.integer(IntRadix::Octadecimal, 2),
            (ch, _) if ch.is_digit(10)   => self.integer(IntRadix::Decimal, 0),

            // Operators
            ('<', '<') => self.two_char(TokenKind::LShift),
            ('>', '>') => self.two_char(TokenKind::RShift),
            ('&', '&') => self.two_char(TokenKind::And),
            ('|', '|') => self.two_char(TokenKind::Or),
            ('<', '=') => self.two_char(TokenKind::LE),
            ('>', '=') => self.two_char(TokenKind::GE),
            ('=', '=') => self.two_char(TokenKind::EQ),
            ('!', '=') => self.two_char(TokenKind::NE),
            ('+', _)   => self.one_char(TokenKind::Plus),
            ('-', _)   => self.one_char(TokenKind::Minus),
            ('*', _)   => self.one_char(TokenKind::Star),
            ('/', _)   => self.one_char(TokenKind::Slash),
            ('%', _)   => self.one_char(TokenKind::Percent),
            ('&', _)   => self.one_char(TokenKind::Ampersand),
            ('|', _)   => self.one_char(TokenKind::Vertical),
            ('^', _)   => self.one_char(TokenKind::Circumflex),
            ('!', _)   => self.one_char(TokenKind::Not),
            ('<', _)   => self.one_char(TokenKind::LT),
            ('>', _)   => self.one_char(TokenKind::GT),
            ('~', _)   => self.one_char(TokenKind::Tilde),
            ('=', _)   => self.one_char(TokenKind::Equal),

            // Symbols
            ('(', _) => self.one_char(TokenKind::LParen),
            (')', _) => self.one_char(TokenKind::RParen),
            ('[', _) => self.one_char(TokenKind::LSquare),
            (']', _) => self.one_char(TokenKind::RSquare),
            (',', _) => self.one_char(TokenKind::Comma),
            ('#', _) => self.one_char(TokenKind::Sharp),
            (':', _) => self.one_char(TokenKind::Colon),
            ('.', _) => self.one_char(TokenKind::Dot),

            // Special
            ('\n', _) => self.one_char(TokenKind::Newline),

            _ => {
                let start = self.curr_offset();
                self.advance_position();
                let end = self.curr_offset();
                Some(Err(LexError::UnexpectedCharacterFound {
                    span: Span::new(self.id, start, end)
                }))
            }
        }
    }

    fn skip_whitespaces(&mut self) {
        while let Some(ch) = self.curr_char() {
            if ch.is_ascii_whitespace() && ch != '\n' {
                self.advance_position();
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
                        self.advance_position();
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
                self.advance_position();
                self.advance_position();
                loop {
                    match (self.curr_char(), self.next_char()) {
                        (Some('/'), Some('*')) => {
                            self.skip_multiline_comment();
                        }
                        (Some('*'), Some('/')) => {
                            self.advance_position();
                            self.advance_position();
                            break;
                        }
                        (Some(_), _) => self.advance_position(),
                        (None, _) => break, // TODO: cause error?
                    }
                }
                true
            }
            _ => false, 
        }
    }

    fn identifier(&mut self) -> Option<Result<Token, LexError>> {
        let start = self.curr_offset();

        let mut body = String::new();
        while let Some(ch) = self.curr_char() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                body.push(ch);
                self.advance_position();
            } else {
                break;
            }
        }
        let end = self.curr_offset();

        let kind = TokenKind::Identifier(body);
        let span = Span::new(self.id, start, end);
        Some(Ok(Token::new(kind, span)))
    }

    fn integer(&mut self, radix: IntRadix, prefix_len: u8) -> Option<Result<Token, LexError>> {
        let start = self.curr_offset();
        for _ in 0..prefix_len {
            self.advance_position();
        }

        let mut body = String::new();
        while let Some(ch) = self.curr_char() {
            if ch.is_digit(radix as u32) {
                body.push(ch);
                self.advance_position();
            } else {
                break;
            }
        }
        let end = self.curr_offset();

        if body.is_empty() {
            return Some(Err(LexError::IntegerWithEmptyBody {
                span: Span::new(self.id, start, end)
            }))
        }

        let kind = TokenKind::Integer(body, radix);
        let span = Span::new(self.id, start, end);
        Some(Ok(Token::new(kind, span)))
    }

    fn one_char(&mut self, kind: TokenKind) -> Option<Result<Token, LexError>> {
        let start = self.curr_offset();
        self.advance_position();
        let end = self.curr_offset();
        Some(Ok(Token::new(kind, Span::new(self.id, start, end))))
    }

    fn two_char(&mut self, kind: TokenKind) -> Option<Result<Token, LexError>> {
        let start = self.curr_offset();
        self.advance_position();
        self.advance_position();
        let end = self.curr_offset();
        Some(Ok(Token::new(kind, Span::new(self.id, start, end))))
    }
}

impl Lexer {
    fn curr_offset(&self) -> usize {
        match self.input.get(self.position) {
            Some((o, _)) => *o,
            None => match self.input.last() {
                Some((o, c)) => *o + c.len_utf8(),
                None => 0,
            }
        }
    }

    fn curr_char(&self) -> Option<char> {
        self.input.get(self.position).map(|(_, c)| *c)
    }

    fn next_char(&self) -> Option<char> {
        self.input.get(self.position + 1).map(|(_, c)| *c)
    }

    fn advance_position(&mut self) {
        self.position += 1;
    }
}
