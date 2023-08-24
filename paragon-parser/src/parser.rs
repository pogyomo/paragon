use paragon_ast::{Statement, InstructionStatement, PseudoInstructionStatement};
use paragon_lexer::LexError;
use paragon_span::{Span, Spannable};
use paragon_token::{Token, TokenKind};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("failed to lex given file")]
    LexError { errors: Vec<LexError> },
    #[error("expected token but not found")]
    ExpectedTokenButNotFound { last: Span },
}

pub(crate) struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, position: 0 }
    }

    pub fn parse(self) -> Result<Vec<Statement>, Vec<ParseError>> {
        if self.tokens.is_empty() {
            return Ok(Vec::new());
        }
        todo!()
    }
}

impl Parser {
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        if 
            matches!(self.curr_token_or_err()?.kind, TokenKind::Dot) || 
            matches!(self.nth_token_or_err(2)?.kind, TokenKind::Dot)
        {
            Ok(self.parse_pseudo_instruction_statement()?.into())
        } else {
            Ok(self.parse_instruction_statement()?.into())
        }
    }

    fn parse_instruction_statement(&mut self) -> Result<InstructionStatement, ParseError> {
        todo!()
    }

    fn parse_pseudo_instruction_statement(&mut self) -> Result<PseudoInstructionStatement, ParseError> {
        todo!()
    }
}

impl Parser {
    fn curr_token_or_err(&self) -> Result<&Token, ParseError> {
        self.tokens.get(self.position).ok_or(ParseError::ExpectedTokenButNotFound { 
            last: self.tokens.last().unwrap().span()
        })
    }

    fn next_token_or_err(&self) -> Result<&Token, ParseError> {
        self.tokens.get(self.position + 1).ok_or(ParseError::ExpectedTokenButNotFound {
            last: self.tokens.last().unwrap().span()
        })
    }

    fn nth_token_or_err(&self, n: usize) -> Result<&Token, ParseError> {
        self.tokens.get(self.position + n).ok_or(ParseError::ExpectedTokenButNotFound {
            last: self.tokens.last().unwrap().span()
        })
    }

    fn advance_position(&mut self) {
        self.position += 1;
    }
}
