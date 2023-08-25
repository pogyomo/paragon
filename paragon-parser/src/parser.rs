use std::rc::Rc;
use paragon_ast::{
    Statement, InstructionStatement, PseudoInstructionStatement, SymbolStatement, LocalSymbol, GlobalSymbol,
    InstructionName, PseudoInstructionName, PseudoInstructionParam, Expression, InstructionParam, Accumulator,
    IndexableRegister, IndexableRegisterKind, Immediate, AbsoluteOrZeropageOrRelative, Indirect
};
use paragon_lexer::LexError;
use paragon_span::{Span, Spannable};
use paragon_token::{Token, TokenKind};
use thiserror::Error;
use crate::stack::NonEpmtyTokenStack;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("failed to lex given file")]
    LexError { errors: Vec<LexError> },
    #[error("expected token but not found")]
    ExpectedTokenButNotFound { last: Span },
    #[error("unexpected token found: expect {expect}")]
    UnexpectedTokenFound { span: Span, expect: &'static str },
    #[error("using both x and y register at indirect is invalid")]
    DuplicateUseOfRegisterAtIndirect { span: Span },
}

pub(crate) struct Parser {
    tokens: Vec<Rc<Token>>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().map(|v| Rc::new(v)).collect(),
            position: 0
        }
    }

    pub fn parse(mut self) -> Result<Vec<Statement>, ParseError> {
        if self.tokens.is_empty() {
            return Ok(Vec::new());
        }

        let mut stmts = Vec::new();
        while self.tokens.get(self.position).is_some() {
            stmts.push(self.parse_statement()?);
        }
        Ok(stmts)
    }
}

impl Parser {
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        if matches!(self.next_token_or_err()?.kind, TokenKind::Colon) {
            let symbol = self.parse_symbol_statement()?;
            if matches!(self.curr_token_or_err()?.kind, TokenKind::Newline) {
                self.advance_position();
            }
            Ok(symbol.into())
        } else if matches!(self.curr_token_or_err()?.kind, TokenKind::Dot) {
            let pseudo = self.parse_pseudo_instruction_statement()?;
            if matches!(self.curr_token_or_err()?.kind, TokenKind::Newline) {
                self.advance_position();
                Ok(pseudo.into())
            } else {
                Err(ParseError::UnexpectedTokenFound {
                    span: self.curr_token_or_err()?.span(),
                    expect: "newline"
                })
            }
        } else {
            let inst = self.parse_instruction_statement()?;
            if matches!(self.curr_token_or_err()?.kind, TokenKind::Newline) {
                self.advance_position();
                Ok(inst.into())
            } else {
                Err(ParseError::UnexpectedTokenFound {
                    span: self.curr_token_or_err()?.span(),
                    expect: "newline"
                })
            }
        }
    }

    fn parse_symbol_statement(&mut self) -> Result<SymbolStatement, ParseError> {
        let mut stack = NonEpmtyTokenStack::new(self.expect_token_or_err()?);
        let is_local = match stack.last().kind {
            TokenKind::Dot => {
                stack.push(self.expect_token_or_err()?);
                true
            }
            _ => false,
        };

        let name = match stack.last().kind {
            TokenKind::Identifier(ref body) => body.clone(),
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: stack.last().span(),
                expect: "identifier",
            })
        };

        stack.push(self.expect_token_or_err()?);
        match stack.last().kind {
            TokenKind::Colon => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: stack.last().span(),
                expect: ":",
            })
        }

        let symbol = if is_local {
            LocalSymbol::new(name, stack.span()).into()
        } else {
            GlobalSymbol::new(name, stack.span()).into()
        };
        Ok(SymbolStatement::new(symbol, stack.span()))
    }

    fn parse_instruction_statement(&mut self) -> Result<InstructionStatement, ParseError> {
        let name = self.parse_instruction_name()?;
        let params = self.parse_instruction_params()?;
        let mut span = name.span();
        for param in params {
            span += param.span();
        }
        Ok(InstructionStatement::new(name, params, span))
    }

    fn parse_instruction_name(&mut self) -> Result<InstructionName, ParseError> {
        let token = self.expect_token_or_err()?;
        match token.kind {
            TokenKind::Identifier(ref name) => {
                Ok(InstructionName::new(name.clone(), token.span()))
            }
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: token.span(),
                expect: "identifier",
            })
        }
    }

    fn parse_instruction_params(&mut self) -> Result<Vec<InstructionParam>, ParseError> {
        let mut params = Vec::new();
        loop {
            match self.curr_token_or_err()?.kind {
                TokenKind::Newline => break,
                _ => {
                    params.push(self.parse_instruction_param()?);
                    match self.curr_token_or_err()?.kind {
                        TokenKind::Comma => self.advance_position(),
                        _ => (),
                    }
                }
            }
        }
        Ok(params)
    }

    fn parse_instruction_param(&mut self) -> Result<InstructionParam, ParseError> {
        match self.curr_token_or_err()?.kind {
            TokenKind::A => self.parse_accumulator().map(|a| a.into()),
            TokenKind::Sharp => self.parse_immediate().map(|i| i.into()),
            TokenKind::LSquare => self.parse_indirect().map(|i| i.into()),
            _ => self.parse_absolute_or_zeropage_or_relative().map(|a| a.into()),
        }
    }

    fn parse_accumulator(&mut self) -> Result<Accumulator, ParseError> {
        let token = self.expect_token_or_err()?;
        match token.kind {
            TokenKind::A => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: token.span(),
                expect: "a",
            })
        }
        Ok(Accumulator::new(token.span()))
    }

    fn parse_absolute_or_zeropage_or_relative(&mut self) -> Result<AbsoluteOrZeropageOrRelative, ParseError> {
        let expr = self.parse_expression()?;

        let mut stack = NonEpmtyTokenStack::new(self.curr_token_or_err()?);
        match stack.last().kind {
            TokenKind::Comma => self.advance_position(),
            _ => return Ok(AbsoluteOrZeropageOrRelative::new(expr, None, expr.span())),
        }

        stack.push(self.expect_token_or_err()?);
        let reg = match stack.last().kind {
            TokenKind::X => IndexableRegister::new(IndexableRegisterKind::X, stack.last().span()),
            TokenKind::Y => IndexableRegister::new(IndexableRegisterKind::Y, stack.last().span()),
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: stack.last().span(),
                expect: "x or y"
            })
        };

        Ok(AbsoluteOrZeropageOrRelative::new(expr, Some(reg), expr.span() + stack.span()))
    }

    fn parse_immediate(&mut self) -> Result<Immediate, ParseError> {
        let token = self.expect_token_or_err()?;
        match token.kind {
            TokenKind::Sharp => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: token.span(),
                expect: "#",
            })
        }

        let expr = self.parse_expression()?;

        Ok(Immediate::new(expr, token.span() + expr.span()))
    }

    fn parse_indirect(&mut self) -> Result<Indirect, ParseError> {
        let mut stack = NonEpmtyTokenStack::new(self.expect_token_or_err()?);
        match stack.last().kind {
            TokenKind::LSquare => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: stack.last().span(),
                expect: "[",
            })
        }

        let expr = self.parse_expression()?;

        stack.push(self.expect_token_or_err()?);
        let xreg = match stack.last().kind {
            TokenKind::Comma => {
                stack.push(self.expect_token_or_err()?);
                match stack.last().kind {
                    TokenKind::X => {
                        stack.push(self.expect_token_or_err()?);
                        Some(IndexableRegister::new(IndexableRegisterKind::X, stack.last().span()))
                    }
                    _ => return Err(ParseError::UnexpectedTokenFound {
                        span: stack.last().span(),
                        expect: "x"
                    })
                }
            }
            _ => None,
        };

        match stack.last().kind {
            TokenKind::RSquare => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: stack.last().span(),
                expect: "]"
            })
        }

        stack.push(self.curr_token_or_err()?);
        let yreg = match stack.last().kind {
            TokenKind::Comma => {
                self.advance_position();
                stack.push(self.expect_token_or_err()?);
                match stack.last().kind {
                    TokenKind::Y => {
                        Some(IndexableRegister::new(IndexableRegisterKind::Y, stack.last().span()))
                    }
                    _ => return Err(ParseError::UnexpectedTokenFound {
                        span: stack.last().span(),
                        expect: "y"
                    })
                }
            }
            _ => {
                stack.pop();
                None
            }
        };

        match (xreg, yreg) {
            (Some(x), None) => Ok(Indirect::new(expr, Some(x), stack.span())),
            (None, Some(y)) => Ok(Indirect::new(expr, Some(y), stack.span())),
            (None, None) => Ok(Indirect::new(expr, None, stack.span())),
            (Some(_), Some(_)) => Err(ParseError::DuplicateUseOfRegisterAtIndirect {
                span: stack.span()
            })
        }
    }

    fn parse_pseudo_instruction_statement(&mut self) -> Result<PseudoInstructionStatement, ParseError> {
        let name = self.parse_pseudo_instruction_name()?;
        let params = self.parse_pseudo_instruction_params()?;
        let mut span = name.span();
        for param in params {
            span += param.span();
        }
        Ok(PseudoInstructionStatement::new(name, params, span))
    }

    fn parse_pseudo_instruction_name(&mut self) -> Result<PseudoInstructionName, ParseError> {
        let mut stack = NonEpmtyTokenStack::new(self.expect_token_or_err()?);
        match stack.last().kind {
            TokenKind::Dot => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: stack.last().span(),
                expect: "."
            })
        }

        stack.push(self.expect_token_or_err()?);
        match stack.last().kind {
            TokenKind::Identifier(ref name) => {
                Ok(PseudoInstructionName::new(name.clone(), stack.span()))
            }
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: stack.last().span(),
                expect: "identifier"
            })
        }
    }

    fn parse_pseudo_instruction_params(&mut self) -> Result<Vec<PseudoInstructionParam>, ParseError> {
        let mut params = Vec::new();
        loop {
            match self.curr_token_or_err()?.kind {
                TokenKind::Newline => break,
                _ => {
                    let expr = self.parse_expression()?;
                    params.push(PseudoInstructionParam::new(expr, expr.span()));
                    match self.curr_token_or_err()?.kind {
                        TokenKind::Comma => self.advance_position(),
                        _ => (),
                    }
                }
            }
        }
        Ok(params)
    }
}

impl Parser {
    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        todo!()
    }
}

impl Parser {
    fn expect_token_or_err(&mut self) -> Result<Rc<Token>, ParseError> {
        let token = self.tokens.get(self.position)
            .map(|t| Rc::clone(t))
            .ok_or(ParseError::ExpectedTokenButNotFound {
                last: self.tokens.last().unwrap().span()
            });
        self.advance_position();
        token
    }

    fn curr_token_or_err(&self) -> Result<Rc<Token>, ParseError> {
        self.tokens.get(self.position)
            .map(|t| Rc::clone(t))
            .ok_or(ParseError::ExpectedTokenButNotFound { 
                last: self.tokens.last().unwrap().span()
            })
    }

    fn next_token_or_err(&self) -> Result<Rc<Token>, ParseError> {
        self.tokens.get(self.position + 1)
            .map(|t| Rc::clone(t))
            .ok_or(ParseError::ExpectedTokenButNotFound {
                last: self.tokens.last().unwrap().span()
            })
    }

    fn nth_token_or_err(&self, n: usize) -> Result<Rc<Token>, ParseError> {
        self.tokens.get(self.position + n)
            .map(|t| Rc::clone(t))
            .ok_or(ParseError::ExpectedTokenButNotFound {
                last: self.tokens.last().unwrap().span()
            })
    }

    fn advance_position(&mut self) {
        self.position += 1;
    }
}
