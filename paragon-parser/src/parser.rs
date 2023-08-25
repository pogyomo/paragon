use std::{rc::Rc, num::ParseIntError};
use paragon_ast::{
    Statement, InstructionStatement, PseudoInstructionStatement, SymbolStatement, LocalSymbol, GlobalSymbol,
    InstructionName, PseudoInstructionName, PseudoInstructionParam, Expression, InstructionParam, Accumulator,
    IndexableRegister, IndexableRegisterKind, Immediate, AbsoluteOrZeropageOrRelative, Indirect,
    UnaryExpression, UnaryOperator, UnaryOperatorKind, InfixExpression, InfixOperator, InfixOperatorKind,
    IntegerExpression, SymbolExpression, FunctionCallExpression, SurroundedExpression,
    FunctionCallExpressionName, FunctionCallExpressionParam
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
    #[error("failed to parse integer: {reason}")]
    FailedToParseInteger { span: Span, reason: ParseIntError }
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
        loop {
            // Skip blank statements
            while self.curr_token_kind_is(TokenKind::Newline) {
                self.advance_position();
            }

            if self.tokens.get(self.position).is_some() {
                stmts.push(self.parse_statement()?);
            } else {
                break;
            }
        }
        Ok(stmts)
    }
}

impl Parser {
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        if self.next_token_kind_is(TokenKind::Colon) || self.nth_token_kind_is(2, TokenKind::Colon) {
            let symbol = self.parse_symbol_statement()?;
            if matches!(self.curr_token_or_err()?.kind, TokenKind::Newline) {
                self.advance_position();
            }
            Ok(symbol.into())
        } else if self.curr_token_kind_is(TokenKind::Dot) {
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
        for param in params.iter() {
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
            _ => {
                let span = expr.span();
                return Ok(AbsoluteOrZeropageOrRelative::new(expr, None, span));
            }
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

        let span = expr.span() + stack.span();
        Ok(AbsoluteOrZeropageOrRelative::new(expr, Some(reg), span))
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

        let span = token.span() + expr.span();
        Ok(Immediate::new(expr, span))
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
        for param in params.iter() {
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
                    let span = expr.span();
                    params.push(PseudoInstructionParam::new(expr, span));
                }
            }
        }
        Ok(params)
    }
}

impl Parser {
    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_cast_expression()
    }
    
    fn parse_cast_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.curr_token_or_err()?;
        match token.kind {
            TokenKind::LT => {
                self.advance_position();
                let expr = self.parse_cast_expression()?;
                let op = UnaryOperator::new(UnaryOperatorKind::LSB, token.span());
                Ok(UnaryExpression::new(expr, op).into())
            }
            TokenKind::GT => {
                self.advance_position();
                let expr = self.parse_cast_expression()?;
                let op = UnaryOperator::new(UnaryOperatorKind::MSB, token.span());
                Ok(UnaryExpression::new(expr, op).into())
            }
            _ => self.parse_logical_or_expression(),
        }
    }

    fn parse_logical_or_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_logical_and_expression()?;
        let token = self.curr_token_or_err()?;
        match token.kind {
            TokenKind::Or => {
                self.advance_position();
                let rhs = self.parse_logical_or_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::Or, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            _ => Ok(lhs)
        }
    }

    fn parse_logical_and_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_bitwise_or_expression()?;
        let token = self.curr_token_or_err()?;
        match token.kind {
            TokenKind::And => {
                self.advance_position();
                let rhs = self.parse_logical_and_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::And, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            _ => Ok(lhs),
        }
    }

    fn parse_bitwise_or_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_bitwise_xor_expression()?;
        let token = self.curr_token_or_err()?;
        match token.kind {
            TokenKind::Vertical => {
                self.advance_position();
                let rhs = self.parse_bitwise_or_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::BitOr, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            _ => Ok(lhs)
        }
    }

    fn parse_bitwise_xor_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_bitwise_and_expression()?;
        let token = self.curr_token_or_err()?;
        match token.kind {
            TokenKind::Circumflex => {
                self.advance_position();
                let rhs = self.parse_bitwise_xor_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::BitXor, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            _ => Ok(lhs)
        }
    }

    fn parse_bitwise_and_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_equality_expression()?;
        let token = self.curr_token_or_err()?;
        match token.kind {
            TokenKind::Ampersand => {
                self.advance_position();
                let rhs = self.parse_bitwise_and_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::BitAnd, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            _ => Ok(lhs)
        }
    }

    fn parse_equality_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_relative_expression()?;
        let token = self.curr_token_or_err()?;
        match token.kind {
            TokenKind::EQ => {
                self.advance_position();
                let rhs = self.parse_equality_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::Eq, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            TokenKind::NE => {
                self.advance_position();
                let rhs = self.parse_equality_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::Ne, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            _ => Ok(lhs)
        }
    }

    fn parse_relative_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_shift_expression()?;
        let token = self.curr_token_or_err()?;
        match token.kind {
            TokenKind::LT => {
                self.advance_position();
                let rhs = self.parse_relative_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::LT, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            TokenKind::GT => {
                self.advance_position();
                let rhs = self.parse_relative_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::GT, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            TokenKind::LE => {
                self.advance_position();
                let rhs = self.parse_relative_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::LE, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            TokenKind::GE => {
                self.advance_position();
                let rhs = self.parse_relative_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::GE, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            _ => Ok(lhs)
        }
    }

    fn parse_shift_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_additive_expression()?;
        let token = self.curr_token_or_err()?;
        match token.kind {
            TokenKind::LShift => {
                self.advance_position();
                let rhs = self.parse_shift_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::Lsh, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            TokenKind::RShift => {
                self.advance_position();
                let rhs = self.parse_shift_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::Rsh, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            _ => Ok(lhs)
        }
    }

    fn parse_additive_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_multiplicative_expression()?;
        let token = self.curr_token_or_err()?;
        match token.kind {
            TokenKind::Plus => {
                self.advance_position();
                let rhs = self.parse_additive_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::Add, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            TokenKind::Minus => {
                self.advance_position();
                let rhs = self.parse_additive_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::Sub, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            _ => Ok(lhs)
        }
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_unary_expression()?;
        let token = self.curr_token_or_err()?;
        match token.kind {
            TokenKind::Star => {
                self.advance_position();
                let rhs = self.parse_multiplicative_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::Mul, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            TokenKind::Slash => {
                self.advance_position();
                let rhs = self.parse_multiplicative_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::Div, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            TokenKind::Percent => {
                self.advance_position();
                let rhs = self.parse_multiplicative_expression()?;
                let op = InfixOperator::new(InfixOperatorKind::Mod, token.span());
                Ok(InfixExpression::new(lhs, rhs, op).into())
            }
            _ => Ok(lhs)
        }
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.curr_token_or_err()?;
        match token.kind {
            TokenKind::Tilde => {
                self.advance_position();
                let expr = self.parse_unary_expression()?;
                let op = UnaryOperator::new(UnaryOperatorKind::BitNot, token.span());
                Ok(UnaryExpression::new(expr, op).into())
            }
            TokenKind::Minus => {
                self.advance_position();
                let expr = self.parse_unary_expression()?;
                let op = UnaryOperator::new(UnaryOperatorKind::Neg, token.span());
                Ok(UnaryExpression::new(expr, op).into())
            }
            TokenKind::Not => {
                self.advance_position();
                let expr = self.parse_unary_expression()?;
                let op = UnaryOperator::new(UnaryOperatorKind::Not, token.span());
                Ok(UnaryExpression::new(expr, op).into())
            }
            _ => self.parse_primary_expression(),
        }
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, ParseError> {
        match self.curr_token_or_err()?.kind {
            TokenKind::Integer(_, _) => self.parse_integer_expression().map(|v| v.into()),
            TokenKind::Identifier(_) => {
                match self.next_token_or_err()?.kind {
                    TokenKind::LParen => self.parse_function_call_expression().map(|v| v.into()),
                    _ => self.parse_symbol_expression().map(|v| v.into()),
                }
            }
            TokenKind::Dot => self.parse_symbol_expression().map(|v| v.into()),
            TokenKind::LParen => {
                let start = self.curr_token_or_err()?.span();
                let expr = self.parse_expression()?;
                let token = self.expect_token_or_err()?;
                match token.kind {
                    TokenKind::RParen => {
                        let span = start + token.span();
                        Ok(SurroundedExpression::new(expr, span).into())
                    }
                    _ => Err(ParseError::UnexpectedTokenFound {
                        span: token.span(),
                        expect: ")",
                    })
                }
            }
            _ => Err(ParseError::UnexpectedTokenFound {
                span: self.curr_token_or_err()?.span(),
                expect: "integer, identifier, . or (",
            })
        }
    }

    fn parse_integer_expression(&mut self) -> Result<IntegerExpression, ParseError> {
        let token = self.expect_token_or_err()?;
        match token.kind {
            TokenKind::Integer(ref body, radix) => {
                let value = match u16::from_str_radix(body, radix as u32) {
                    Ok(value) => value,
                    Err(reason) => return Err(ParseError::FailedToParseInteger {
                        span: token.span(),
                        reason,
                    })
                };
                Ok(IntegerExpression::new(value, token.span()))
            }
            _ => Err(ParseError::UnexpectedTokenFound {
                span: token.span(),
                expect: "integer",
            })
        }
    }

    fn parse_symbol_expression(&mut self) -> Result<SymbolExpression, ParseError> {
        let mut stack = NonEpmtyTokenStack::new(self.expect_token_or_err()?);
        let is_local = match stack.last().kind {
            TokenKind::Dot => {
                stack.push(self.expect_token_or_err()?);
                true
            }
            _ => false,
        };

        let name = match stack.last().kind {
            TokenKind::Identifier(ref name) => name.clone(),
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: stack.last().span(),
                expect: "identifier",
            })
        };

        let symbol = if is_local {
            LocalSymbol::new(name, stack.span()).into()
        } else {
            GlobalSymbol::new(name, stack.span()).into()
        };
        Ok(SymbolExpression::new(symbol))
    }

    fn parse_function_call_expression(&mut self) -> Result<FunctionCallExpression, ParseError> {
        let mut stack = NonEpmtyTokenStack::new(self.expect_token_or_err()?);
        let name = match stack.last().kind {
            TokenKind::Identifier(ref name) => {
                FunctionCallExpressionName::new(name.clone(), stack.last().span())
            }
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: stack.last().span(),
                expect: "identifier",
            })
        };

        stack.push(self.expect_token_or_err()?);
        match stack.last().kind {
            TokenKind::LParen => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                span: stack.last().span(),
                expect: "("
            })
        }

        let mut params = Vec::new();
        loop {
            stack.push(self.curr_token_or_err()?);
            match stack.last().kind {
                TokenKind::RParen => break,
                _ => {
                    stack.pop();
                    let expr = self.parse_expression()?;
                    params.push(FunctionCallExpressionParam::new(expr));
                    stack.push(self.expect_token_or_err()?);
                    match stack.last().kind {
                        TokenKind::RParen => break,
                        TokenKind::Comma => (),
                        _ => return Err(ParseError::UnexpectedTokenFound {
                            span: stack.last().span(),
                            expect: ") or ,"
                        })
                    }
                }
            }
        }

        Ok(FunctionCallExpression::new(name, params, stack.span()))
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

    fn curr_token(&self) -> Option<Rc<Token>> {
        self.tokens.get(self.position).map(|v| Rc::clone(v))
    }

    fn curr_token_or_err(&self) -> Result<Rc<Token>, ParseError> {
        self.curr_token().ok_or(ParseError::ExpectedTokenButNotFound { 
            last: self.tokens.last().unwrap().span()
        })
    }

    fn curr_token_kind_is(&self, kind: TokenKind) -> bool {
        match self.curr_token() {
            Some(tk) if tk.kind == kind => true,
            _ => false,
        }
    }

    fn next_token(&self) -> Option<Rc<Token>> {
        self.tokens.get(self.position + 1).map(|v| Rc::clone(v))
    }

    fn next_token_or_err(&self) -> Result<Rc<Token>, ParseError> {
        self.next_token().ok_or(ParseError::ExpectedTokenButNotFound {
            last: self.tokens.last().unwrap().span()
        })
    }

    fn next_token_kind_is(&self, kind: TokenKind) -> bool {
        match self.next_token() {
            Some(tk) if tk.kind == kind => true,
            _ => false,
        }
    }

    fn nth_token(&self, n: usize) -> Option<Rc<Token>> {
        self.tokens.get(self.position + n).map(|v| Rc::clone(v))
    }

    #[allow(unused)]
    fn nth_token_or_err(&self, n: usize) -> Result<Rc<Token>, ParseError> {
        self.nth_token(n).ok_or(ParseError::ExpectedTokenButNotFound {
            last: self.tokens.last().unwrap().span()
        })
    }

    fn nth_token_kind_is(&self, n: usize, kind: TokenKind) -> bool {
        match self.nth_token(n) {
            Some(tk) if tk.kind == kind => true,
            _ => false,
        }
    }

    fn advance_position(&mut self) {
        self.position += 1;
    }
}
