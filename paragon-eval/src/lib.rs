use paragon_ast::{
    Expression, InfixExpression, UnaryExpression, FunctionCallExpression, UnaryOperatorKind, Symbol, InfixOperatorKind
};
use paragon_object::{Object, ObjectKind};
use paragon_span::{Spannable, Span};
use paragon_symtable::SymbolTable;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("no such global symbol defined")]
    NoSuchGlobalSymbolDefined { span: Span },
    #[error("no such local symbol defined")]
    NoSuchLocalSymbolDefined { span: Span },
    #[error("invalid application of infix operator")]
    InvalidApplicationOfInfixOperator { span: Span },
    #[error("invalid application of unary operator: {op} cannot be applied to {ty}")]
    InvalidApplicationOfUnaryOperator { span: Span, op: &'static str, ty: &'static str },
    #[error("invalid number of argument: expect {expect}, but got {got}")]
    InvalidNumberOfArgumentForTheFunction { span: Span, expect: usize, got: usize },
    #[error("invalid tyep of function argument: expect {expect}, but got {got}")]
    InvalidTypeOfFunctionArgument { span: Span, expect: &'static str, got: &'static str },
    #[error("no such function defined")]
    NoSuchFunctionDefined { span: Span },
}

pub fn eval(expr: &Expression, symtable: &SymbolTable, global: &str) -> Result<Object, EvalError> {
    match expr {
        Expression::InfixExpression(infix) => eval_infix(infix, symtable, global),
        Expression::UnaryExpression(unary) => eval_unary(unary, symtable, global),
        Expression::IntegerExpression(int) => Ok(Object::new(ObjectKind::Word(int.value), int.span())),
        Expression::SurroundedExpression(surr) => eval(&surr.expr, symtable, global),
        Expression::FunctionCallExpression(func) => eval_func(func, symtable, global),
        Expression::SymbolExpression(sym) => match sym.symbol {
            Symbol::GlobalSymbol(ref global) => match symtable.fetch_global_value(&global.name) {
                Some(value) => Ok(value.clone()),
                None => Err(EvalError::NoSuchGlobalSymbolDefined { span: sym.span() }),
            }
            Symbol::LocalSymbol(ref local) => match symtable.fetch_local_value(global, &local.name) {
                Some(value) => Ok(value.clone()),
                None => Err(EvalError::NoSuchLocalSymbolDefined { span: sym.span() }),
            }
        }
    }
}

fn eval_infix(infix: &InfixExpression, symtable: &SymbolTable, global: &str) -> Result<Object, EvalError> {
    let lhs = eval(&infix.lhs, symtable, global)?;
    let rhs = eval(&infix.rhs, symtable, global)?;
    match infix.op.kind {
        InfixOperatorKind::Or => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                let value = if l == 0 && r == 0 { 0 } else { 1 };
                Ok(Object::new(ObjectKind::Byte(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                let value = if l == 0 && r == 0 { 0 } else { 1 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                let value = if l == 0 && r == 0 { 0 } else { 1 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                let value = if l == 0 && r == 0 { 0 } else { 1 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::And => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                let value = if l == 1 && r == 1 { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Byte(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                let value = if l == 1 && r == 1 { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                let value = if l == 1 && r == 1 { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                let value = if l == 1 && r == 1 { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::BitOr => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Byte(l | r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l | r), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l as u16 | r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Word(l | r as u16), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::BitXor => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Byte(l ^ r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l ^ r), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l as u16 ^ r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Word(l ^ r as u16), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::BitAnd => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Byte(l & r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l & r), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l as u16 & r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Word(l & r as u16), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::Eq => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                let value = if l == r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Byte(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                let value = if l == r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                let value = if l as u16 == r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                let value = if l == r as u16 { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::Ne => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                let value = if l != r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Byte(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                let value = if l != r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                let value = if l as u16 != r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                let value = if l != r as u16 { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::LT => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                let value = if l < r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Byte(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                let value = if l < r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                let value = if (l as u16) < r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                let value = if l < r as u16 { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::GT => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                let value = if l > r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Byte(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                let value = if l > r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                let value = if (l as u16) > r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                let value = if l > r as u16 { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::LE => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                let value = if l <= r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Byte(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                let value = if l <= r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                let value = if (l as u16) <= r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                let value = if l <= r as u16 { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::GE => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                let value = if l >= r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Byte(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                let value = if l >= r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                let value = if (l as u16) >= r { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                let value = if l >= r as u16 { 1 } else { 0 };
                Ok(Object::new(ObjectKind::Word(value), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::Lsh => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Byte(l << r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l << r), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word((l as u16) << r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Word(l << r as u16), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::Rsh => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Byte(l >> r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l >> r), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word((l as u16) >> r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Word(l >> r as u16), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::Add => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Byte(l + r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l + r), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l as u16 + r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Word(l + r as u16), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::String(r)) => {
                Ok(Object::new(ObjectKind::String(l.to_string() + &r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::String(r)) => {
                Ok(Object::new(ObjectKind::String(l.to_string() + &r), infix.span()))
            }
            (ObjectKind::String(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::String(l + &r.to_string()), infix.span()))
            }
            (ObjectKind::String(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::String(l + &r.to_string()), infix.span()))
            }
            (ObjectKind::String(l), ObjectKind::String(r)) => {
                Ok(Object::new(ObjectKind::String(l + &r), infix.span()))
            }
        }
        InfixOperatorKind::Sub => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Byte(l - r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l - r), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l as u16 - r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Word(l - r as u16), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::Mul => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Byte(l * r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l * r), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l as u16 * r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Word(l * r as u16), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::Div => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Byte(l / r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l / r), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l as u16 / r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Word(l / r as u16), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
        InfixOperatorKind::Mod => match (lhs.kind, rhs.kind) {
            (ObjectKind::Byte(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Byte(l % r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l % r), infix.span()))
            }
            (ObjectKind::Byte(l), ObjectKind::Word(r)) => {
                Ok(Object::new(ObjectKind::Word(l as u16 % r), infix.span()))
            }
            (ObjectKind::Word(l), ObjectKind::Byte(r)) => {
                Ok(Object::new(ObjectKind::Word(l % r as u16), infix.span()))
            }
            _ => Err(EvalError::InvalidApplicationOfInfixOperator { span: infix.span() })
        }
    }
}

fn eval_unary(unary: &UnaryExpression, symtable: &SymbolTable, global: &str) -> Result<Object, EvalError> {
    let obj = eval(&unary.expr, symtable, global)?;
    match unary.op.kind {
        UnaryOperatorKind::Neg => {
            match obj.kind {
                ObjectKind::Byte(value) => {
                    let value = (value as i8 * -1) as u8;
                    Ok(Object::new(ObjectKind::Byte(value), obj.span()))
                }
                ObjectKind::Word(value) => {
                    let value = (value as i16 * -1) as u16;
                    Ok(Object::new(ObjectKind::Word(value), obj.span()))
                }
                ObjectKind::String(_) => {
                    Err(EvalError::InvalidApplicationOfUnaryOperator {
                        span: unary.span(),
                        op: "-",
                        ty: "string"
                    })
                }
            }
        }
        UnaryOperatorKind::Not => {
            match obj.kind {
                ObjectKind::Byte(value) => {
                    let value = if value != 0 { 0 } else { 1 };
                    Ok(Object::new(ObjectKind::Byte(value), obj.span()))
                }
                ObjectKind::Word(value) => {
                    let value = if value != 0 { 0 } else { 1 };
                    Ok(Object::new(ObjectKind::Word(value), obj.span()))
                }
                ObjectKind::String(_) => {
                    Err(EvalError::InvalidApplicationOfUnaryOperator {
                        span: unary.span(),
                        op: "!",
                        ty: "string",
                    })
                }
            }
        }
        UnaryOperatorKind::BitNot => {
            match obj.kind {
                ObjectKind::Byte(value) => {
                    Ok(Object::new(ObjectKind::Byte(!value), obj.span()))
                }
                ObjectKind::Word(value) => {
                    Ok(Object::new(ObjectKind::Word(!value), obj.span()))
                }
                ObjectKind::String(_) => {
                    Err(EvalError::InvalidApplicationOfUnaryOperator {
                        span: unary.span(),
                        op: "~",
                        ty: "string",
                    })
                }
            }
        }
        UnaryOperatorKind::LSB => {
            match obj.kind {
                ObjectKind::Byte(value) => {
                    let value = (value as u16).to_le_bytes()[0];
                    Ok(Object::new(ObjectKind::Byte(value), obj.span()))
                }
                ObjectKind::Word(value) => {
                    let value = value.to_le_bytes()[0];
                    Ok(Object::new(ObjectKind::Byte(value), obj.span()))
                }
                ObjectKind::String(_) => {
                    Err(EvalError::InvalidApplicationOfUnaryOperator {
                        span: unary.span(),
                        op: "<",
                        ty: "string",
                    })
                }
            }
        }
        UnaryOperatorKind::MSB => {
            match obj.kind {
                ObjectKind::Byte(value) => {
                    let value = (value as u16).to_le_bytes()[1];
                    Ok(Object::new(ObjectKind::Byte(value), obj.span()))
                }
                ObjectKind::Word(value) => {
                    let value = value.to_le_bytes()[1];
                    Ok(Object::new(ObjectKind::Byte(value), obj.span()))
                }
                ObjectKind::String(_) => {
                    Err(EvalError::InvalidApplicationOfUnaryOperator {
                        span: unary.span(),
                        op: ">",
                        ty: "string",
                    })
                }
            }
        }
    }
}

fn eval_func(func: &FunctionCallExpression, symtable: &SymbolTable, global: &str) -> Result<Object, EvalError> {
    match func.name.name.as_str() {
        "typeof" => {
            if func.params.len() == 1 {
                let obj = eval(&func.params[0].expr, symtable, global)?;
                let obj = match obj.kind {
                    ObjectKind::Byte(_) => Object::new(ObjectKind::String("byte".to_string()), obj.span()),
                    ObjectKind::Word(_) => Object::new(ObjectKind::String("word".to_string()), obj.span()),
                    ObjectKind::String(_) => Object::new(ObjectKind::String("string".to_string()), obj.span()),
                };
                Ok(obj)
            } else {
                Err(EvalError::InvalidNumberOfArgumentForTheFunction {
                    span: func.span(), expect: 1, got: func.params.len()
                })
            }
        }
        "bankof" => {
            if func.params.len() == 1 {
                match func.params[0].expr.as_ref() {
                    Expression::SymbolExpression(sym) => match sym.symbol {
                        Symbol::GlobalSymbol(ref global) => match symtable.fetch_global_bank(&global.name) {
                            Some(bank) => Ok(Object::new(ObjectKind::Word(bank), func.params[0].span())),
                            None => Err(EvalError::NoSuchGlobalSymbolDefined { span: sym.span() }),
                        }
                        Symbol::LocalSymbol(ref local) => match symtable.fetch_local_bank(global, &local.name) {
                            Some(bank) => Ok(Object::new(ObjectKind::Word(bank), func.params[0].span())),
                            None => Err(EvalError::NoSuchLocalSymbolDefined { span: sym.span() }),
                        }
                    }
                    _ => Err(EvalError::InvalidTypeOfFunctionArgument {
                        span: func.params[0].expr.span(), expect: "symbol", got: "expression"
                    })
                }
            } else {
                Err(EvalError::InvalidNumberOfArgumentForTheFunction {
                    span: func.span(), expect: 1, got: func.params.len()
                })
            }
        }
        _ => Err(EvalError::NoSuchFunctionDefined { span: func.span() })
    }
}
