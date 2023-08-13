// HACK: Can I automatically implement from trait for each expression?

use derive_new::new;
use crate::symbol::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression<'src> {
    IntegerExpression(IntegerExpression),
    FunctionCallExpression(FunctionCallExpression<'src>),
    SymbolExpression(SymbolExpression<'src>),
    InfixExpression(InfixExpression<'src>),
    UnaryExpression(UnaryExpression<'src>),
}

#[derive(new)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntegerExpression {
    pub value: u16,
}

impl<'src> From<IntegerExpression> for Expression<'src> {
    fn from(value: IntegerExpression) -> Self {
        Expression::IntegerExpression(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpression<'src> {
    pub name: FunctionCallExpressionName<'src>,
    pub params: FunctionCallExpressionParams<'src>,
}

impl<'src> From<FunctionCallExpression<'src>> for Expression<'src> {
    fn from(value: FunctionCallExpression<'src>) -> Self {
        Expression::FunctionCallExpression(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionName<'src> {
    pub name: &'src str,
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionParams<'src> {
    pub params: Vec<FunctionCallExpressionParam<'src>>,
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionParam<'src> {
    pub expr: Box<Expression<'src>>,
}

#[derive(new)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymbolExpression<'src> {
    pub symbol: Symbol<'src>,
}

impl<'src> From<SymbolExpression<'src>> for Expression<'src> {
    fn from(value: SymbolExpression<'src>) -> Self {
        Expression::SymbolExpression(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InfixExpression<'src> {
    pub lhs: Box<Expression<'src>>,
    pub rhs: Box<Expression<'src>>,
    pub op: InfixOperator,
}

impl<'src> From<InfixExpression<'src>> for Expression<'src> {
    fn from(value: InfixExpression<'src>) -> Self {
        Expression::InfixExpression(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum InfixOperator {
    /// "||"
    Or,
    /// "&&"
    And,
    /// "|"
    BitOr,
    /// "^"
    BitXor,
    /// "&"
    BitAnd,
    /// "=="
    Eq,
    /// "!="
    Ne,
    /// "<"
    LT,
    /// ">"
    GT,
    /// "<="
    LE,
    /// ">="
    GE,
    /// "<<"
    Lsh,
    /// ">>"
    Rsh,
    /// "+"
    Add,
    /// "-"
    Sub,
    /// "*"
    Mul,
    /// "/"
    Div,
    /// "%"
    Mod,
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnaryExpression<'src> {
    pub expr: Box<Expression<'src>>,
    pub op: UnaryOperator,
}

impl<'src> From<UnaryExpression<'src>> for Expression<'src> {
    fn from(value: UnaryExpression<'src>) -> Self {
        Expression::UnaryExpression(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOperator {
    /// "-"
    Neg,
    /// "~"
    BitNot,
    /// "!"
    Not,
}
