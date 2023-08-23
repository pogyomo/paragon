use derive_new::new;
use paragon_span::{Span, Spannable};
use crate::symbol::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression<'src> {
    IntegerExpression(IntegerExpression),
    FunctionCallExpression(FunctionCallExpression<'src>),
    SymbolExpression(SymbolExpression<'src>),
    InfixExpression(InfixExpression<'src>),
    UnaryExpression(UnaryExpression<'src>),
}

impl<'src> Spannable for Expression<'src> {
    fn span(&self) -> Span {
        match self {
            Expression::IntegerExpression(int) => int.span(),
            Expression::FunctionCallExpression(func) => func.span(),
            Expression::SymbolExpression(sym) => sym.span(),
            Expression::InfixExpression(infix) => infix.span(),
            Expression::UnaryExpression(unary) => unary.span(),
        }
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntegerExpression {
    pub value: u16,
    span: Span,
}

impl<'src> Spannable for IntegerExpression {
    fn span(&self) -> Span {
        self.span
    }
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
    span: Span,
}

impl<'src> Spannable for FunctionCallExpression<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'src> From<FunctionCallExpression<'src>> for Expression<'src> {
    fn from(value: FunctionCallExpression<'src>) -> Self {
        Expression::FunctionCallExpression(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionName<'src> {
    pub name: &'src str,
    span: Span,
}

impl<'src> Spannable for FunctionCallExpressionName<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionParams<'src> {
    pub params: Vec<FunctionCallExpressionParam<'src>>,
    span: Span,
}

impl<'src> Spannable for FunctionCallExpressionParams<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionParam<'src> {
    pub expr: Box<Expression<'src>>,
    span: Span,
}

impl<'src> Spannable for FunctionCallExpressionParam<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymbolExpression<'src> {
    pub symbol: Symbol<'src>,
    span: Span,
}

impl<'src> Spannable for SymbolExpression<'src> {
    fn span(&self) -> Span {
        self.span
    }
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
    span: Span,
}

impl<'src> Spannable for InfixExpression<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'src> From<InfixExpression<'src>> for Expression<'src> {
    fn from(value: InfixExpression<'src>) -> Self {
        Expression::InfixExpression(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InfixOperator {
    pub kind: InfixOperatorKind,
    span: Span,
}

impl Spannable for InfixOperator {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum InfixOperatorKind {
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
    span: Span,
}

impl<'src> Spannable for UnaryExpression<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'src> From<UnaryExpression<'src>> for Expression<'src> {
    fn from(value: UnaryExpression<'src>) -> Self {
        Expression::UnaryExpression(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnaryOperator {
    pub kind: UnaryOperatorKind,
    span: Span
}

impl Spannable for UnaryOperator {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOperatorKind {
    /// "-"
    Neg,
    /// "~"
    BitNot,
    /// "!"
    Not,
}
