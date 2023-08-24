use derive_new::new;
use paragon_span::{Span, Spannable};
use crate::symbol::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    IntegerExpression(IntegerExpression),
    FunctionCallExpression(FunctionCallExpression),
    SymbolExpression(SymbolExpression),
    InfixExpression(InfixExpression),
    UnaryExpression(UnaryExpression),
}

impl Spannable for Expression {
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

impl Spannable for IntegerExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl From<IntegerExpression> for Expression {
    fn from(value: IntegerExpression) -> Self {
        Expression::IntegerExpression(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpression {
    pub name: FunctionCallExpressionName,
    pub params: FunctionCallExpressionParams,
    span: Span,
}

impl Spannable for FunctionCallExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl From<FunctionCallExpression> for Expression {
    fn from(value: FunctionCallExpression) -> Self {
        Expression::FunctionCallExpression(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionName {
    pub name: String,
    span: Span,
}

impl Spannable for FunctionCallExpressionName {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionParams {
    pub params: Vec<FunctionCallExpressionParam>,
    span: Span,
}

impl Spannable for FunctionCallExpressionParams {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionParam {
    pub expr: Box<Expression>,
    span: Span,
}

impl Spannable for FunctionCallExpressionParam {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymbolExpression {
    pub symbol: Symbol,
    span: Span,
}

impl Spannable for SymbolExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl From<SymbolExpression> for Expression {
    fn from(value: SymbolExpression) -> Self {
        Expression::SymbolExpression(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InfixExpression {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub op: InfixOperator,
    span: Span,
}

impl Spannable for InfixExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl From<InfixExpression> for Expression {
    fn from(value: InfixExpression) -> Self {
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
pub struct UnaryExpression {
    pub expr: Box<Expression>,
    pub op: UnaryOperator,
    span: Span,
}

impl Spannable for UnaryExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl From<UnaryExpression> for Expression {
    fn from(value: UnaryExpression) -> Self {
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
