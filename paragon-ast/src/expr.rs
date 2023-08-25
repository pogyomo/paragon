use derive_new::new;
use paragon_span::{Span, Spannable};
use crate::symbol::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    SurroundedExpression(SurroundedExpression),
    IntegerExpression(IntegerExpression),
    FunctionCallExpression(FunctionCallExpression),
    SymbolExpression(SymbolExpression),
    InfixExpression(InfixExpression),
    UnaryExpression(UnaryExpression),
}

impl Spannable for Expression {
    fn span(&self) -> Span {
        match self {
            Expression::SurroundedExpression(surr) => surr.span(),
            Expression::IntegerExpression(int) => int.span(),
            Expression::FunctionCallExpression(func) => func.span(),
            Expression::SymbolExpression(sym) => sym.span(),
            Expression::InfixExpression(infix) => infix.span(),
            Expression::UnaryExpression(unary) => unary.span(),
        }
    }
}

/// (expr)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SurroundedExpression {
    pub expr: Box<Expression>,
    span: Span,
}

impl SurroundedExpression {
    pub fn new(expr: Expression, span: Span) -> Self {
        Self { expr: Box::new(expr), span }
    }
}

impl Spannable for SurroundedExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl From<SurroundedExpression> for Expression {
    fn from(value: SurroundedExpression) -> Self {
        Expression::SurroundedExpression(value)
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
    pub params: Vec<FunctionCallExpressionParam>,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionParam {
    pub expr: Box<Expression>,
}

impl FunctionCallExpressionParam {
    pub fn new(expr: Expression) -> Self {
        Self { expr: Box::new(expr) }
    }
}

impl Spannable for FunctionCallExpressionParam {
    fn span(&self) -> Span {
        self.expr.span()
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymbolExpression {
    pub symbol: Symbol,
}

impl Spannable for SymbolExpression {
    fn span(&self) -> Span {
        self.symbol.span()
    }
}

impl From<SymbolExpression> for Expression {
    fn from(value: SymbolExpression) -> Self {
        Expression::SymbolExpression(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InfixExpression {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub op: InfixOperator,
}

impl InfixExpression {
    pub fn new(lhs: Expression, rhs: Expression, op: InfixOperator) -> Self {
        Self { lhs: Box::new(lhs), rhs: Box::new(rhs), op }
    }
}

impl Spannable for InfixExpression {
    fn span(&self) -> Span {
        self.lhs.span() + self.rhs.span()
    }
}

impl From<InfixExpression> for Expression {
    fn from(value: InfixExpression) -> Self {
        Expression::InfixExpression(value)
    }
}

#[derive(new)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnaryExpression {
    pub expr: Box<Expression>,
    pub op: UnaryOperator,
}

impl UnaryExpression {
    pub fn new(expr: Expression, op: UnaryOperator) -> Self {
        Self { expr: Box::new(expr), op }
    }
}

impl Spannable for UnaryExpression {
    fn span(&self) -> Span {
        self.expr.span() + self.op.span()
    }
}

impl From<UnaryExpression> for Expression {
    fn from(value: UnaryExpression) -> Self {
        Expression::UnaryExpression(value)
    }
}

#[derive(new)]
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
    /// "<"
    LSB,
    /// ">"
    MSB,
}
