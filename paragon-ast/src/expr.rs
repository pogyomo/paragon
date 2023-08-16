use derive_new::new;
use crate::symbol::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression<'src, S> {
    IntegerExpression(IntegerExpression<S>),
    FunctionCallExpression(FunctionCallExpression<'src, S>),
    SymbolExpression(SymbolExpression<'src, S>),
    InfixExpression(InfixExpression<'src, S>),
    UnaryExpression(UnaryExpression<'src, S>),
}

impl<'src, S> Expression<'src, S> {
    pub fn span(&self) -> &S {
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
pub struct IntegerExpression<S> {
    pub value: u16,
    span: S,
}

impl<'src, S> IntegerExpression<S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

impl<'src, S> From<IntegerExpression<S>> for Expression<'src, S> {
    fn from(value: IntegerExpression<S>) -> Self {
        Expression::IntegerExpression(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpression<'src, S> {
    pub name: FunctionCallExpressionName<'src, S>,
    pub params: FunctionCallExpressionParams<'src, S>,
    span: S,
}

impl<'src, S> FunctionCallExpression<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

impl<'src, S> From<FunctionCallExpression<'src, S>> for Expression<'src, S> {
    fn from(value: FunctionCallExpression<'src, S>) -> Self {
        Expression::FunctionCallExpression(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionName<'src, S> {
    pub name: &'src str,
    span: S,
}

impl<'src, S> FunctionCallExpressionName<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionParams<'src, S> {
    pub params: Vec<FunctionCallExpressionParam<'src, S>>,
    span: S,
}

impl<'src, S> FunctionCallExpressionParams<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallExpressionParam<'src, S> {
    pub expr: Box<Expression<'src, S>>,
    span: S,
}

impl<'src, S> FunctionCallExpressionParam<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymbolExpression<'src, S> {
    pub symbol: Symbol<'src, S>,
    span: S,
}

impl<'src, S> SymbolExpression<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

impl<'src, S> From<SymbolExpression<'src, S>> for Expression<'src, S> {
    fn from(value: SymbolExpression<'src, S>) -> Self {
        Expression::SymbolExpression(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InfixExpression<'src, S> {
    pub lhs: Box<Expression<'src, S>>,
    pub rhs: Box<Expression<'src, S>>,
    pub op: InfixOperator<S>,
    span: S,
}

impl<'src, S> InfixExpression<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

impl<'src, S> From<InfixExpression<'src, S>> for Expression<'src, S> {
    fn from(value: InfixExpression<'src, S>) -> Self {
        Expression::InfixExpression(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InfixOperator<S> {
    pub kind: InfixOperatorKind,
    span: S,
}

impl<S> InfixOperator<S> {
    pub fn span(&self) -> &S {
        &self.span
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
pub struct UnaryExpression<'src, S> {
    pub expr: Box<Expression<'src, S>>,
    pub op: UnaryOperator<S>,
    span: S,
}

impl<'src, S> UnaryExpression<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

impl<'src, S> From<UnaryExpression<'src, S>> for Expression<'src, S> {
    fn from(value: UnaryExpression<'src, S>) -> Self {
        Expression::UnaryExpression(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnaryOperator<S> {
    pub kind: UnaryOperatorKind,
    span: S
}

impl<S> UnaryOperator<S> {
    pub fn span(&self) -> &S {
        &self.span
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
