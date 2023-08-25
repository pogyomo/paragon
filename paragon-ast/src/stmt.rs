// HACK: Can I automatically implement from trait for each statement?

use derive_new::new;
use paragon_span::{Spannable, Span};
use crate::{symbol::Symbol, expr::Expression};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Statement {
    SymbolStatement(SymbolStatement),
    InstructionStatement(InstructionStatement),
    PseudoInstructionStatement(PseudoInstructionStatement),
}

impl Spannable for Statement {
    fn span(&self) -> Span {
        match self {
            Statement::SymbolStatement(sym) => sym.span(),
            Statement::InstructionStatement(inst) => inst.span(),
            Statement::PseudoInstructionStatement(pseudo) => pseudo.span(),
        }
    }
}

/// symbol:
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymbolStatement {
    pub symbol: Symbol,
    span: Span,
}

impl Spannable for SymbolStatement {
    fn span(&self) -> Span {
        self.span
    }
}

impl From<SymbolStatement> for Statement {
    fn from(value: SymbolStatement) -> Self {
        Statement::SymbolStatement(value)
    }
}

/// name params
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionStatement {
    pub name: InstructionName,
    pub params: InstructionParams,
    span: Span,
}

impl Spannable for InstructionStatement {
    fn span(&self) -> Span {
        self.span
    }
}

impl From<InstructionStatement> for Statement {
    fn from(value: InstructionStatement) -> Self {
        Statement::InstructionStatement(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionName {
    pub name: String,
    span: Span,
}

impl Spannable for InstructionName {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionParams {
    pub params: Vec<InstructionParam>,
    span: Span,
}

impl Spannable for InstructionParams {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum InstructionParam {
    Accumulator(Accumulator),
    AbsoluteOrZeropage(AbsoluteOrZeropage),
    Relative(Relative),
    Indirect(Indirect),
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Accumulator {
    span: Span
}

impl Spannable for Accumulator {
    fn span(&self) -> Span {
        self.span
    }
}

/// expr [ , x ] | [ , y ]
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AbsoluteOrZeropage {
    pub expr: Expression,
    pub register: Option<IndexableRegister>,
    span: Span,
}

impl Spannable for AbsoluteOrZeropage {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Relative {
    pub symbol: Symbol,
    span: Span,
}

impl Spannable for Relative {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Indirect {
    pub expr: Expression,
    pub register: Option<IndexableRegister>,
    span: Span,
}

impl Spannable for Indirect {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IndexableRegister {
    pub kind: IndexableRegisterKind,
    span: Span,
}

impl Spannable for IndexableRegister {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum IndexableRegisterKind {
    X,
    Y,
}

/// .name params
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionStatement {
    pub name: PseudoInstructionName,
    pub params: PseudoInstructionParams,
    span: Span,
}

impl Spannable for PseudoInstructionStatement {
    fn span(&self) -> Span {
        self.span
    }
}

impl From<PseudoInstructionStatement> for Statement {
    fn from(value: PseudoInstructionStatement) -> Self {
        Statement::PseudoInstructionStatement(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionName {
    pub name: String,
    span: Span,
}

impl Spannable for PseudoInstructionName {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionParams {
    pub params: Vec<PseudoInstructionParam>,
    span: Span,
}

impl Spannable for PseudoInstructionParams {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionParam {
    pub expr: Expression,
    span: Span,
}

impl Spannable for PseudoInstructionParam {
    fn span(&self) -> Span {
        self.span
    }
}
