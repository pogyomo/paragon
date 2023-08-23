// HACK: Can I automatically implement from trait for each statement?

use derive_new::new;
use paragon_span::{Spannable, Span};
use crate::{symbol::Symbol, expr::Expression};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Statement<'src> {
    InstructionStatement(InstructionStatement<'src>),
    PseudoInstructionStatement(PseudoInstructionStatement<'src>),
}

impl<'src> Spannable for Statement<'src> {
    fn span(&self) -> Span {
        match self {
            Statement::InstructionStatement(inst) => inst.span(),
            Statement::PseudoInstructionStatement(pseudo) => pseudo.span(),
        }
    }
}

/// [ symbol: ] name params
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionStatement<'src> {
    pub symbol: Option<InstructionSymbol<'src>>,
    pub name: InstructionName<'src>,
    pub params: InstructionParams<'src>,
    span: Span,
}

impl<'src> Spannable for InstructionStatement<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'src> From<InstructionStatement<'src>> for Statement<'src> {
    fn from(value: InstructionStatement<'src>) -> Self {
        Statement::InstructionStatement(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionSymbol<'src> {
    pub symbol: Symbol<'src>,
    span: Span,
}

impl<'src> Spannable for InstructionSymbol<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionName<'src> {
    pub name: &'src str,
    span: Span,
}

impl<'src> Spannable for InstructionName<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionParams<'src> {
    pub params: Vec<InstructionParam<'src>>,
    span: Span,
}

impl<'src> Spannable for InstructionParams<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum InstructionParam<'src> {
    Accumulator(Accumulator),
    AbsoluteOrZeropage(AbsoluteOrZeropage<'src>),
    Relative(Relative<'src>),
    Indirect(Indirect<'src>),
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
pub struct AbsoluteOrZeropage<'src> {
    pub expr: Expression<'src>,
    pub register: Option<IndexableRegister>,
    span: Span,
}

impl<'src> Spannable for AbsoluteOrZeropage<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Relative<'src> {
    pub symbol: Symbol<'src>,
    span: Span,
}

impl<'src> Spannable for Relative<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Indirect<'src> {
    pub expr: Expression<'src>,
    pub register: Option<IndexableRegister>,
    span: Span,
}

impl<'src> Spannable for Indirect<'src> {
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

/// [ symbol: ] .name params
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionStatement<'src> {
    pub symbol: Option<PseudoInstructionSymbol<'src>>,
    pub name: PseudoInstructionName<'src>,
    pub params: PseudoInstructionParams<'src>,
    span: Span,
}

impl<'src> Spannable for PseudoInstructionStatement<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'src> From<PseudoInstructionStatement<'src>> for Statement<'src> {
    fn from(value: PseudoInstructionStatement<'src>) -> Self {
        Statement::PseudoInstructionStatement(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionSymbol<'src> {
    pub symbol: Symbol<'src>,
    span: Span,
}

impl<'src> Spannable for PseudoInstructionSymbol<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionName<'src> {
    pub name: &'src str,
    span: Span,
}

impl<'src> Spannable for PseudoInstructionName<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionParams<'src> {
    pub params: Vec<PseudoInstructionParam<'src>>,
    span: Span,
}

impl<'src> Spannable for PseudoInstructionParams<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionParam<'src> {
    pub expr: Expression<'src>,
    span: Span,
}

impl<'src> Spannable for PseudoInstructionParam<'src> {
    fn span(&self) -> Span {
        self.span
    }
}
