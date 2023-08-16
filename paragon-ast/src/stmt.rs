// HACK: Can I automatically implement from trait for each statement?

use derive_new::new;
use crate::{symbol::Symbol, expr::Expression};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Statement<'src, S> {
    InstructionStatement(InstructionStatement<'src, S>),
    PseudoInstructionStatement(PseudoInstructionStatement<'src, S>),
}

impl<'src, S> Statement<'src, S> {
    pub fn span(&self) -> &S {
        match self {
            Statement::InstructionStatement(inst) => inst.span(),
            Statement::PseudoInstructionStatement(pseudo) => pseudo.span(),
        }
    }
}

/// [ symbol: ] name params
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionStatement<'src, S> {
    pub symbol: Option<InstructionSymbol<'src, S>>,
    pub name: InstructionName<'src, S>,
    pub params: InstructionParams<'src, S>,
    span: S,
}

impl<'src, S> InstructionStatement<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

impl<'src, S> From<InstructionStatement<'src, S>> for Statement<'src, S> {
    fn from(value: InstructionStatement<'src, S>) -> Self {
        Statement::InstructionStatement(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionSymbol<'src, S> {
    pub symbol: Symbol<'src, S>,
    span: S,
}

impl<'src, S> InstructionSymbol<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionName<'src, S> {
    pub name: &'src str,
    span: S,
}

impl<'src, S> InstructionName<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionParams<'src, S> {
    pub params: Vec<InstructionParam<'src, S>>,
    span: S,
}

impl<'src, S> InstructionParams<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum InstructionParam<'src, S> {
    Accumulator(Accumulator<S>),
    AbsoluteOrZeropage(AbsoluteOrZeropage<'src, S>),
    Relative(Relative<'src, S>),
    Indirect(Indirect<'src, S>),
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Accumulator<S> {
    span: S
}

impl<S> Accumulator<S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

/// expr [ , x ] | [ , y ]
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AbsoluteOrZeropage<'src, S> {
    pub expr: Expression<'src, S>,
    pub register: Option<IndexableRegister<S>>,
    span: S,
}

impl<'src, S> AbsoluteOrZeropage<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Relative<'src, S> {
    pub symbol: Symbol<'src, S>,
    span: S,
}

impl<'src, S> Relative<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Indirect<'src, S> {
    pub expr: Expression<'src, S>,
    pub register: Option<IndexableRegister<S>>,
    span: S,
}

impl<'src, S> Indirect<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IndexableRegister<S> {
    pub kind: IndexableRegisterKind,
    span: S,
}

impl<S> IndexableRegister<S> {
    pub fn span(&self) -> &S {
        &self.span
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
pub struct PseudoInstructionStatement<'src, S> {
    pub symbol: Option<PseudoInstructionSymbol<'src, S>>,
    pub name: PseudoInstructionName<'src, S>,
    pub params: PseudoInstructionParams<'src, S>,
    span: S,
}

impl<'src, S> PseudoInstructionStatement<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

impl<'src, S> From<PseudoInstructionStatement<'src, S>> for Statement<'src, S> {
    fn from(value: PseudoInstructionStatement<'src, S>) -> Self {
        Statement::PseudoInstructionStatement(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionSymbol<'src, S> {
    pub symbol: Symbol<'src, S>,
    span: S,
}

impl<'src, S> PseudoInstructionSymbol<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionName<'src, S> {
    pub name: &'src str,
    span: S,
}

impl<'src, S> PseudoInstructionName<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionParams<'src, S> {
    pub params: Vec<PseudoInstructionParam<'src, S>>,
    span: S,
}

impl<'src, S> PseudoInstructionParams<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionParam<'src, S> {
    pub expr: Expression<'src, S>,
    span: S,
}

impl<'src, S> PseudoInstructionParam<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}
