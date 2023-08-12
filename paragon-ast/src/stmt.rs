use crate::{symbol::Symbol, expr::Expression};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Statement<'src> {
    InstructionStatement(InstructionStatement<'src>),
    PseudoInstructionStatement(PseudoInstructionStatement<'src>),
}

/// [ symbol: ] name operaond
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionStatement<'src> {
    pub symbol: Option<Symbol<'src>>,
    pub name: InstructionName<'src>,
    pub params: InstructionParams<'src>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionName<'src> {
    pub name: &'src str,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionParams<'src> {
    pub params: Vec<InstructionParam<'src>>
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum InstructionParam<'src> {
    Accumulator(Accumulator),
    AbsoluteOrZeropage(AbsoluteOrZeropage<'src>),
    Relative(Relative<'src>),
    Indirect(Indirect<'src>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Accumulator;

/// expr [ , x ] | [ , y ]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AbsoluteOrZeropage<'src> {
    pub expr: Expression<'src>,
    pub register: Option<IndexableRegister>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Relative<'src> {
    pub symbol: Symbol<'src>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Indirect<'src> {
    pub expr: Expression<'src>,
    pub register: Option<IndexableRegister>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IndexableRegister {
    X,
    Y,
}

/// [ symbol: ] .name operand
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionStatement<'src> {
    pub symbol: Option<Symbol<'src>>,
    pub name: PseudoInstructionName<'src>,
    pub params: PseudoInstructionParams<'src>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionName<'src> {
    pub name: &'src str,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionParams<'src> {
    pub params: Vec<PseudoInstructionParam<'src>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PseudoInstructionParam<'src> {
    pub expr: Expression<'src>,
}
