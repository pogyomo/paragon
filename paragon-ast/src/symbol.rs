use derive_new::new;
use paragon_span::{Spannable, Span};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol<'src> {
    GlobalSymbol(GlobalSymbol<'src>),
    LocalSymbol(LocalSymbol<'src>),
}

impl<'src> Spannable for Symbol<'src> {
    fn span(&self) -> Span {
        match self {
            Self::GlobalSymbol(sym) => sym.span(),
            Self::LocalSymbol(sym) => sym.span(),
        }
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct GlobalSymbol<'src> {
    pub name: &'src str,
    span: Span,
}

impl<'src> Spannable for GlobalSymbol<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'src> From<GlobalSymbol<'src>> for Symbol<'src> {
    fn from(value: GlobalSymbol<'src>) -> Self {
        Symbol::GlobalSymbol(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalSymbol<'src> {
    pub name: &'src str,
    span: Span,
}

impl<'src> Spannable for LocalSymbol<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'src> From<LocalSymbol<'src>> for Symbol<'src> {
    fn from(value: LocalSymbol<'src>) -> Self {
        Symbol::LocalSymbol(value)
    }
}
