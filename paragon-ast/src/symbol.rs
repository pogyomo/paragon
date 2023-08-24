use derive_new::new;
use paragon_span::{Spannable, Span};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol {
    GlobalSymbol(GlobalSymbol),
    LocalSymbol(LocalSymbol),
}

impl Spannable for Symbol {
    fn span(&self) -> Span {
        match self {
            Self::GlobalSymbol(sym) => sym.span(),
            Self::LocalSymbol(sym) => sym.span(),
        }
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct GlobalSymbol {
    pub name: String,
    span: Span,
}

impl Spannable for GlobalSymbol {
    fn span(&self) -> Span {
        self.span
    }
}

impl From<GlobalSymbol> for Symbol {
    fn from(value: GlobalSymbol) -> Self {
        Symbol::GlobalSymbol(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalSymbol {
    pub name: String,
    span: Span,
}

impl Spannable for LocalSymbol {
    fn span(&self) -> Span {
        self.span
    }
}

impl From<LocalSymbol> for Symbol {
    fn from(value: LocalSymbol) -> Self {
        Symbol::LocalSymbol(value)
    }
}
