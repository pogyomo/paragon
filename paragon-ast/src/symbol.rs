use derive_new::new;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol<'src, S> {
    GlobalSymbol(GlobalSymbol<'src, S>),
    LocalSymbol(LocalSymbol<'src, S>),
}

impl<'src, S> Symbol<'src, S> {
    pub fn span(&self) -> &S {
        match self {
            Self::GlobalSymbol(sym) => sym.span(),
            Self::LocalSymbol(sym) => sym.span(),
        }
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct GlobalSymbol<'src, S> {
    pub name: &'src str,
    span: S,
}

impl<'src, S> GlobalSymbol<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

impl<'src, S> From<GlobalSymbol<'src, S>> for Symbol<'src, S> {
    fn from(value: GlobalSymbol<'src, S>) -> Self {
        Symbol::GlobalSymbol(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalSymbol<'src, S> {
    pub name: &'src str,
    span: S,
}

impl<'src, S> LocalSymbol<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

impl<'src, S> From<LocalSymbol<'src, S>> for Symbol<'src, S> {
    fn from(value: LocalSymbol<'src, S>) -> Self {
        Symbol::LocalSymbol(value)
    }
}
