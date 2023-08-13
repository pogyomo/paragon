use derive_new::new;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol<'src> {
    GlobalSymbol(GlobalSymbol<'src>),
    LocalSymbol(LocalSymbol<'src>),
}

#[derive(new)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct GlobalSymbol<'src> {
    pub name: &'src str,
}

impl<'src> From<GlobalSymbol<'src>> for Symbol<'src> {
    fn from(value: GlobalSymbol<'src>) -> Self {
        Symbol::GlobalSymbol(value)
    }
}

#[derive(new)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalSymbol<'src> {
    pub name: &'src str,
}

impl<'src> From<LocalSymbol<'src>> for Symbol<'src> {
    fn from(value: LocalSymbol<'src>) -> Self {
        Symbol::LocalSymbol(value)
    }
}
