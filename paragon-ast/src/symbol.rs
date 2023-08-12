#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol<'src> {
    GlobalSymbol(GlobalSymbol<'src>),
    LocalSymbol(LocalSymbol<'src>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct GlobalSymbol<'src> {
    pub name: &'src str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalSymbol<'src> {
    pub name: &'src str,
}
