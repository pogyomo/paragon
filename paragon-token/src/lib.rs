use derive_new::new;

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token<'src, S> {
    pub kind: TokenKind<'src>,
    span: S,
}

impl<'src, S> Token<'src, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind<'src> {
    // Literal
    Identifier(&'src str),
    Integer(&'src str, IntRadix),

    // Oerators
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "*"
    Star,
    /// "/"
    Slash,
    /// "%"
    Percent,
    /// "&"
    Ampersand,
    /// "|"
    Vertical,
    /// "^"
    Circumflex,
    /// "<<"
    LShift,
    /// '>>'
    RShift,
    /// "&&"
    And,
    /// "||"
    Or,
    /// "!"
    Not,
    /// "<"
    LT,
    /// ">"
    GT,
    /// "<="
    LE,
    /// ">="
    GE,
    /// "=="
    EQ,
    /// "!="
    NE,
    /// "~"
    Tilde,

    // Symbols
    /// "("
    LParen,
    /// ")"
    RParen,
    /// "["
    LSquare,
    /// "]"
    RSquare,
    /// ","
    Comma,

    // Special
    /// '\n'
    Newline,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntRadix {
    Binary = 2,
    Octadecimal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}
