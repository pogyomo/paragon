use derive_new::new;
use paragon_span::Span;

#[derive(new)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub span: Span,
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
