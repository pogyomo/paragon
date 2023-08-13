#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token<'src> {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntRadix {
    Binary = 2,
    Octadecimal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}
