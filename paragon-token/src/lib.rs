use paragon_span::{Span, Spannable};
use derive_new::new;

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub kind: TokenKind,
    span: Span,
}

impl Spannable for Token {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    // Literal
    Identifier(String),
    Integer(String, IntRadix),

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
    /// "="
    Equal,

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
    /// "#"
    Sharp,
    /// ":"
    Colon,

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
