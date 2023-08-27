use derive_new::new;
use paragon_span::{Span, Spannable};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ObjectType {
    Byte,
    Word,
    String,
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Object {
    pub kind: ObjectKind,
    span: Span,
}

impl Spannable for Object {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ObjectKind {
    Byte(u8),
    Word(u16),
    String(String),
}

impl ObjectKind {
    pub fn byte(value: u8) -> ObjectKind {
        ObjectKind::Byte(value)
    }

    pub fn word(value: u16) -> ObjectKind {
        ObjectKind::Word(value)
    }

    pub fn string(value: String) -> ObjectKind {
        ObjectKind::String(value)
    }
}
