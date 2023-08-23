use std::ops::{Add, AddAssign};

/// A trait for struct which hold its span.
pub trait Spannable {
    fn span(&self) -> Span;
}

/// A struct which represent the byte-range of the item in source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    #[inline]
    pub fn start(&self) -> usize {
        self.start
    }

    #[inline]
    pub fn end(&self) -> usize {
        self.end
    }
}

impl Add for Span {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: if self.start() < rhs.start() {
                self.start()
            } else {
                rhs.start()
            },
            end: if self.end() > rhs.end() {
                self.end()
            } else {
                rhs.end()
            }
        }
    }
}

impl AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}
