use std::rc::Rc;
use paragon_span::{Span, Spannable};
use paragon_token::Token;

pub struct NonEpmtyTokenStack {
    head: Rc<Token>,
    tail: Vec<Rc<Token>>,
} 

impl<'a> Spannable for NonEpmtyTokenStack {
    fn span(&self) -> Span {
        let mut span = self.head.span();
        for t in self.tail.iter() {
            span += t.span();
        }
        span
    }
}

impl NonEpmtyTokenStack {
    pub fn new(item: Rc<Token>) -> Self {
        Self { head: item, tail: Vec::new() }
    }

    pub fn push(&mut self, token: Rc<Token>) {
        self.tail.push(token)
    }

    pub fn pop(&mut self) -> Option<Rc<Token>> {
        self.tail.pop()
    }

    pub fn last(&self) -> Rc<Token> {
        match self.tail.last() {
            Some(last) => Rc::clone(last),
            None => Rc::clone(&self.head),
        }
    }
}
