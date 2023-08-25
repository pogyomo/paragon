mod parser;
mod stack;

use std::{path::Path, io};
use parser::Parser;
use paragon_ast::Statement;
use paragon_cache::FileCache;
use paragon_lexer::lex;

pub use parser::ParseError;

type ParseResult = io::Result<Result<Vec<Statement>, Vec<ParseError>>>;

/// Parse a file.
pub fn parse<P: AsRef<Path>>(path: P, cache: &mut FileCache) -> ParseResult {
    let tokens = match lex(path, cache)? {
        Ok(tokens) => tokens,
        Err(errors) => return Ok(Err(vec![
            ParseError::LexError { errors }
        ]))
    };
    Ok(Parser::new(tokens).parse())
}
