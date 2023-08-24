mod lexer;

use std::path::Path;
use std::io;
use lexer::Lexer;
use paragon_token::Token;
use paragon_cache::FileCache;

pub use lexer::LexError;

type LexResult = io::Result<Result<Vec<Token>, Vec<LexError>>>;

pub fn lex<P: AsRef<Path>>(path: P, cache: &mut FileCache) -> LexResult {
    let id = cache.cache(path.as_ref())?;
    Ok(Lexer::new(id, cache.fetch_content(id).unwrap()).lex())
}
