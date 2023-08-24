mod lexer;

use std::path::Path;
use lexer::Lexer;
use paragon_token::Token;
use paragon_cache::FileCache;

pub use lexer::LexError;

pub fn lex<P: AsRef<Path>>(path: P, cache: &mut FileCache) -> Result<Vec<Token>, Vec<LexError>> {
    let id = match cache.cache(path.as_ref()) {
        Ok(id) => id,
        Err(e) => return Err(vec![
            LexError::FailedToOpenFile {
                path: path.as_ref().to_path_buf(),
                reason: e,
            }
        ])
    };
    Lexer::new(id, cache.fetch_content(id).unwrap()).lex()
}
