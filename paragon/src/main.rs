use paragon_cache::FileCache;
use paragon_lexer::lex;
use paragon_parser::parse;

fn main() {
    let args = std::env::args_os().collect::<Vec<_>>();
    let mut cache = FileCache::new();
    let stmts = parse(args.get(1).unwrap(), &mut cache).unwrap();
    match stmts {
        Ok(stmts) => {
            println!("{stmts:#?}");
        }
        Err(e) => {
            println!("{e}");
        }
    }
}
