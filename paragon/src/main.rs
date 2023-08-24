use paragon_cache::FileCache;
use paragon_lexer::lex;

fn main() {
    let args = std::env::args_os().collect::<Vec<_>>();
    let mut cache = FileCache::new();
    let tks = lex(args.get(1).unwrap(), &mut cache);
    match tks {
        Ok(tks) => for tk in tks {
            println!("{tk:?}");
        }
        Err(e) => for e in e {
            println!("{e}");
        }
    }
}
