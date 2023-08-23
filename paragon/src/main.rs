use paragon_lexer::Lexer;

fn main() {
    let lexer = Lexer::new("// hoge");
    println!("{:#?}", lexer.lex());
}
