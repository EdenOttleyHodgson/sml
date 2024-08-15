use std::env::args;

mod lexer_parser;

fn main() {
    let input = "
                    plog 
                    Char 
                    String
                    Int
                    Void
                    Regex
                    if
                    else
                    while
                    for
                    in
                    to
                    fn
                    :
                    ;
                    let
                    =
                    ==
                    +
                    -
                    *
                    /
                    (
                    )
                    [
                    ]
                    {
                    }
                    //trogle
                ";

    let tokens = lexer_parser::lexer::Lexer::new(input);
    for tok in tokens.into_iter() {
        println!("{tok:?}")
    }
}
