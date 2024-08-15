use thiserror::Error;

use super::*;
use plex::lexer;
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Identifier(String), //
    Type(SMLType),
    If,                    //
    Else,                  //
    While,                 //
    For,                   //
    In,                    //
    To,                    //
    Fn,                    //
    Colon,                 //
    Semicolon,             //
    Let,                   //
    Assign,                //
    Equals,                //
    Plus,                  //
    Minus,                 //
    Star,                  //
    Slash,                 //
    LBracket,              //
    RBracket,              //
    LSquare,               //
    RSquare,               //
    LCurly,                //
    RCurly,                //
    RegexLiteral(String),  //
    StringLiteral(String), //
    IntegerLiteral(i64),   //
    BoolLiteral(bool),     //
    Comment,
    Whitespace,
}
lexer! {
    fn next_token(text: 'a) -> Result<Token, LexicalError>;

    r#"//[^\n]*"# => Ok(Token::Comment),

    r#"[0-9]+"# => {
            if let Ok(i) = text.parse() {
                Ok(Token::IntegerLiteral(i))
            } else {
                Err(LexicalError::IntegerLiteralOutOfBounds(text.to_string()))
            }
    }

        r#"="# =>Ok( Token::Assign),
        r#"=="# =>Ok( Token::Equals),
        r#"\+"# =>Ok( Token::Plus),
        r#"-"# =>Ok( Token::Minus),
        r#"\*"# =>Ok( Token::Star),
        r#"/"# =>Ok( Token::Slash),
        r#"\("# =>Ok( Token::LBracket),
        r#"\)"# =>Ok( Token::RBracket),
        r#";"# =>Ok( Token::Semicolon),
        r#":"# =>Ok( Token::Colon),
        r#"{"# =>Ok( Token::LCurly),
        r#"}"# =>Ok( Token::RCurly),
        r#"\["# =>Ok( Token::LSquare),
        r#"\]"# =>Ok( Token::RSquare),
        r#"if"# => Ok(Token::If),
        r#"else"# => Ok(Token::Else),
        r#"for"# => Ok(Token::For),
        r#"while"# => Ok(Token::While),
        r#"in"# => Ok(Token::In),
        r#"to"# => Ok(Token::To),
        r#"fn"# => Ok(Token::Fn),
        r#"let"# => Ok(Token::Let),
        r#"true"# => Ok(Token::BoolLiteral(true)),
        r#"false"# => Ok(Token::BoolLiteral(false)),
        r#"."# => Err(LexicalError::UnexpectedCharacter(text.to_string())),
        r#"//[^\n]*"# => Ok(Token::Comment),
        r#"Char"# => Ok(Token::Type(SMLType::Char)),
        r#"String"# => Ok(Token::Type(SMLType::String)),
        r#"Int"# => Ok(Token::Type(SMLType::Int)),
        r#"Bool"# => Ok(Token::Type(SMLType::Bool)),
        r#"Regex"# => Ok(Token::Type(SMLType::Regex)),
        r#"Void"# => Ok(Token::Type(SMLType::Void)),
        r#""(\\.|[^"\\])"*"# => Ok(Token::StringLiteral(text.to_string())),
        r#"`(```|[^`])*`"# => Ok(Token::RegexLiteral(process_regex_string(text))),
        r#"[a-zA-Z_][a-zA-Z0-9_]*"# => Ok(Token::Identifier(text.to_owned())),
        r#"[ \t\r\n]+"# => Ok(Token::Whitespace),

}

pub struct Lexer<'a> {
    original: &'a str,
    remaining: &'a str,
    error_occurred: bool,
    on_error: Box<dyn Fn(&mut Lexer, LexicalError, Span) -> bool>,
}

impl<'a> Clone for Lexer<'a> {
    fn clone(&self) -> Self {
        Lexer {
            original: self.original,
            remaining: self.remaining,
            error_occurred: self.error_occurred,
            //evil
            on_error: Box::new(|_, _, _| true),
        }
    }
}
impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Lexer<'a> {
        let on_error = |lexer: &mut Lexer, e: LexicalError, s: Span| {
            lexical_error(e, s);
            let check_lexer = Lexer {
                original: lexer.original,
                remaining: lexer.remaining,
                error_occurred: true,
                on_error: Box::new(|_, e, s| {
                    lexical_error(e, s);
                    true
                }),
            };
            check_lexer.into_iter().last();
            false
        };

        Lexer {
            original: s,
            remaining: s,
            error_occurred: false,
            on_error: Box::new(on_error),
        }
    }
    pub fn with_error_behaviour(
        s: &'a str,
        on_error: impl Fn(&mut Lexer, LexicalError, Span) -> bool + 'static,
    ) -> Lexer {
        Lexer {
            original: s,
            remaining: s,
            error_occurred: false,
            on_error: Box::new(on_error),
        }
    }
}
#[derive(Debug)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (tok, span) = if let Some((tok, new_remaining)) = next_token(self.remaining) {
                let lo = self.original.len() - self.remaining.len();
                let hi = self.original.len() - new_remaining.len();
                let span = Span { lo, hi };
                self.remaining = new_remaining;
                match tok {
                    Ok(t) => (t, span),
                    Err(e) => {
                        self.error_occurred = true;
                        if (self.on_error)(&mut self.clone(), e, span) == false {
                            return None;
                        };
                        continue;
                    }
                }
            } else {
                return None;
            };
            match tok {
                Token::Comment | Token::Whitespace => continue,
                tok => return Some((tok, span)),
            }
        }
    }
}

fn process_regex_string(text: &str) -> String {
    let mut trimmed_iter = text
        .get(1..text.len() - 1)
        .expect("Size of text should be greater than two as ensured by lexer regex")
        .split("```");
    let init_acc = trimmed_iter.next().unwrap();
    trimmed_iter
        .fold(init_acc.to_string(), |acc, s| format!("{acc}`{s}"))
        .to_string()
}
#[derive(Error, Debug)]
pub enum LexicalError {
    #[error("The integer literal {0} is out of bounds.")]
    IntegerLiteralOutOfBounds(String),
    #[error("Unexpected character in file: {0}")]
    UnexpectedCharacter(String),
}

fn lexical_error(e: LexicalError, span: Span) {
    println!("lexical error at {}! : {}", span.lo, e.to_string());
}

#[cfg(test)]
mod test {
    use std::{iter::zip, rc::Rc};

    use super::*;
    #[test]
    fn lexes_non_literals_correctly() {
        let input = "
                    Plog 
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
        let expected = {
            use SMLType as t;
            use Token::*;

            [
                Identifier("Plog".to_string()),
                Type(t::Char),
                Type(t::String),
                Type(t::Int),
                Type(t::Void),
                Type(t::Regex),
                If,
                Else,
                While,
                For,
                In,
                To,
                Fn,
                Colon,
                Semicolon,
                Let,      //
                Assign,   //
                Equals,   //
                Plus,     //
                Minus,    //
                Star,     //
                Slash,    //
                LBracket, //
                RBracket, //
                LSquare,  //
                RSquare,  //
                LCurly,   //
                RCurly,   //
            ]
        };
        let output = Lexer::new(input).into_iter();
        for (expect, actual) in zip(expected, output) {
            let actual = actual.0;
            println!("{expect:?}|{actual:?}");
            assert_eq!(expect, actual)
        }
    }

    #[test]
    fn unexpected_character() {
        let input = "@@@@@@@";
        let on_error = |_: &mut Lexer, e, _| {
            assert!(matches!(e, LexicalError::UnexpectedCharacter(_)));
            false
        };
        let mut lexer = Lexer::with_error_behaviour(input, on_error);

        consume_lexer(&mut lexer);
        assert!(lexer.error_occurred);
    }

    #[test]
    fn integer_too_big() {
        let input = (i64::MAX as i128 + 1).to_string();
        let on_error = |_: &mut Lexer, e, _| {
            assert!(matches!(e, LexicalError::IntegerLiteralOutOfBounds(_)));
            false
        };
        let mut lexer = Lexer::with_error_behaviour(&input, on_error);
        consume_lexer(&mut lexer);
        assert!(lexer.error_occurred);
    }

    fn consume_lexer(lexer: &mut Lexer) {
        while let Some(tok) = lexer.next() {
            println!("{tok:?}");
        }
    }
}
