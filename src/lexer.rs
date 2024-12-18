use std::{iter::repeat_n, num::ParseIntError};

type Integer = i64;
type Name = String;

#[derive(Debug, PartialEq)]
pub enum LexerError {
    OutOfRangeError,
}


#[derive(Debug, PartialEq)]
pub enum Token {
    Function,
    Ident(Name),
    Integer(Integer),
    Comment(String),
}

#[derive(Debug, PartialEq)]
struct Lexer {
    chars: Vec<char>,
    pos: usize,
}

impl Lexer {
    fn get_char(&self) -> Result<char, LexerError> {
        tracing::trace!("getting char at {}", self.pos);
        match self.chars.get(self.pos) {
            None => Err(LexerError::OutOfRangeError),
            Some(character) => Ok(*character),
        }
    }
    fn parse_identifier(&mut self) -> Result<Token, LexerError> {
        let mut identifier = String::new();
        while self.get_char()?.is_alphanumeric() {
            identifier.push(self.get_char()?);
            self.pos += 1;
        }

        match identifier.as_str() {
            "fn" => Ok(Token::Function),
            _ => Ok(Token::Ident(identifier)),
        }
    }

    pub(super) fn get_token(&mut self) -> Result<Token, LexerError> {
        // Skip all the whitespace until something important starts again
        while self.get_char()?.is_whitespace() {
            self.pos += 1;
        }
        // If the first character is alphabetic that's an identifier (possibly keyword) start
        if self.get_char()?.is_alphabetic() {
            return self.parse_identifier();
        }

        todo!()
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Token;
    use test_case::test_case;

    use super::Lexer;
    #[test_case("ident with other stuff", Some("ident"); "when only letters")]
    #[test_case("id3nt with a number", Some("id3nt"); "when there is a number")]
    #[test_case("ιδεντ in greek", Some("ιδεντ"); "when there are funky letters")]
    #[test_case("", None; "illegal when there is no ident at all")]
    #[test_case("ident", None; "illegal when the ident is the last thing")]
    fn identifier_lexing(input: &'static str, identifier: Option<&'static str>) {
        let mut lexer = Lexer {
            chars: input.chars().collect(),
            pos: 0,
        };
        match identifier {
            Some(id) => {
                assert_eq!(
                    lexer.parse_identifier().unwrap(),
                    Token::Ident(id.to_string())
                );
                assert_eq!(lexer.pos, id.chars().count());
            }
            None => assert_eq!(
                lexer.parse_identifier(),
                Err(super::LexerError::OutOfRangeError)
            ),
        }
    }
}
