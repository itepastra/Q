use std::{iter::repeat_n, num::ParseIntError};

type Integer = i64;
type Name = String;

#[derive(Debug, PartialEq)]
pub enum LexerError {
    OutOfRangeError,
}

trait SmartChar {
    fn is_newline(self) -> bool;
}

impl SmartChar for char {
    fn is_newline(self) -> bool {
        self == '\n'
    }
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

    fn get_char_at_offset(&self, offset: i32) -> Result<char, LexerError> {
        tracing::trace!("getting char at {}", (self.pos as i32 + offset) as usize);
        match self.chars.get((self.pos as i32 + offset) as usize) {
            None => Err(LexerError::OutOfRangeError),
            Some(character) => Ok(*character),
        }
    }

    fn get_repeat_len(&self, target: char) -> Result<usize, LexerError> {
        let mut len = 0;
        while self.get_char_at_offset(len as i32) == Ok(target) {
            len += 1;
        }
        tracing::debug!("found {} '{}' in a row", len, target);
        Ok(len)
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

    #[test_case("*", 1; "single star")]
    #[test_case("*********", 9; "many stars")]
    #[test_case("***/**", 3; "interrupted stars")]
    #[test_case("** ** ** ****", 2; "multiple star gaps")]
    #[test_case("/* */", 0; "no star at start")]
    #[test_case("", 0; "empty")]
    fn run_length(input: &'static str, correct: usize) {
        let lexer = Lexer {
            chars: input.chars().collect(),
            pos: 0,
        };
        assert_eq!(lexer.get_repeat_len('*').unwrap(), correct)
    }

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
