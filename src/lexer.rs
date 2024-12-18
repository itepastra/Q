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
    pub(super) fn get_token(&mut self) -> Result<Token, LexerError> {
        // Skip all the whitespace until something important starts again
        while self.get_char()?.is_whitespace() {
            self.pos += 1;
        }
        todo!()
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Token;
    use test_case::test_case;

    use super::Lexer;
}
