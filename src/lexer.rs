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

    fn parse_comment(&mut self) -> Result<Option<Token>, LexerError> {
        let original = self.pos;
        let mut comment = String::new();
        // go to second slash position
        self.pos += 1;
        if self.get_char()? == '/' {
            self.pos += 1;
            while !self.get_char().unwrap_or('\n').is_newline() {
                comment.push(self.get_char()?);
                self.pos += 1;
            }
            return Ok(Some(Token::Comment(comment.trim().to_string())));
        }
        let star_count = self.get_repeat_len('*')?;
        if star_count == 0 {
            self.pos = original;
            return Ok(None);
        }
        self.pos += star_count;
        loop {
            if self.get_repeat_len('*')? == star_count {
                self.pos += star_count;
                if self.get_char()? == '/' {
                    self.pos += 1;
                    return Ok(Some(Token::Comment(comment.trim().to_string())));
                } else {
                    comment.extend(repeat_n('*', star_count));
                }
            }
            comment.push(self.get_char()?);
            self.pos += 1;
        }
    }
    pub(super) fn get_token(&mut self) -> Result<Token, LexerError> {
        // Skip all the whitespace until something important starts again
        while self.get_char()?.is_whitespace() {
            self.pos += 1;
        }

        // I want // to be a single line comment, and /* */ multiline, with any amount of stars
        if self.get_char()? == '/' {
            match self.parse_comment() {
                Ok(Some(token)) => return Ok(token),
                Ok(None) => {}
                Err(err) => return Err(err),
            }
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

    #[test_case("// this is a comment", "this is a comment", 20; "single line simple comment")]
    #[test_case("/* this is a comment */", "this is a comment", 23; "single line star comment")]
    #[test_case(r#"/* this is a
    multiline comment */"#, r#"this is a
    multiline comment"#, 37; "multiline line star comment")]
    #[test_case("/** multi star comment **/", "multi star comment", 26; "single line stars comment")]
    #[test_case("/** multi /* star */ comment **/", "multi /* star */ comment", 32; "single line hard stars comment")]
    fn comment_lexing(input: &'static str, correct: &'static str, correct_position: usize) {
        let mut lexer = Lexer {
            chars: input.chars().collect(),
            pos: 0,
        };

        assert_eq!(
            lexer.parse_comment().unwrap(),
            Some(Token::Comment(correct.to_string()))
        );
        assert_eq!(lexer.pos, correct_position)
    }
}
