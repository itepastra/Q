use core::f64;
use std::{
    iter::repeat_n,
    num::{ParseFloatError, ParseIntError},
};

type Integer = i64;
type Floating = f64;
type Name = String;

#[derive(Debug, PartialEq)]
pub enum LexerError {
    OutOfRangeError,
    ParseIntError,
    MultipleDotsError,
    ParseFloatError,
}

impl From<ParseIntError> for LexerError {
    fn from(_value: ParseIntError) -> Self {
        Self::ParseIntError
    }
}

impl From<ParseFloatError> for LexerError {
    fn from(_value: ParseFloatError) -> Self {
        Self::ParseFloatError
    }
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
pub(crate) enum Token {
    OpenBrace,
    CloseBrace,
    OpenBrack,
    CloseBrack,
    OpenParen,
    CloseParen,
    Equals,
    Add,
    Subtract,
    Multiply,
    Divide,
    Imaginary,
    Function,
    Use,
    Namespace,
    Ident(Name),
    Integer(Integer),
    Floating(Floating),
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
        while self.get_char()?.is_alphanumeric() || "_-".contains(self.get_char()?) {
            identifier.push(self.get_char()?);
            self.pos += 1;
        }

        match identifier.as_str() {
            "pi" => Ok(Token::Floating(f64::consts::PI)),
            "e" => Ok(Token::Floating(f64::consts::E)),
            "i" => Ok(Token::Imaginary),
            "fn" => Ok(Token::Function),
            "use" => Ok(Token::Use),
            _ => Ok(Token::Ident(identifier)),
        }
    }

    fn parse_number(&mut self) -> Result<Token, LexerError> {
        let mut number = String::new();
        let mut is_float = false;
        while self.get_char().is_ok_and(|c| "0123456789._".contains(c)) {
            match self.get_char() {
                Ok('_') => self.pos += 1,
                Ok('.') => {
                    if is_float {
                        return Err(LexerError::MultipleDotsError);
                    }
                    is_float = true;
                    number.push('.');
                    self.pos += 1;
                }
                Ok(c) => {
                    number.push(c);
                    self.pos += 1;
                }
                Err(err) => return Err(err.into()),
            }
        }
        if is_float {
            match number.parse() {
                Ok(num) => Ok(Token::Floating(num)),
                Err(err) => Err(err.into()),
            }
        } else {
            match number.parse() {
                Ok(num) => Ok(Token::Integer(num)),
                Err(err) => Err(err.into()),
            }
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

    fn match_single_tokens(&mut self) -> Result<Option<Token>, LexerError> {
        let ret = match self.get_char()? {
            '(' => Ok(Some(Token::OpenParen)),
            ')' => Ok(Some(Token::CloseParen)),
            '{' => Ok(Some(Token::OpenBrace)),
            '}' => Ok(Some(Token::CloseBrace)),
            '[' => Ok(Some(Token::OpenBrack)),
            ']' => Ok(Some(Token::CloseBrack)),
            '=' => Ok(Some(Token::Equals)),
            '+' => Ok(Some(Token::Add)),
            '-' => Ok(Some(Token::Subtract)),
            '*' => Ok(Some(Token::Multiply)),
            '/' => Ok(Some(Token::Divide)),
            _ => return Ok(None),
        };
        self.pos += 1;
        ret
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

        // parse the :: token, for namespace access
        if self.get_char()? == ':' && self.get_char_at_offset(1)? == ':' {
            // skip to the end of the ::
            self.pos += 2;
            return Ok(Token::Namespace);
        }

        // Now we can check all the single character tokens
        if let Some(tok) = self.match_single_tokens()? {
            return Ok(tok);
        }

        // If the first character is alphabetic that's an identifier (possibly keyword) start
        if self.get_char()?.is_alphabetic() {
            return self.parse_identifier();
        }

        // Numbers start with a digit, maybe also with a -
        // TODO: allow numbers to start with + or -, possibly by putting before identifier
        if self.get_char()?.is_digit(10) || self.get_char()? == '-' {
            return self.parse_number();
        }

        todo!()
    }
}

#[cfg(test)]
mod test {
    use core::f64;

    use crate::lexer::Token;
    use test_case::test_case;

    use super::{Floating, Integer, Lexer};

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
                assert_eq!(lexer.get_token().unwrap(), Token::Ident(id.to_string()));
                assert_eq!(lexer.pos, id.chars().count());
            }
            None => assert_eq!(lexer.get_token(), Err(super::LexerError::OutOfRangeError)),
        }
    }

    #[test_case("29210", 29210, 5; "positive integer")]
    #[test_case("29210 and then some text", 29210, 5; "positive integer with trailing")]
    fn integer_lexing(input: &'static str, correct: Integer, correct_position: usize) {
        let mut lexer = Lexer {
            chars: input.chars().collect(),
            pos: 0,
        };
        assert_eq!(lexer.get_token().unwrap(), Token::Integer(correct));
        assert_eq!(lexer.pos, correct_position);
    }

    #[test_case("29210.3", 29210.3, 7; "positive float")]
    #[test_case("29210.3 and then some text", 29210.3, 7; "positive float with trailing")]
    fn float_lexing(input: &'static str, correct: Floating, correct_position: usize) {
        let mut lexer = Lexer {
            chars: input.chars().collect(),
            pos: 0,
        };
        assert_eq!(lexer.get_token().unwrap(), Token::Floating(correct));
        assert_eq!(lexer.pos, correct_position);
    }

    #[test_case("// this is a comment", "this is a comment", 20; "single line simple comment")]
    #[test_case("/* this is a comment */", "this is a comment", 23; "single line star comment")]
    #[test_case(r#"/* this is a
    multiline comment */"#, r#"this is a
    multiline comment"#, 37; "multiline line star comment")]
    #[test_case("/** multi star comment **/", "multi star comment", 26; "single line stars comment")]
    #[test_case("/** multi /* star */ comment **/ and this is not a comment anymore **/", "multi /* star */ comment", 32; "single line hard stars comment")]
    #[test_case("//", "", 2; "empty single line comment")]
    #[test_case("/* */and then there can be code", "", 5; "empty single star comment")]
    fn comment_lexing(input: &'static str, correct: &'static str, correct_position: usize) {
        let mut lexer = Lexer {
            chars: input.chars().collect(),
            pos: 0,
        };

        assert_eq!(
            lexer.get_token().unwrap(),
            Token::Comment(correct.to_string())
        );
        assert_eq!(lexer.pos, correct_position)
    }

    #[test]
    fn test_lexer() {
        let input = r#"
use std::rot_y
// this is a comment
x0 = 1 + i
x2 = 2+0.1i

/* do hadamard gate or something */
fn hadamard() {
    rot_y(pi/2)
    rot_x(pi)
}
"#;
        let mut lexer = Lexer {
            chars: input.chars().collect(),
            pos: 0,
        };
        assert_eq!(lexer.get_token().unwrap(), Token::Use);
        assert_eq!(lexer.get_token().unwrap(), Token::Ident("std".to_string()));
        assert_eq!(lexer.get_token().unwrap(), Token::Namespace);
        assert_eq!(
            lexer.get_token().unwrap(),
            Token::Ident("rot_y".to_string())
        );
        assert_eq!(
            lexer.get_token().unwrap(),
            Token::Comment("this is a comment".to_string())
        );
        assert_eq!(lexer.get_token().unwrap(), Token::Ident("x0".to_string()));
        assert_eq!(lexer.get_token().unwrap(), Token::Equals);
        assert_eq!(lexer.get_token().unwrap(), Token::Integer(1));
        assert_eq!(lexer.get_token().unwrap(), Token::Add);
        assert_eq!(lexer.get_token().unwrap(), Token::Imaginary);
        assert_eq!(lexer.get_token().unwrap(), Token::Ident("x2".to_string()));
        assert_eq!(lexer.get_token().unwrap(), Token::Equals);
        assert_eq!(lexer.get_token().unwrap(), Token::Integer(2));
        assert_eq!(lexer.get_token().unwrap(), Token::Add);
        assert_eq!(lexer.get_token().unwrap(), Token::Floating(0.1));
        assert_eq!(lexer.get_token().unwrap(), Token::Imaginary);
        assert_eq!(
            lexer.get_token().unwrap(),
            Token::Comment("do hadamard gate or something".to_string())
        );
        assert_eq!(lexer.get_token().unwrap(), Token::Function);
        assert_eq!(
            lexer.get_token().unwrap(),
            Token::Ident("hadamard".to_string())
        );
        assert_eq!(lexer.get_token().unwrap(), Token::OpenParen);
        assert_eq!(lexer.get_token().unwrap(), Token::CloseParen);
        assert_eq!(lexer.get_token().unwrap(), Token::OpenBrace);
        assert_eq!(
            lexer.get_token().unwrap(),
            Token::Ident("rot_y".to_string())
        );
        assert_eq!(lexer.get_token().unwrap(), Token::OpenParen);
        assert_eq!(lexer.get_token().unwrap(), Token::Floating(f64::consts::PI));
        assert_eq!(lexer.get_token().unwrap(), Token::Divide);
        assert_eq!(lexer.get_token().unwrap(), Token::Integer(2));
        assert_eq!(lexer.get_token().unwrap(), Token::CloseParen);
        assert_eq!(
            lexer.get_token().unwrap(),
            Token::Ident("rot_x".to_string())
        );
        assert_eq!(lexer.get_token().unwrap(), Token::OpenParen);
        assert_eq!(lexer.get_token().unwrap(), Token::Floating(f64::consts::PI));
        assert_eq!(lexer.get_token().unwrap(), Token::CloseParen);
        assert_eq!(lexer.get_token().unwrap(), Token::CloseBrace);
        assert_eq!(
            lexer.get_token(),
            Err(crate::lexer::LexerError::OutOfRangeError)
        )
    }
}
