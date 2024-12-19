#![allow(dead_code)]

// ^ remove when the parser is acutally used
use crate::{
    lexer::Token,
    types::{Complex, Floating, Integer, Name},
};

mod expressions;
mod functions;
mod prototypes;

enum ParseError {
    Unimplemented,
    // Unexpected: expected {token}, got {token} (in that order)
    Unexpected(Token, Token),
    UnexpectedArr(Vec<Token>, Token),
    // Unexpected {token}, no guess though
    Unknown(Token),
    StreamEnded,
}

enum AST {
    Expr(expressions::AST),
    Prototype(prototypes::AST),
    Function(functions::AST),
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn get_current(&self) -> Result<Token, ParseError> {
        match self.tokens.get(self.pos) {
            Some(tok) => Ok(tok.clone()),
            None => Err(ParseError::StreamEnded),
        }
    }

    fn get_and_increment(&mut self) -> Result<Token, ParseError> {
        let tok = self.get_current();
        self.inc();
        tok
    }

    fn inc(&mut self) {
        self.pos += 1
    }

    fn get_at_offset(&self, offset: i64) -> Result<Token, ParseError> {
        match self.tokens.get((self.pos as i64 + offset) as usize) {
            Some(tok) => Ok(tok),
            None => Err(ParseError::StreamEnded),
        }
    }

    fn parse_number(&mut self) -> Result<expressions::AST, ParseError> {
        let ast = match self.get_current()? {
            // number starts with `+`, ignore the `+`
            Token::Add => {
                self.pos += 1;
                self.parse_number()?
            }
            // number starts with `-`, negate the number
            Token::Subtract => {
                self.pos += 1;
                expressions::AST::Number(expressions::Number::Negate(self.parse_expr()?.into()))
            }
            // token is `i` => if not parsed explicitly, assume it means `0+1i`
            Token::Imaginary => {
                self.pos += 1;
                expressions::AST::Number(expressions::Number::Complex(num_complex::Complex {
                    re: 0.0,
                    im: 1.0,
                }))
            }
            Token::Integer(num) => {
                let mut skip_amount = 1;
                // if a number is just before an `i` it should be counted as a complex number
                let ast = if let Ok(Token::Imaginary) = self.get_at_offset(1) {
                    skip_amount = 2;
                    expressions::AST::Number(expressions::Number::Complex(num_complex::Complex {
                        re: 0.0,
                        im: num as f64,
                    }))
                } else {
                    expressions::AST::Number(expressions::Number::Integer(num))
                };
                self.pos += skip_amount; // also skip the `i`
                ast
            }
            Token::Floating(num) => {
                let mut skip_amount = 1;
                // if a number is just before an `i` it should be counted as a complex number
                let ast = if let Ok(Token::Imaginary) = self.get_at_offset(1) {
                    skip_amount = 2;
                    expressions::AST::Number(expressions::Number::Complex(num_complex::Complex {
                        re: 0.0,
                        im: num,
                    }))
                } else {
                    expressions::AST::Number(expressions::Number::Floating(num))
                };
                self.pos += skip_amount;
                ast
            }
            tok => unreachable!("token {tok:?} is not a start of number"),
        };

        Ok(ast)
    }

    fn parse_paren(&mut self) -> Result<expressions::AST, ParseError> {
        // this got called with a paren at the current position already, skip it
        self.pos += 1;

        let ast = self.parse_expr()?;

        if self.get_and_increment()? != Token::CloseParen {
            return Err(ParseError::Unexpected(
                Token::CloseParen,
                self.get_at_offset(-1)?.clone(),
            ));
        }
        self.pos += 1;
        Ok(ast)
    }

    fn parse_ident(&mut self) -> Result<expressions::AST, ParseError> {
        if let Token::Ident(name) = self.get_and_increment()? {
            // check if the next token is `(`, that'd make it a function call
            if let Token::OpenParen = self.get_current()? {
                // can't use `get_and_increment` since it shouldn't happen if there isnt a paren
                self.inc();
                let mut args = Vec::new();
                // waa()
                loop {
                    let arg = self.parse_expr()?;
                    args.push(arg);

                    if self.get_current()? == Token::CloseParen {
                        // increment so next parse_expr has the correct starting state
                        self.inc();
                        return Ok(expressions::AST::Call(expressions::Call::Function(
                            name.to_string(),
                            args,
                        )));
                    }

                    if self.get_and_increment()? != Token::Seperator {
                        // between expressions there should be commas, or the parameters should
                        // close
                        return Err(ParseError::UnexpectedArr(
                            vec![Token::Seperator, Token::CloseParen],
                            self.get_at_offset(-1)?,
                        ));
                    }
                }
            } else {
                Ok(expressions::AST::Variable(expressions::Variable::Compile(
                    name.to_string(),
                )))
            }
        } else {
            unreachable!("parse_ident called without an ident at current position")
        }
    }

    fn parse_expr(&mut self) -> Result<expressions::AST, ParseError> {
        match self.get_current()? {
            Token::Add
            | Token::Subtract
            | Token::Imaginary
            | Token::Integer(_)
            | Token::Floating(_) => self.parse_number(),
            Token::OpenParen => self.parse_paren(),
            Token::Ident(_) => self.parse_ident(),
            tok => Err(ParseError::Unknown(tok.clone())),
        }
    }

    pub(crate) fn parse(&mut self) -> Result<AST, ParseError> {
        Err(ParseError::Unimplemented)
    }
}
