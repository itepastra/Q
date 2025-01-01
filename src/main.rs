mod expr;
use std::{
    collections::HashMap,
    io::{stdin, Read},
};

use expr::{parse_expr, Expr};
use num_complex::Complex64;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "src/grammar.pest"]
struct QParser;

type Ident = String;

#[derive(Debug)]
enum ParserError {}

#[derive(Debug)]
enum Value {
    Expression(Expr<Complex64, String>),
    Ket(Vec<Expr<Complex64, String>>),
}

#[derive(Debug)]
struct Variable {
    value: Value,
}

#[derive(Debug)]
struct Procedure {}

#[derive(Debug)]
struct Unitary {}

#[derive(Debug)]
struct Qubit {
    value: Value,
}

#[derive(Debug, Default)]
struct Program {
    variables: HashMap<Ident, Variable>,
    procedures: HashMap<Ident, Procedure>,
    unitaries: HashMap<Ident, Unitary>,
    qubits: HashMap<Ident, Qubit>,
}

impl Program {
    fn parse_ket(&self, pair: Pair<Rule>) -> Result<Value, ParserError> {
        let mut pairs = pair.into_inner();
        println!("ket pairs are {pairs:#?}");
        let p0 = pairs
            .next()
            .expect("ket should have an expression on the left")
            .into_inner();
        let p1 = pairs
            .next()
            .expect("ket should have an expression on the left")
            .into_inner();
        let w0 = parse_expr(p0);
        let w1 = parse_expr(p1);
        Ok(Value::Ket(vec![w0, w1]))
    }

    fn parse_variable_assignment(
        &self,
        pairs: &mut Pairs<Rule>,
    ) -> Result<(Ident, Variable), ParserError> {
        println!("variable assignment for pair {pairs:#?}");
        let name = pairs.next().expect("assignment should have an identifier");
        let value = pairs.next().expect("assignment should have a value");
        match value.as_rule() {
            Rule::ketValue => {
                let ket = self.parse_ket(value.into_inner().next().expect("ketValue has ket"))?;
                Ok((name.to_string(), Variable { value: ket }))
            }
            Rule::singleValue => {
                let value = parse_expr(value.into_inner());
                Ok((
                    name.to_string(),
                    Variable {
                        value: Value::Expression(value),
                    },
                ))
            }
            rule => unreachable!("expected a variable assignment, found {rule:#?}"),
        }
    }

    fn parse_single_qubit(&self, pair: Pair<Rule>) -> Result<(Ident, Qubit), ParserError> {
        match pair.as_rule() {
            Rule::singleQbitAssignment => {
                let mut parts = pair.clone().into_inner();
                let name = parts.next().expect("assignment has identifier");
                let value = parts.next().expect("assignment has value");
                let val = match value.as_rule() {
                    Rule::ket => self.parse_ket(value)?,
                    Rule::ident => Value::Expression(Expr::Var(name.to_string())),
                    rule => unreachable!("expected a qubit initialisation value, found {rule:#?}"),
                };
                Ok((name.to_string(), Qubit { value: val }))
            }
            rule => unreachable!("expected a qubit assignment, found {rule:#?}"),
        }
    }

    fn parse(&mut self, pairs: Pairs<Rule>) -> () {
        for pair in pairs {
            match pair.as_rule() {
                Rule::variableAssignment => {
                    let (name, val) = self
                        .parse_variable_assignment(&mut pair.into_inner())
                        .expect("variable assignment not correct");
                    self.variables.insert(name, val);
                }
                Rule::singleQbitAssignment => {
                    let (name, val) = self
                        .parse_single_qubit(pair)
                        .expect("qubit assignment not correct");
                    self.qubits.insert(name, val);
                }
                rule => unreachable!("expected a statement, found {rule:#?}"),
            }
        }
    }
}

fn main() {
    let mut file = String::new();
    match stdin().lock().read_to_string(&mut file) {
        Ok(len) => eprintln!("read input with length {len}"),
        Err(_) => todo!(),
    }
    eprintln!("input was: \n\n{file}");
    let mut program = Program::default();

    match QParser::parse(Rule::program, file.as_str()) {
        Ok(pairs) => {
            program.parse(pairs);
            println!("parsed: {:#?}", program)
        }
        Err(_) => todo!(),
    }
}

#[cfg(test)]
mod test {
    use core::f64;

    use num_complex::Complex;
    use pest::Parser;

    use crate::{parse_expr, Expr, QParser, Rule};

    #[test]
    fn test_math_parser_add() {
        let input = "5+3";
        let pairs = QParser::parse(Rule::expr, input).unwrap();
        assert_eq!(parse_expr(pairs), Expr::Res(Complex::new(8.0, 0.0)));
    }

    #[test]
    fn test_math_parser_multiply() {
        let input = "5*3";
        let pairs = QParser::parse(Rule::expr, input).unwrap();
        assert_eq!(parse_expr(pairs), Expr::Res(Complex::new(15.0, 0.0)));
    }

    #[test]
    fn test_math_parser_pi() {
        let input = "pi";
        let pairs = QParser::parse(Rule::expr, input).unwrap();
        assert_eq!(
            parse_expr(pairs),
            Expr::Res(Complex::new(f64::consts::PI, 0.0))
        );
    }

    #[test]
    fn test_math_parser_sqrt() {
        let input = "sqrt(2)";
        let pairs = QParser::parse(Rule::expr, input).unwrap();
        assert_eq!(
            parse_expr(pairs),
            Expr::Res(Complex::new(f64::consts::SQRT_2, 0.0))
        );
    }
}
