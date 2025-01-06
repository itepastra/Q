mod expr;
mod ket;
mod matrix;
mod procedure;

use std::{
    collections::HashMap,
    io::{stdin, Read},
};

use expr::{parse_expr, Expr};
use matrix::Matrix;
use num_complex::Complex64;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "src/grammar.pest"]
struct QParser;

type Ident = String;

#[derive(Debug, PartialEq)]
enum ParserError {
    MalformedMatrix,
    EmptyMatrix,
}

#[derive(Debug, PartialEq, Clone)]
enum Value {
    Expression(Expr<Complex64, String>),
    Ket(Vec<Expr<Complex64, String>>),
}

#[derive(Debug, Clone)]
enum Unit {
    Matrix(Matrix<Expr<Complex64, String>>),
    Procedured(Procedure),
    Unitary(Unitary),
}

#[derive(Debug, PartialEq, Clone)]
struct Variable {
    value: Value,
}

#[derive(Debug, Clone)]
struct Procedure {
    parameters: Vec<Ident>,
    program: Program,
}

#[derive(Debug, Default, Clone)]
struct Unitary {
    parameters: Vec<Ident>,
    steps: Vec<(Unit, Vec<usize>)>,
}

#[derive(Debug, Clone)]
struct Qubit {
    value: Value,
}

#[derive(Debug, Default, Clone)]
struct Program {
    variables: HashMap<Ident, Variable>,
    procedures: HashMap<Ident, Procedure>,
    unitaries: HashMap<Ident, Unitary>,
    qubits: HashMap<Ident, Qubit>,
}

impl Program {
    fn parse_variable_assignment(
        &self,
        pairs: &mut Pairs<Rule>,
    ) -> Result<(Ident, Variable), ParserError> {
        let p = pairs.clone();
        let name = pairs
            .next()
            .expect(&format!("assignment should have an identifier, ({p:#?})"))
            .as_str()
            .to_string();
        let value = pairs
            .next()
            .expect(&format!("assignment should have an identifier, ({p:#?})"));
        match value.as_rule() {
            Rule::ketValue => {
                let ket = self.parse_ket(value.into_inner().next().expect("ketValue has ket"))?;
                Ok((name, Variable { value: ket }))
            }
            Rule::singleValue => {
                let value = parse_expr(value.into_inner());
                Ok((
                    name,
                    Variable {
                        value: Value::Expression(value),
                    },
                ))
            }
            rule => unreachable!("expected a variable assignment, found {rule:#?}"),
        }
    }

    fn parse_unitary(&self, pair: Pair<Rule>) -> Result<(Ident, Unitary), ParserError> {
        match pair.as_rule() {
            Rule::functionUnitary => {
                let mut parts = pair.into_inner();
                let name = parts.next().expect("unitary does not have a name");
                let parameters = parts
                    .next()
                    .expect("unitary does not have parameters")
                    .into_inner()
                    .map(|param| param.as_str().to_string())
                    .collect();
                let mut unit = Unitary {
                    parameters,
                    steps: Vec::new(),
                };

                while let Some(stmt) = parts.next() {
                    // TODO: parse statements for thingy
                }
                Ok((name.to_string(), unit))
            }
            Rule::matrixUnitary => {
                let mut parts = pair.into_inner();
                let name = parts.next().expect("matrix unitary does not have a name");
                let matrix = self.parse_matrix(
                    parts
                        .next()
                        .expect("matrix unitary has a matrix")
                        .into_inner(),
                );
                // TODO: decompose matrix to unitary
                Ok((name.to_string(), Unitary::default()))
            }
            rule => unreachable!("expected a unitary, found {rule:#?}"),
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
                Rule::functionUnitary | Rule::matrixUnitary => {
                    let (name, unitary) = self.parse_unitary(pair).expect("unitary not valid");
                    self.unitaries.insert(name, unitary);
                }
                Rule::procedure => {
                    let (name, procedure) =
                        self.parse_procedure(pair).expect("procedure not valid");
                    self.procedures.insert(name, procedure);
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

    use num_complex::{Complex, Complex64};
    use pest::Parser;

    use crate::{parse_expr, Expr, Program, QParser, Rule, Value, Variable};

    fn test_frame_expr(input: &str, correct: Expr<Complex64, String>) {
        let pairs = QParser::parse(Rule::expr, input).unwrap();
        println!("pairs: {pairs:#?}");
        assert_eq!(parse_expr(pairs), correct)
    }

    #[test]
    fn test_math_parser_add() {
        test_frame_expr("5+3", Expr::Res(Complex::new(8.0, 0.0)));
    }

    #[test]
    fn test_math_parser_multiply() {
        test_frame_expr("5*3", Expr::Res(Complex::new(15.0, 0.0)));
    }

    #[test]
    fn test_math_parser_negate() {
        test_frame_expr("4 * -3", Expr::Res(Complex::new(-12.0, 0.0)));
    }

    #[test]
    fn test_math_parser_subtract() {
        test_frame_expr("4-3", Expr::Res(Complex::new(1.0, 0.0)));
    }

    #[test]
    fn test_math_parser_pi() {
        test_frame_expr("pi", Expr::Res(Complex::new(f64::consts::PI, 0.0)));
    }

    #[test]
    fn test_math_parser_sqrt() {
        test_frame_expr("sqrt(2)", Expr::Res(Complex::new(f64::consts::SQRT_2, 0.0)));
    }

    fn test_frame_assignment(input: &str, name: &str, correct: Value) {
        let mut pairs = QParser::parse(Rule::variableAssignment, input)
            .unwrap()
            .next()
            .unwrap()
            .into_inner();
        let program = Program::default();
        let (n, v) = program.parse_variable_assignment(&mut pairs).unwrap();
        assert_eq!(v, Variable { value: correct });
        assert_eq!(n, name.to_string());
    }

    #[test]
    fn test_assignment_ket() {
        test_frame_assignment(
            "let wowa = (13,53 - 69)",
            "wowa",
            Value::Ket(vec![
                Expr::Res(Complex::new(13.0, 0.0)),
                Expr::Res(Complex::new(53.0 - 69.0, 0.0)),
            ]),
        );
    }

    #[test]
    fn test_assignment_number() {
        test_frame_assignment(
            "let br3e = 29 * 33 - 19i",
            "br3e",
            Value::Expression(Expr::Res(Complex::new(29.0 * 33.0, -19.0))),
        );
    }
}
