use core::f64;
use std::io::{stdin, BufRead, Read};
use std::ops::{Add, Div, Mul, Neg, Sub};

use num_complex::{Complex, Complex64, ComplexFloat};
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::PrattParser;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "src/grammar.pest"]
struct QParser;

#[derive(Debug, PartialEq, Clone, Copy)]
enum SingleOp {
    Imaginary,
    Negate,
    Sqrt,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum DualOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

#[derive(Debug, PartialEq, Clone)]
enum Expr<T, U> {
    Res(T),
    Var(U),
    Func(String, Vec<Expr<T, U>>),
    SingleOp(SingleOp, Box<Expr<T, U>>),
    DualOp(Box<Expr<T, U>>, DualOp, Box<Expr<T, U>>),
}

impl<T: ComplexFloat, U> Expr<T, U> {
    fn sqrt(self) -> Expr<T, U> {
        match self {
            Expr::Res(s) => s.sqrt().into(),
            s => Expr::SingleOp(SingleOp::Sqrt, s.into()),
        }
    }
}

impl<U> Expr<Complex64, U> {
    fn powc(self, rhs: Expr<Complex64, U>) -> Expr<Complex64, U> {
        match (self, rhs) {
            (Expr::Res(s), Expr::Res(r)) => s.powc(r).into(),
            (l, r) => Expr::DualOp(l.into(), DualOp::Pow, r.into()),
        }
    }
}

impl<T, U> From<T> for Expr<T, U> {
    fn from(value: T) -> Self {
        Expr::Res(value)
    }
}

impl<T: Mul<Output = T>, U> Mul<Expr<T, U>> for Expr<T, U> {
    type Output = Expr<T, U>;

    fn mul(self, rhs: Expr<T, U>) -> Self::Output {
        match (self, rhs) {
            (Expr::Res(s), Expr::Res(r)) => Expr::Res(r * s),
            (l, r) => Expr::DualOp(l.into(), DualOp::Mul, r.into()),
        }
    }
}

impl<T: Div<Output = T>, U> Div<Expr<T, U>> for Expr<T, U> {
    type Output = Expr<T, U>;

    fn div(self, rhs: Expr<T, U>) -> Self::Output {
        match (self, rhs) {
            (Expr::Res(s), Expr::Res(r)) => Expr::Res(r / s),
            (l, r) => Expr::DualOp(l.into(), DualOp::Div, r.into()),
        }
    }
}

impl<T: Sub<Output = T>, U> Sub<Expr<T, U>> for Expr<T, U> {
    type Output = Expr<T, U>;

    fn sub(self, rhs: Expr<T, U>) -> Self::Output {
        match (self, rhs) {
            (Expr::Res(s), Expr::Res(r)) => Expr::Res(r - s),
            (l, r) => Expr::DualOp(l.into(), DualOp::Sub, r.into()),
        }
    }
}

impl<T: Add<Output = T>, U> Add<Expr<T, U>> for Expr<T, U> {
    type Output = Expr<T, U>;

    fn add(self, rhs: Expr<T, U>) -> Self::Output {
        match (self, rhs) {
            (Expr::Res(s), Expr::Res(r)) => Expr::Res(r + s),
            (l, r) => Expr::DualOp(l.into(), DualOp::Add, r.into()),
        }
    }
}

impl<T: Neg<Output = T>, U> Neg for Expr<T, U> {
    type Output = Expr<T, U>;

    fn neg(self) -> Self::Output {
        match self {
            Expr::Res(s) => Expr::Res(-s),
            s => Expr::SingleOp(SingleOp::Negate, s.into()),
        }
    }
}

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        PrattParser::new()
        .op(Op::infix(add, Left) | Op::infix(subtract, Left))
        .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
        .op(Op::infix(power, Right))
        .op(Op::postfix(imaginary))
        .op(Op::prefix(subtract))
    };
}

fn parse_expr(pairs: Pairs<Rule>) -> Expr<Complex64, String> {
    eprintln!("pairs are: {pairs:#?}");
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::num => Expr::Res(Complex::new(primary.as_str().parse().unwrap(), 0.0)),
            Rule::pi => Expr::Res(Complex::new(f64::consts::PI, 0.0)),
            Rule::expr => parse_expr(primary.into_inner()),
            Rule::functionCall => {
                let mut pairs = primary.into_inner();
                let name = pairs.next().expect("function has a name").as_str();
                let params: Vec<_> = pairs
                    .next()
                    .into_iter()
                    .map(|pair| parse_expr(pair.into_inner()))
                    .collect();
                match name {
                    // builtin math functions
                    "sqrt" => {
                        assert!(params.len() == 1, "sqrt takes 1 argument");
                        params[0].clone().sqrt()
                    }
                    name => Expr::Func(name.to_string(), params),
                }
            }
            rule => unreachable!("Expr::parse expected primary, found {rule:?}"),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::subtract => -rhs,
            rule => unreachable!("Expr::parse expected prefix, found {rule:?}"),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::imaginary => lhs * Complex::new(0.0, 1.0).into(),
            rule => unreachable!("Expr::parse expected postfix, found {rule:?}"),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::add => lhs + rhs,
            Rule::subtract => lhs - rhs,
            Rule::multiply => lhs * rhs,
            Rule::divide => lhs / rhs,
            Rule::power => lhs.powc(rhs),
            rule => unreachable!("Expr::parse expected infix, found {rule:?}"),
        })
        .parse(pairs)
}

#[derive(Debug)]
enum ParserError {}

#[derive(Debug)]
enum Value {
    Expression(Expr<Complex64, String>),
    Ket(Expr<Complex64, String>, Expr<Complex64, String>),
}

#[derive(Debug)]
struct Variable {
    name: String,
    value: Value,
}

#[derive(Debug)]
struct Procedure {}

#[derive(Debug)]
struct Unitary {}

#[derive(Debug)]
struct Qubit {
    name: String,
    value: Value,
}

#[derive(Debug, Default)]
struct Program {
    variables: Vec<Variable>,
    procedures: Vec<Procedure>,
    unitaries: Vec<Unitary>,
    qubits: Vec<Qubit>,
}

impl Program {
    fn parse_ket(&self, pair: Pair<Rule>) -> Result<Value, ParserError> {
        match pair.as_rule() {
            Rule::ket => {
                let mut parts = pair.into_inner();
                let w0 = parse_expr(
                    parts
                        .next()
                        .expect("ket has expression on the left")
                        .into_inner(),
                );
                let w1 = parse_expr(
                    parts
                        .next()
                        .expect("ket has expression on the right")
                        .into_inner(),
                );
                Ok(Value::Ket(w0, w1))
            }
            rule => unreachable!("expected a ket, found {rule:#?}"),
        }
    }

    fn parse_variable_assignment(&self, pair: Pair<Rule>) -> Result<Variable, ParserError> {
        match pair.as_rule() {
            Rule::ketAssignment => {
                let mut parts = pair.clone().into_inner();
                let name = parts.next().expect("assignment has identifier");
                let ket = self.parse_ket(parts.next().expect("ket assignment has a ket"))?;
                Ok(Variable {
                    name: name.to_string(),
                    value: ket,
                })
            }
            rule => unreachable!("expected a variable assignment, found {rule:#?}"),
        }
    }

    fn parse_single_qubit(&self, pair: Pair<Rule>) -> Result<Qubit, ParserError> {
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
                Ok(Qubit {
                    name: name.to_string(),
                    value: val,
                })
            }
            rule => unreachable!("expected a qubit assignment, found {rule:#?}"),
        }
    }

    fn parse(&mut self, pairs: Pairs<Rule>) -> () {
        for pair in pairs {
            match pair.as_rule() {
                Rule::variableAssignment => self.variables.push(
                    self.parse_variable_assignment(pair.into_inner().next().unwrap())
                        .expect("variable assignment not correct"),
                ),
                Rule::singleQbitAssignment => {
                    println!("{pair:#?}");
                    self.qubits.push(
                        self.parse_single_qubit(pair)
                            .expect("qubit assignment not correct"),
                    )
                }
                rule => unreachable!("expected a statement, found {rule:#?}"),
            }
        }
    }
}

fn main() {
    println!("Hello, world!");

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
