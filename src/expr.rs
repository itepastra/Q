use std::{
    f64,
    ops::{Add, Div, Mul, Neg, Sub},
};

use num_complex::{Complex, Complex64, ComplexFloat};
use pest::{iterators::Pairs, pratt_parser::PrattParser};

use crate::Rule;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        PrattParser::new()
        .op(Op::infix(add, Left) | Op::infix(subtract, Left))
        .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
        .op(Op::infix(power, Right))
        .op(Op::postfix(imaginary))
        .op(Op::prefix(negate))
    };
}

pub fn parse_expr(pairs: Pairs<Rule>) -> Expr<Complex64, String> {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::num => Expr::Res(Complex::new(
                primary.as_str().parse().expect("primary num should parse"),
                0.0,
            )),
            Rule::var => Expr::Var(primary.as_str().to_string()),
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
            Rule::negate => -rhs,
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
pub(crate) enum Expr<T, U> {
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
            (Expr::Res(s), Expr::Res(r)) => Expr::Res(s - r),
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
