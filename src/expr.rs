use std::{
    f64,
    ops::{Add, Div, Mul, Neg, Sub},
};

use num_complex::{Complex, Complex64, ComplexFloat};
use pest::{iterators::Pairs, pratt_parser::PrattParser};

use crate::{ParserError, Rule};

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

fn builtin<T: ComplexFloat, U: Clone>(
    func: String,
    params: Vec<Expr<T, U>>,
) -> Result<Option<Expr<T, U>>, ParserError> {
    match func.as_str() {
        "sqrt" => {
            if params.len() != 1 {
                return Err(ParserError::InvalidParameterLength);
            }
            Ok(Some(Expr::sqrt(params[0].clone())))
        }
        _ => Ok(None),
    }
}

pub fn parse_expr(pairs: Pairs<Rule>) -> Result<Expr<Complex64, String>, ParserError> {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::num => Ok(Expr::Res(Complex::new(
                primary.as_str().parse().expect("primary num should parse"),
                0.0,
            ))),
            Rule::var => Ok(Expr::Var(primary.as_str().to_string())),
            Rule::pi => Ok(Expr::Res(Complex::new(f64::consts::PI, 0.0))),
            Rule::expr => parse_expr(primary.into_inner()),
            Rule::functionCall => {
                let mut pairs = primary.into_inner();
                let name = pairs.next().expect("function should have a name").as_str();

                let mut params_vec: Vec<Vec<_>> = Vec::new();
                while let Some(parameters) = pairs.next() {
                    let param_pairs = parameters.into_inner();
                    let params: Vec<_> = param_pairs
                        .map(|param| parse_expr(param.into_inner()))
                        .collect();
                    if params.iter().any(|p| p.is_err()) {
                        todo!("return error when param errors")
                    }
                    params_vec.push(params.into_iter().map(|param| param.unwrap()).collect());
                }

                let mut last = None;
                for pv in params_vec {
                    println!("pv is {pv:#?}");
                    match last {
                        None => {
                            let params = pv.iter().map(|param| param.clone()).collect();
                            if let Some(builtin) = builtin(name.to_string(), params)? {
                                return Ok(builtin);
                            }
                            last = Some(Expr::IFunc(
                                name.to_string(),
                                pv.iter().map(|param| param.clone()).collect(),
                            ))
                        }
                        Some(l) => {
                            last = Some(Expr::RFunc(
                                l.into(),
                                pv.iter().map(|param| param.clone()).collect(),
                            ))
                        }
                    }
                }

                Ok(last.expect("function can not exist without at least 1 set of parameters"))
            }
            rule => unreachable!("Expr::parse expected primary, found {rule:?}"),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::negate => Ok(-(rhs?)),
            rule => unreachable!("Expr::parse expected prefix, found {rule:?}"),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::imaginary => Ok(lhs? * Complex::new(0.0, 1.0).into()),
            rule => unreachable!("Expr::parse expected postfix, found {rule:?}"),
        })
        .map_infix(|lhs, op, rhs| {
            let lhs = lhs?;
            let rhs = rhs?;
            let res = match op.as_rule() {
                Rule::add => lhs + rhs,
                Rule::subtract => lhs - rhs,
                Rule::multiply => lhs * rhs,
                Rule::divide => lhs / rhs,
                Rule::power => lhs.powc(rhs),
                rule => unreachable!("Expr::parse expected infix, found {rule:?}"),
            };
            Ok(res)
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
    IFunc(String, Vec<Expr<T, U>>),
    RFunc(Box<Expr<T, U>>, Vec<Expr<T, U>>),
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
            (Expr::Res(s), Expr::Res(r)) => Expr::Res(s / r),
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

#[cfg(test)]
mod test {
    use num_complex::Complex64;
    use pest::Parser;

    use crate::{
        expr::{parse_expr, Complex},
        QParser, Rule,
    };
    use std::f64;

    use super::Expr;

    const IMAGINARY: Complex64 = Complex::new(0.0, 1.0);

    fn test_frame_expr(input: &str, correct: Expr<Complex64, String>) {
        let pairs = QParser::parse(Rule::expr, input).unwrap();
        println!("pairs: {pairs:#?}");
        assert_eq!(parse_expr(pairs).unwrap(), correct)
    }

    #[test]
    fn parser_add() {
        test_frame_expr("5+3", Expr::Res(8.0.into()));
    }

    #[test]
    fn parser_multiply() {
        test_frame_expr("5*3", Expr::Res(15.0.into()));
    }

    #[test]
    fn parser_negate() {
        test_frame_expr("4 * -3", Expr::Res((-12.0).into()));
    }

    #[test]
    fn parser_divide() {
        test_frame_expr("3/4", Expr::Res((0.75).into()));
    }

    #[test]
    fn parser_subtract() {
        test_frame_expr("4-3", Expr::Res(1.0.into()));
    }

    #[test]
    fn parser_pi() {
        test_frame_expr("pi", Expr::Res(f64::consts::PI.into()));
    }

    #[test]
    fn parser_sqrt() {
        test_frame_expr("sqrt(2)", Expr::Res(f64::consts::SQRT_2.into()));
    }

    #[test]
    fn parser_variable() {
        test_frame_expr(
            "number / 3",
            Expr::DualOp(
                Expr::Var("number".to_string()).into(),
                super::DualOp::Div,
                Expr::Res(3.0.into()).into(),
            ),
        );
    }

    #[test]
    fn parser_function() {
        test_frame_expr(
            "some_function() + 1i",
            Expr::DualOp(
                Expr::IFunc("some_function".to_string(), vec![]).into(),
                super::DualOp::Add,
                Expr::Res(IMAGINARY).into(),
            ),
        );
    }

    #[test]
    fn parser_function_with_param() {
        test_frame_expr(
            "some_function(3*4) + 1i",
            Expr::DualOp(
                Expr::IFunc("some_function".to_string(), vec![Expr::Res(12.0.into())]).into(),
                super::DualOp::Add,
                Expr::Res(IMAGINARY).into(),
            ),
        );
    }

    #[test]
    fn parser_function_with_params() {
        test_frame_expr(
            "some_function(param1, 2*3, param3)",
            Expr::IFunc(
                "some_function".to_string(),
                vec![
                    Expr::Var("param1".to_string()),
                    Expr::Res(6.0.into()),
                    Expr::Var("param3".to_string()),
                ],
            ),
        );
    }

    #[test]
    fn parser_double_function() {
        test_frame_expr(
            "some_meta_function(3)(2.5*3)",
            Expr::RFunc(
                Expr::IFunc(
                    "some_meta_function".to_string(),
                    vec![Expr::Res(3.0.into())],
                )
                .into(),
                vec![Expr::Res(7.5.into())],
            ),
        );
    }
}
