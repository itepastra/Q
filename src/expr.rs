use std::{
    collections::HashMap,
    f64,
    ops::{Add, Div, Mul, Neg, Sub},
};

use num_complex::{Complex, Complex64, ComplexFloat};
use pest::{iterators::Pairs, pratt_parser::PrattParser};

use crate::{
    function::parse_function_call, ket::Ket, Ident, MetaProgramError, ParserError, Procedure,
    Program, Qubit, Rule, Unitary, Value, Variable,
};

pub(crate) type Expression = Expr<Complex64, Ident>;

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

impl Expression {
    pub(crate) fn simplify(
        &self,
        variables: &HashMap<Ident, Variable>,
        qubits: &HashMap<Ident, Qubit>,
        procedures: &HashMap<Ident, Procedure>,
        unitaries: &HashMap<Ident, Unitary>,
    ) -> Result<Self, MetaProgramError> {
        println!("simplifying expression {self:#?}");
        match self {
            Expr::Index(_, expr) => expr.simplify(variables, qubits, procedures, unitaries),
            Expr::IFunc(ident, vec) => {
                let params: Vec<_> = vec
                    .into_iter()
                    .map(|ele| ele.simplify(variables, qubits, procedures, unitaries))
                    .collect::<Result<Vec<_>, _>>()?;
                if let Some(unit) = unitaries.get(ident) {
                    let simple = unit.simplify(variables, qubits, procedures, unitaries)?;
                    Ok(Expr::Unitary(simple, params))
                } else if let Some(proc) = procedures.get(ident) {
                    todo!("calculate procedure {proc:#?}, we have params {params:#?}")
                } else {
                    Ok(Expr::IFunc(ident.to_string(), params))
                }
            }
            Expr::RFunc(expr, vec) => Ok(Expr::RFunc(
                expr.simplify(variables, qubits, procedures, unitaries)?
                    .into(),
                vec.into_iter()
                    .map(|ele| ele.simplify(variables, qubits, procedures, unitaries))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Expr::SingleOp(single_op, expr) => {
                let simple = expr.simplify(variables, qubits, procedures, unitaries)?;
                if let Expr::Res(res) = simple {
                    Ok(Expr::Res(match single_op {
                        SingleOp::Negate => -res,
                        SingleOp::Sqrt => res.sqrt(),
                        SingleOp::NormSquared => res.norm_sqr().into(),
                    }))
                } else {
                    Ok(Expr::SingleOp(*single_op, simple.into()))
                }
            }
            Expr::DualOp(expr, dual_op, expr1) => {
                let simplel = expr.simplify(variables, qubits, procedures, unitaries)?;
                let simpler = expr1.simplify(variables, qubits, procedures, unitaries)?;
                if let (Expr::Res(lhs), Expr::Res(rhs)) = (simplel.clone(), simpler.clone()) {
                    Ok(Expr::Res(match dual_op {
                        DualOp::Add => lhs + rhs,
                        DualOp::Sub => lhs - rhs,
                        DualOp::Mul => lhs * rhs,
                        DualOp::Div => lhs / rhs,
                        DualOp::Pow => lhs.powc(rhs),
                    }))
                } else {
                    Ok(Expr::DualOp(simplel.into(), *dual_op, simpler.into()))
                }
            }
            Expr::Var(name) => {
                if let Some(Variable { value }) = variables.get(name) {
                    match value {
                        crate::Value::Expression(expr) => {
                            expr.simplify(variables, qubits, procedures, unitaries)
                        }
                        crate::Value::Ket(ket) => Expr::Ket(ket.clone().into())
                            .simplify(variables, qubits, procedures, unitaries),
                    }
                } else if let Some(qub) = qubits.get(name) {
                    println!("found {name} at {qub:#?}");
                    Expr::Qub(name.to_string(), qub.clone().into())
                        .simplify(variables, qubits, procedures, unitaries)
                } else {
                    Err(MetaProgramError::VariableNotFound)
                }
            }
            Expr::Res(val) => Ok(Expr::Res(*val)),
            Expr::Unitary(unit, params) => Ok(Expr::Unitary(unit.clone(), params.clone())),
            Expr::Qub(name, qub) => {
                let Qubit { value } = *qub.clone();
                let inner = match value {
                    Value::Expression(expr) => {
                        if let Expr::Ket(ket) =
                            expr.simplify(variables, qubits, procedures, unitaries)?
                        {
                            Ok(*ket)
                        } else {
                            Err(MetaProgramError::QubitNonKetAssignment)
                        }
                    }
                    Value::Ket(ket) => Ok(Ket {
                        zero: ket
                            .zero
                            .simplify(variables, qubits, procedures, unitaries)?,
                        one: ket.one.simplify(variables, qubits, procedures, unitaries)?,
                    }),
                }?;
                Ok(Expr::Qub(
                    name.to_string(),
                    Qubit {
                        value: Value::Ket(inner),
                    }
                    .into(),
                ))
            }
            Expr::Ket(ket) => {
                let Ket { zero, one } = *ket.clone();
                Ok(Expr::Ket(
                    Ket {
                        zero: zero.simplify(variables, qubits, procedures, unitaries)?,
                        one: one.simplify(variables, qubits, procedures, unitaries)?,
                    }
                    .into(),
                )
                .into())
            }
        }
    }
}

pub fn parse_expr(pairs: &mut Pairs<Rule>) -> Result<Expression, ParserError> {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::num => Ok(Expr::Res(
                primary.as_str().parse().expect("primary num should parse"),
            )),
            Rule::var => Ok(Expr::Var(primary.as_str().to_string().into())),
            Rule::pi => Ok(Expr::Res(f64::consts::PI.into())),
            Rule::expr => parse_expr(&mut primary.into_inner()),
            Rule::listIndex => {
                let mut pairs = primary.into_inner();
                let ident = pairs
                    .next()
                    .expect("list index call should have an ident")
                    .as_str()
                    .to_string();
                let idx = parse_expr(&mut pairs)?;
                Ok(Expr::Index(ident, idx.into()))
            }
            Rule::functionCall => {
                let mut pairs = primary.into_inner();
                parse_function_call(&mut pairs)
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
pub(crate) enum SingleOp {
    Negate,
    Sqrt,
    NormSquared,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum DualOp {
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
    Index(U, Box<Expr<T, U>>),
    IFunc(Ident, Vec<Expr<T, U>>),
    RFunc(Box<Expr<T, U>>, Vec<Expr<T, U>>),
    SingleOp(SingleOp, Box<Expr<T, U>>),
    DualOp(Box<Expr<T, U>>, DualOp, Box<Expr<T, U>>),
    Unitary(Unitary, Vec<Expr<T, U>>),
    Qub(U, Box<Qubit>),
    Ket(Box<Ket<Expr<T, U>>>),
}

impl<T: ComplexFloat, U> Expr<T, U> {
    pub(crate) fn sqrt(self) -> Expr<T, U> {
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

    pub(crate) fn norm_sqr(self) -> Expr<Complex64, U> {
        match self {
            Expr::Res(s) => Expr::Res(s.norm_sqr().into()),
            other => Expr::SingleOp(SingleOp::NormSquared, other.into()),
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

    use crate::{expr::parse_expr, QParser, Rule};
    use std::f64;

    use super::{DualOp, Expr};

    fn test_frame_expr(input: &str, correct: Expr<Complex64, String>) {
        let mut pairs = QParser::parse(Rule::expr, input).unwrap();
        println!("pairs: {pairs:#?}");
        assert_eq!(parse_expr(&mut pairs).unwrap(), correct)
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
    fn parser_list_index() {
        test_frame_expr(
            "7 * some_list[3*x]",
            Expr::DualOp(
                Expr::Res(7.0.into()).into(),
                super::DualOp::Mul,
                Expr::Index(
                    "some_list".to_string(),
                    Expr::DualOp(
                        Expr::Res(3.0.into()).into(),
                        DualOp::Mul,
                        Expr::Var("x".to_string()).into(),
                    )
                    .into(),
                )
                .into(),
            ),
        );
    }
}
