use pest::iterators::Pair;

use crate::{
    expr::{parse_expr, Expr, Expression},
    ParserError, Program, Rule, Value,
};

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Ket<T> {
    pub(crate) zero: T,
    pub(crate) one: T,
}

pub(crate) fn parse_ket(pair: Pair<Rule>) -> Result<Value, ParserError> {
    let mut pairs = pair.into_inner();
    let mut p0 = pairs
        .next()
        .expect("ket should have an expression on the left")
        .into_inner();
    let mut p1 = pairs
        .next()
        .expect("ket should have an expression on the left")
        .into_inner();
    let w0 = parse_expr(&mut p0)?;
    let w1 = parse_expr(&mut p1)?;
    println!("w0 {w0:#?}, w1 {w1:#?}");
    let w0s = w0.clone().norm_sqr();
    let w1s = w1.clone().norm_sqr();
    let norm = (w0s.clone() + w1s.clone()).sqrt();
    println!("w0s {w0s:#?} + w1s {w1s:#?} = {norm:#?}");
    Ok(Value::Ket(Ket {
        zero: w0 / norm.clone(),
        one: w1 / norm,
    }))
}

#[cfg(test)]
mod test {
    use pest::Parser;

    use crate::{expr::Expr, ket, QParser, Rule, Value};

    use super::{Expression, Ket};

    fn test_frame_ket(input: &str, correct: Ket<Expression>) {
        let mut pairs = QParser::parse(Rule::ket, input).unwrap();
        println!("pairs: {pairs:#?}");
        assert_eq!(
            ket::parse_ket(pairs.next().expect("parsing should return a ket rule")).unwrap(),
            Value::Ket(correct)
        )
    }

    #[test]
    fn if_ket_is_parsed_correctly() {
        test_frame_ket(
            "(1/sqrt(2),1/sqrt(2))",
            Ket {
                zero: Expr::Res(0.7071067811865475.into()),
                one: Expr::Res(0.7071067811865475.into()),
            },
        );
    }

    #[test]
    fn if_ket_gets_normalized_1() {
        test_frame_ket(
            "(1,1)",
            Ket {
                zero: Expr::Res(0.7071067811865475.into()),
                one: Expr::Res(0.7071067811865475.into()),
            },
        );
    }

    #[test]
    fn if_ket_gets_normalized_2() {
        test_frame_ket(
            "(1,0)",
            Ket {
                zero: Expr::Res(1.0.into()),
                one: Expr::Res(0.0.into()),
            },
        );
    }
}
