use num_complex::{Complex64, ComplexFloat};
use pest::iterators::Pairs;

use crate::{
    expr::{parse_expr, Expr, Expression},
    ParserError, Rule,
};

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

pub(crate) fn parse_function_call(pairs: &mut Pairs<Rule>) -> Result<Expression, ParserError> {
    let name = pairs.next().expect("function should have a name").as_str();

    let mut params_vec: Vec<Vec<_>> = Vec::new();
    while let Some(parameters) = pairs.next() {
        let param_pairs = parameters.into_inner();
        let params: Vec<_> = param_pairs
            .map(|param| parse_expr(&mut param.into_inner()))
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

#[cfg(test)]
mod test {
    use num_complex::Complex64;
    use pest::Parser;

    use crate::{
        expr::{parse_expr, DualOp},
        QParser, Rule,
    };

    use super::{Expr, Expression};

    const IMAGINARY: Complex64 = Complex64::new(0.0, 1.0);
    fn test_frame_expr(input: &str, correct: Expression) {
        let mut pairs = QParser::parse(Rule::expr, input).unwrap();
        println!("pairs: {pairs:#?}");
        assert_eq!(parse_expr(&mut pairs).unwrap(), correct)
    }

    #[test]
    fn parser_function() {
        test_frame_expr(
            "some_function() + 1i",
            Expr::DualOp(
                Expr::IFunc("some_function".to_string(), vec![]).into(),
                DualOp::Add,
                Expr::Res(IMAGINARY).into(),
            ),
        );
    }

    #[test]
    fn parser_function_with_param() {
        let correct = Expr::DualOp(
            Expr::IFunc("some_function".to_string(), vec![Expr::Res(12.0.into())]).into(),
            DualOp::Add,
            Expr::Res(IMAGINARY).into(),
        );
        test_frame_expr("some_function(3*4) + 1i", correct);
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

    #[test]
    fn parser_function_array() {
        test_frame_expr(
            "some_unitary(w[0], q[n])",
            Expr::IFunc(
                "some_unitary".to_string(),
                vec![
                    Expr::Index("w".to_string(), Expr::Res(0.0.into()).into()),
                    Expr::Index("q".to_string(), Expr::Var("n".to_string()).into()),
                ],
            ),
        );
    }
}
