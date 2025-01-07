use pest::iterators::{Pair, Pairs};

use crate::{
    expr::{Expr, Expression},
    function::parse_function_call,
    r#for::parse_for_loop,
    ParserError, Rule, UnitaryBody, UnitaryBodyStmt, UnitaryStmt,
};

pub(crate) fn parse_unitary_body(pairs: &mut Pairs<Rule>) -> Result<UnitaryBody, ParserError> {
    let statements = pairs
        .map(|stmt| parse_unitary_statement(stmt).expect("statement in for loop should be correct"))
        .collect();
    Ok(statements)
}

fn unitary_function_call(expr: Expression) -> Result<UnitaryBodyStmt, ParserError> {
    match expr {
        Expr::IFunc(ident, params) => Ok(UnitaryBodyStmt::Stmt(UnitaryStmt {
            unit: ident,
            qbits: params,
        })),
        Expr::RFunc(expr, vec) => Ok(UnitaryBodyStmt::Rec(
            unitary_function_call(*expr)?.into(),
            vec,
        )),
        expr => unreachable!(
            "unitary function call can only be called with rfunc or ifunc, called with {expr:#?}"
        ),
    }
}

fn parse_unitary_statement(pair: Pair<Rule>) -> Result<UnitaryBodyStmt, ParserError> {
    match pair.as_rule() {
        Rule::functionCall => {
            let func = parse_function_call(&mut pair.into_inner())?;
            unitary_function_call(func)
        }
        Rule::forLoop => {
            let lop = parse_for_loop(&mut pair.into_inner())?;
            Ok(UnitaryBodyStmt::ForLoop(lop))
        }
        rule => {
            unreachable!("expected valid unitary statement, got {rule:#?} with pairs {pair:#?}")
        }
    }
}

#[cfg(test)]
mod test {
    use num_complex::Complex64;
    use pest::Parser;

    use crate::{
        expr::Expr, r#for::Loop, unitary::parse_unitary_statement, QParser, Rule, UnitaryBodyStmt,
        UnitaryStmt,
    };

    #[test]
    fn parser_unitary_for_loop() {
        let input = "for a in (2..5) {
do_something(q[a])
}";
        let correct = UnitaryBodyStmt::ForLoop(Loop {
            loopvar: "a".to_string(),
            lower: Expr::Res(2.0.into()),
            higher: Expr::Res(5.0.into()),
            body: vec![UnitaryBodyStmt::Stmt(UnitaryStmt {
                unit: "do_something".to_string(),
                qbits: vec![Expr::Index(
                    "q".to_string(),
                    Expr::Var("a".to_string()).into(),
                )],
            })],
        });

        let mut pairs = QParser::parse(Rule::forLoop, input).unwrap();
        println!("{pairs:#?}");

        assert_eq!(
            parse_unitary_statement(pairs.next().expect("there should be a statement")).unwrap(),
            correct
        );
    }

    #[test]
    fn parser_unitary_statement_function() {
        let input = "some_unitary(q3, q5)";
        let correct = UnitaryBodyStmt::Stmt(UnitaryStmt {
            unit: "some_unitary".to_string(),
            qbits: vec![Expr::Var("q3".to_string()), Expr::Var("q5".to_string())],
        });
        let mut pairs = QParser::parse(Rule::functionCall, input).unwrap();
        println!("{pairs:#?}");

        assert_eq!(
            parse_unitary_statement(pairs.next().expect("there should be a statement")).unwrap(),
            correct
        );
    }

    #[test]
    fn parser_unitary_statement_rfunc() {
        let input = "some_proc(1/2)(q3, q5)";
        let correct = UnitaryBodyStmt::Rec(
            UnitaryBodyStmt::Stmt(UnitaryStmt {
                unit: "some_proc".to_string(),
                qbits: vec![Expr::Res(0.5.into())],
            })
            .into(),
            vec![Expr::Var("q3".to_string()), Expr::Var("q5".to_string())],
        );
        let mut pairs = QParser::parse(Rule::functionCall, input).unwrap();
        println!("{pairs:#?}");

        assert_eq!(
            parse_unitary_statement(pairs.next().expect("there should be a statement")).unwrap(),
            correct
        );
    }

    #[test]
    fn parser_unitary_statement_rrfunc() {
        let input = "some_proc(2i)(1/2)(q3, q5)";
        let correct = UnitaryBodyStmt::Rec(
            UnitaryBodyStmt::Rec(
                UnitaryBodyStmt::Stmt(UnitaryStmt {
                    unit: "some_proc".to_string(),
                    qbits: vec![Expr::Res(Complex64::new(0.0, 2.0))],
                })
                .into(),
                vec![Expr::Res(0.5.into())],
            )
            .into(),
            vec![Expr::Var("q3".to_string()), Expr::Var("q5".to_string())],
        );
        let mut pairs = QParser::parse(Rule::functionCall, input).unwrap();
        println!("{pairs:#?}");

        assert_eq!(
            parse_unitary_statement(pairs.next().expect("there should be a statement")).unwrap(),
            correct
        );
    }
}
