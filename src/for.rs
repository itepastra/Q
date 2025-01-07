use pest::iterators::Pairs;

use crate::{
    expr::{parse_expr, Expression},
    unitary::parse_unitary_body,
    Ident, ParserError, Rule, UnitaryBody,
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Loop {
    pub(crate) loopvar: Ident,
    pub(crate) lower: Expression,
    pub(crate) higher: Expression,
    pub(crate) body: UnitaryBody,
}

fn parse_range(pairs: &mut Pairs<Rule>) -> Result<(Expression, Expression), ParserError> {
    let lower = pairs.next().expect("range should have a lower bound");
    let upper = pairs.next().expect("range should have an upper bound");

    let lower = parse_expr(&mut lower.into_inner())?;
    let upper = parse_expr(&mut upper.into_inner())?;

    Ok((lower, upper))
}

pub(crate) fn parse_for_loop(pairs: &mut Pairs<Rule>) -> Result<Loop, ParserError> {
    let loopvar = pairs
        .next()
        .expect("for loop should have a loop variable")
        .as_str()
        .to_string();
    let range = pairs.next().expect("for loop should have a range");
    let (lower, upper) = parse_range(&mut range.into_inner())?;
    let body = pairs.next().expect("for loop should have a loop body");

    let unit = parse_unitary_body(&mut body.into_inner())?;

    Ok(Loop {
        loopvar,
        lower,
        higher: upper,
        body: unit,
    })
}

#[cfg(test)]
mod test {
    use pest::Parser;

    use crate::{expr::Expr, r#for::parse_for_loop, QParser, Rule, UnitaryBodyStmt, UnitaryStmt};

    use super::Loop;

    #[test]
    fn parse_empty_for() {
        let input = "for x in (0..3) {}";
        let correct = Loop {
            loopvar: "x".to_string(),
            lower: Expr::Res(0.0.into()),
            higher: Expr::Res(3.0.into()),
            body: vec![],
        };

        let mut pairs = QParser::parse(Rule::forLoop, input).unwrap();
        println!("{pairs:#?}");

        assert_eq!(
            parse_for_loop(&mut pairs.next().unwrap().into_inner()).unwrap(),
            correct
        );
    }

    #[test]
    fn parse_unitary_stmts_for() {
        let input = "for x in (0..3) {
hadamard(q[x])
}";
        let correct = Loop {
            loopvar: "x".to_string(),
            lower: Expr::Res(0.0.into()),
            higher: Expr::Res(3.0.into()),
            body: vec![UnitaryBodyStmt::Stmt(UnitaryStmt {
                unit: "hadamard".to_string(),
                qbits: vec![Expr::Index(
                    "q".to_string(),
                    Expr::Var("x".to_string()).into(),
                )],
            })],
        };

        let mut pairs = QParser::parse(Rule::forLoop, input).unwrap();
        println!("{pairs:#?}");

        assert_eq!(
            parse_for_loop(&mut pairs.next().unwrap().into_inner()).unwrap(),
            correct
        );
    }
}
