use pest::iterators::Pairs;

use crate::{expr::parse_expr, Param, Parameters, ParserError, Rule};

fn parse_parameters_inner(pairs: &mut Pairs<Rule>) -> Result<Parameters, ParserError> {
    Ok(pairs
        .map(|param| Param::Ident(param.as_str().to_string()))
        .collect())
}

fn parse_lambda_list(pairs: &mut Pairs<Rule>) -> Result<Parameters, ParserError> {
    let name = pairs.next().expect("array parameters should have a name");
    let amount = parse_expr(pairs)?;
    Ok(vec![Param::Arr(crate::Arr {
        name: name.as_str().to_string(),
        len: amount,
    })])
}

pub(crate) fn parse_parameters_or_arr(pairs: &mut Pairs<Rule>) -> Result<Parameters, ParserError> {
    let next = pairs.next().expect("a parameter block was expected");
    match next.as_rule() {
        Rule::parameters => parse_parameters_inner(&mut next.into_inner()),
        Rule::lambdaList => parse_lambda_list(&mut next.into_inner()),
        rule => unreachable!("parameters or list was expected, found {rule:#?}"),
    }
}

pub(crate) fn parse_parameters(pairs: &mut Pairs<Rule>) -> Result<Parameters, ParserError> {
    let next = pairs.next().expect("a parameter block was expected");
    match next.as_rule() {
        Rule::parameters => parse_parameters_inner(&mut next.into_inner()),
        rule => unreachable!("parameters was expected, found {rule:#?}"),
    }
}
