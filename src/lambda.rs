use pest::iterators::Pair;

use crate::{
    expr::parse_expr,
    parameters::{self, parse_parameters},
    ParserError, Procedure, Program, Rule, Unitary, Value,
};

impl Program {
    pub(crate) fn parse_proc_lambda(&self, pair: Pair<Rule>) -> Result<Procedure, ParserError> {
        let mut pairs = pair.into_inner();
        let parameters = parse_parameters(&mut pairs)?;
        println!("found parameters {parameters:#?}");

        let body = pairs.next().expect("procedure should have a body");

        let mut program = Program::default();
        program.parse(body.clone().into_inner());

        println!("program is {program:#?}");
        Ok(Procedure {
            parameters,
            program,
        })
    }

    pub(crate) fn parse_unit_lambda(&self, pair: Pair<Rule>) -> Result<Unitary, ParserError> {
        let mut pairs = pair.into_inner();
        let parameters = parse_parameters(&mut pairs)?;
        println!("found parameters {parameters:#?}");

        let body = pairs.next().expect("procedure should have a body");

        println!("body is {body:#?}");
        todo!()
    }
}
