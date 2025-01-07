use pest::iterators::Pair;

use crate::{
    parameters::parse_parameters_or_arr, unitary, ParserError, Procedure, Program, Rule, Unitary,
};

impl Program {
    pub(crate) fn parse_proc_lambda(&self, pair: Pair<Rule>) -> Result<Procedure, ParserError> {
        let mut pairs = pair.into_inner();
        let parameters = parse_parameters_or_arr(&mut pairs)?;

        let body = pairs.next().expect("procedure lambda should have a body");

        let mut program = Program::default();
        program.parse(body.clone().into_inner());

        Ok(Procedure {
            parameters,
            program,
        })
    }

    pub(crate) fn parse_unit_lambda(&self, pair: Pair<Rule>) -> Result<Unitary, ParserError> {
        let mut pairs = pair.into_inner();
        let parameters = parse_parameters_or_arr(&mut pairs)?;

        let mut body = pairs
            .next()
            .expect("unitary lambda should have a body")
            .into_inner();

        let stmts = unitary::parse_unitary_body(&mut body)?;

        Ok(Unitary {
            parameters,
            steps: stmts,
        })
    }
}
