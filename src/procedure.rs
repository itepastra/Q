use std::collections::HashMap;

use pest::iterators::{Pair, Pairs};

use crate::{Ident, ParserError, Procedure, Program, Rule};

impl Program {
    pub(crate) fn parse_procedure(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<(Ident, Procedure), ParserError> {
        let mut pairs = pair.into_inner();
        let name = pairs
            .next()
            .expect("procedure should have a name")
            .to_string();
        let parameters = pairs
            .next()
            .expect("procedure should have a parameter block")
            .into_inner()
            .map(|param| param.as_str().to_string())
            .collect();
        let typ = pairs.next().expect("procedure should have a return type");
        let body = pairs.next().expect("procedure should have a body");

        let mut program = Program::default();
        program.parse(body.clone().into_inner());

        todo!("the body pairs are {body:#?}");
        Ok((
            name,
            Procedure {
                parameters,
                program: todo!(),
            },
        ))
    }
}
