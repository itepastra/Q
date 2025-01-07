use pest::iterators::Pair;

use crate::{parameters::parse_parameters, Ident, ParserError, Procedure, Program, Rule};

impl Program {
    pub(crate) fn parse_procedure(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<(Ident, Procedure), ParserError> {
        let mut pairs = pair.into_inner();
        let name = pairs
            .next()
            .expect("procedure should have a name")
            .as_str()
            .to_string();
        let parameters = parse_parameters(&mut pairs)?;
        // TODO: do something with types
        let _typ = pairs.next().expect("procedure should have a return type");
        let body = pairs.next().expect("procedure should have a body");

        let mut program = Program::default();
        program.parse(body.into_inner());

        Ok((
            name,
            Procedure {
                parameters,
                program,
            },
        ))
    }
}
