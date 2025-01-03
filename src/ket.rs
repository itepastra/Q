use pest::iterators::Pair;

use crate::{expr::parse_expr, ParserError, Program, Rule, Value};

impl Program {
    pub(crate) fn parse_ket(&self, pair: Pair<Rule>) -> Result<Value, ParserError> {
        let mut pairs = pair.into_inner();
        let p0 = pairs
            .next()
            .expect("ket should have an expression on the left")
            .into_inner();
        let p1 = pairs
            .next()
            .expect("ket should have an expression on the left")
            .into_inner();
        let w0 = parse_expr(p0);
        let w1 = parse_expr(p1);
        Ok(Value::Ket(vec![w0, w1]))
    }
}
