mod expr;
mod r#for;
mod function;
mod ket;
mod lambda;
mod matrix;
mod parameters;
mod procedure;
mod unitary;

use std::{
    collections::HashMap,
    io::{stdin, Read},
};

use expr::{parse_expr, Expr, Expression};
use ket::{parse_ket, Ket};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use r#for::Loop;
use unitary::parse_unitary_body;

#[derive(Parser)]
#[grammar = "src/grammar.pest"]
struct QParser;

type Ident = String;
type Parameters = Vec<Param>;

#[derive(Debug, Clone, PartialEq)]
enum Param {
    Ident(Ident),
    Arr(Arr),
}

#[derive(Debug, Clone, PartialEq)]
struct Arr {
    name: Ident,
    len: Expression,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum ParserError {
    EmptyMatrix,
    InvalidParameterLength,
    MalformedMatrix,
}

#[derive(Debug, PartialEq, Clone)]
enum Value {
    Expression(Expression),
    Ket(Ket<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
enum ReturnStmt {
    Expression(Value),
    UnitaryLambda(Unitary),
    ProcedureLambda(Box<Procedure>), // I won't allow assigning these to variables tho, otherwise we get
                                     // js
}

type Unit = Ident;

#[derive(Debug, PartialEq, Clone)]
struct Variable {
    value: Value,
}

#[derive(Debug, Clone, PartialEq)]
struct Procedure {
    parameters: Parameters,
    program: Program,
}

type UnitaryBody = Vec<UnitaryBodyStmt>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum UnitaryBodyStmt {
    ForLoop(Loop),
    Stmt(UnitaryStmt),
    Rec(Box<UnitaryBodyStmt>, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
struct UnitaryStmt {
    unit: Unit,
    qbits: Vec<Expression>,
}

#[derive(Debug, Default, Clone, PartialEq)]
struct Unitary {
    parameters: Parameters,
    steps: UnitaryBody,
}

#[derive(Debug, Clone, PartialEq)]
struct Qubit {
    value: Value,
}

#[derive(Debug, Default, Clone, PartialEq)]
struct Program {
    variables: HashMap<Ident, Variable>,
    procedures: HashMap<Ident, Procedure>,
    unitaries: HashMap<Ident, Unitary>,
    qubits: HashMap<Ident, Qubit>,
    ret: Option<ReturnStmt>,
}

impl Program {
    fn parse_variable_assignment(
        &self,
        pairs: &mut Pairs<Rule>,
    ) -> Result<(Ident, Variable), ParserError> {
        let p = pairs.clone();
        let name = pairs
            .next()
            .expect(&format!("assignment should have an identifier, ({p:#?})"))
            .as_str()
            .to_string();
        let value = pairs
            .next()
            .expect(&format!("assignment should have an identifier, ({p:#?})"));
        match value.as_rule() {
            Rule::ketValue => {
                let ket = parse_ket(value.into_inner().next().expect("ketValue has ket"))?;
                Ok((name, Variable { value: ket }))
            }
            Rule::singleValue => {
                let value = parse_expr(&mut value.into_inner())?;
                Ok((
                    name,
                    Variable {
                        value: Value::Expression(value),
                    },
                ))
            }
            rule => unreachable!("expected a variable assignment, found {rule:#?}"),
        }
    }

    fn parse_unitary(&self, pair: Pair<Rule>) -> Result<(Ident, Unitary), ParserError> {
        match pair.as_rule() {
            Rule::functionUnitary => {
                let mut parts = pair.into_inner();
                let name = parts
                    .next()
                    .expect("unitary does not have a name")
                    .as_str()
                    .to_string();
                let parameters = parts
                    .next()
                    .expect("unitary does not have parameters")
                    .into_inner()
                    .map(|param| Param::Ident(param.as_str().to_string()))
                    .collect();
                let unit = Unitary {
                    parameters,
                    steps: parse_unitary_body(
                        &mut parts
                            .next()
                            .expect("unitary should have a body")
                            .into_inner(),
                    )?,
                };

                Ok((name, unit))
            }
            Rule::matrixUnitary => {
                let mut parts = pair.into_inner();
                let name = parts
                    .next()
                    .expect("matrix unitary does not have a name")
                    .as_str()
                    .to_string();
                let _matrix = self.parse_matrix(
                    parts
                        .next()
                        .expect("matrix unitary has a matrix")
                        .into_inner(),
                );
                // TODO: decompose matrix to unitaries
                Ok((name, Unitary::default()))
            }
            rule => unreachable!("expected a unitary, found {rule:#?}"),
        }
    }

    fn parse_single_qubit(&self, pair: Pair<Rule>) -> Result<(Ident, Qubit), ParserError> {
        match pair.as_rule() {
            Rule::singleQbitAssignment => {
                let mut parts = pair.clone().into_inner();
                let name = parts
                    .next()
                    .expect("assignment has identifier")
                    .as_str()
                    .to_string();
                let value = parts.next().expect("assignment has value");
                let val = match value.as_rule() {
                    Rule::ket => parse_ket(value)?,
                    Rule::ident => Value::Expression(Expr::Var(name.to_string())),
                    rule => unreachable!("expected a qubit initialisation value, found {rule:#?}"),
                };
                Ok((name, Qubit { value: val }))
            }
            rule => unreachable!("expected a qubit assignment, found {rule:#?}"),
        }
    }

    fn parse_return(&mut self, pair: Pair<Rule>) -> Result<ReturnStmt, ParserError> {
        match pair.as_rule() {
            Rule::returnExpr => {
                let expr = parse_expr(&mut pair.into_inner())?;
                Ok(ReturnStmt::Expression(Value::Expression(expr)))
            }
            Rule::ulambda => {
                let lambda = self.parse_unit_lambda(pair)?;
                Ok(ReturnStmt::UnitaryLambda(lambda))
            }
            Rule::plambda => {
                let lambda = self.parse_proc_lambda(pair)?;
                Ok(ReturnStmt::ProcedureLambda(lambda.into()))
            }
            rule => unreachable!("expected a valid return value, found {rule:#?}"),
        }
    }

    fn parse(&mut self, pairs: Pairs<Rule>) -> () {
        for pair in pairs {
            match pair.as_rule() {
                Rule::variableAssignment => {
                    let (name, val) = self
                        .parse_variable_assignment(&mut pair.into_inner())
                        .expect("variable assignment not correct");
                    self.variables.insert(name, val);
                }
                Rule::singleQbitAssignment => {
                    let (name, val) = self
                        .parse_single_qubit(pair)
                        .expect("qubit assignment not correct");
                    self.qubits.insert(name, val);
                }
                Rule::functionUnitary | Rule::matrixUnitary => {
                    let (name, unitary) = self.parse_unitary(pair).expect("unitary not valid");
                    self.unitaries.insert(name, unitary);
                }
                Rule::procedure => {
                    let (name, procedure) =
                        self.parse_procedure(pair).expect("procedure not valid");
                    self.procedures.insert(name, procedure);
                }
                Rule::r#return => {
                    let ret = self
                        .parse_return(
                            pair.into_inner()
                                .next()
                                .expect("return statement should have a body"),
                        )
                        .expect("return statement not valid");
                    self.ret = Some(ret);
                }
                Rule::EOI => return,
                rule => unreachable!("expected a statement, found {rule:#?}"),
            }
        }
    }
}

fn main() {
    let mut file = String::new();
    match stdin().lock().read_to_string(&mut file) {
        Ok(len) => eprintln!("read input with length {len}"),
        Err(_) => todo!(),
    }
    eprintln!("input was: \n\n{file}");
    let mut program = Program::default();

    match QParser::parse(Rule::program, file.as_str()) {
        Ok(pairs) => {
            program.parse(pairs);
            println!("parsed: {:#?}", program)
        }
        Err(err) => todo!("error was {err:#?}"),
    }
}

#[cfg(test)]
mod test {
    use num_complex::Complex;
    use pest::Parser;

    use crate::{ket::Ket, Expr, Program, QParser, Rule, Value, Variable};

    fn test_frame_assignment(input: &str, name: &str, correct: Value) {
        let mut pairs = QParser::parse(Rule::variableAssignment, input)
            .unwrap()
            .next()
            .unwrap()
            .into_inner();
        let program = Program::default();
        let (n, v) = program.parse_variable_assignment(&mut pairs).unwrap();
        assert_eq!(v, Variable { value: correct });
        assert_eq!(n, name.to_string());
    }

    #[test]
    fn test_assignment_ket() {
        test_frame_assignment(
            "let wowa = (13,53 - 69)",
            "wowa",
            Value::Ket(Ket {
                zero: Expr::Res(Complex::new(0.6305926250944657, 0.0)),
                one: Expr::Res(Complex::new(-0.7761140001162654, 0.0)),
            }),
        );
    }

    #[test]
    fn test_assignment_number() {
        test_frame_assignment(
            "let br3e = 29 * 33 - 19i",
            "br3e",
            Value::Expression(Expr::Res(Complex::new(29.0 * 33.0, -19.0))),
        );
    }
}
