use super::{expressions, prototypes};

pub(crate) enum AST {
    Function(prototypes::AST, expressions::AST),
}
