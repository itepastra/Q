use super::{expressions, Name};

pub(crate) enum AST {
    Function(Name, Vec<expressions::AST>),
}
