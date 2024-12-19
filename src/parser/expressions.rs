use super::{Complex, Floating, Integer, Name};

pub(crate) enum AST {
    Number(Number),
    Variable(Variable),
    Call(Call),
    Binary(Binary),
}

pub(crate) enum Number {
    Integer(Integer),
    Floating(Floating),
    Complex(Complex),
    Negate(Box<AST>),
}

pub(crate) enum Variable {
    Compile(Name),
    Qubit(Name),
}

pub(crate) enum Call {
    Function(Name, Vec<AST>),
    Unitary(Name, Vec<AST>),
}

pub(crate) enum Binary {
    Add(Box<AST>, Box<AST>),
    Subtract(Box<AST>, Box<AST>),
    Multiply(Box<AST>, Box<AST>),
    Divide(Box<AST>, Box<AST>),
}
