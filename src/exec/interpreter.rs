use crate::{
    analysis::visit::{combine_all, Result, Visit, VisitExpr, VisitProgram},
    frontend::ast::*,
};

use super::{environment::Environment, RuntimeError};

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }
}

impl Visit for Interpreter {
    type Output = ();
    type Error = RuntimeError;
}
