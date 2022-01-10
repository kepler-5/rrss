use crate::analysis::visit::Visit;

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
