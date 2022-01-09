use derive_more::From;

use crate::frontend::ast::VariableName;

use super::{
    sym_table::{SymTable, SymTableError},
    val::Val,
};

#[cfg(test)]
mod tests;

#[derive(Debug, From, PartialEq)]
pub enum EnvironmentError {
    SymTableError(SymTableError),
}

#[derive(Debug, PartialEq)]
pub struct Environment {
    variables: Vec<SymTable>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            variables: vec![SymTable::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.variables.push(SymTable::new())
    }
    pub fn pop_scope(&mut self) {
        debug_assert!(self.variables.len() > 1);
        self.variables.pop();
    }

    pub fn lookup_var(&self, name: &VariableName) -> Result<&Val, EnvironmentError> {
        self.variables
            .iter()
            .rev()
            .map(|table| table.lookup(name))
            .find(|r| r.is_ok())
            .map_or_else(
                || Err(SymTableError::NameNotFound(name.clone()).into()),
                |r| r.map_err(Into::into),
            )
    }
    pub fn lookup_var_mut(&mut self, name: &VariableName) -> Result<&mut Val, EnvironmentError> {
        self.variables
            .iter_mut()
            .rev()
            .map(|table| table.lookup_mut(name))
            .find(|r| r.is_ok())
            .map_or_else(
                || Err(SymTableError::NameNotFound(name.clone()).into()),
                |r| r.map_err(Into::into),
            )
    }
    pub fn create_var(&mut self, name: &VariableName) -> &mut Val {
        self.variables.last_mut().unwrap().emplace(name)
    }
}
