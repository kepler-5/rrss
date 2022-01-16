use std::{
    cell::RefCell,
    io::{stdin, stdout, Read, Stdin, Stdout, Write},
};

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
    MissingPronounReferent,
}

#[derive(Debug, PartialEq)]
pub struct Environment<In, Out> {
    variables: Vec<SymTable>,
    last_access: Option<VariableName>,
    input_buf: In,
    output_buf: Out,
}

impl<In: Read, Out: Write> Environment<In, Out> {
    pub fn raw(input_buf: In, output_buf: Out) -> Self {
        Self {
            variables: vec![SymTable::new()],
            last_access: None,
            input_buf,
            output_buf,
        }
    }

    pub fn refcell_raw(input_buf: In, output_buf: Out) -> RefCell<Self> {
        RefCell::new(Self::raw(input_buf, output_buf))
    }
}

impl Environment<Stdin, Stdout> {
    pub fn new() -> Self {
        Self::raw(stdin(), stdout())
    }

    pub fn refcell() -> RefCell<Self> {
        RefCell::new(Self::new())
    }
}

impl<I, O> Environment<I, O> {
    pub fn push_scope(&mut self) {
        self.variables.push(SymTable::new())
    }
    pub fn pop_scope(&mut self) {
        debug_assert!(self.variables.len() > 1);
        self.variables.pop();
        self.last_access = None;
    }

    pub fn lookup_var(&mut self, name: &VariableName) -> Result<&Val, EnvironmentError> {
        self.last_access = Some(name.clone());
        self.lookup_var_impl(name)
    }

    pub fn lookup_var_mut(&mut self, name: &VariableName) -> Result<&mut Val, EnvironmentError> {
        self.last_access = Some(name.clone());
        self.lookup_var_mut_impl(name)
    }

    fn lookup_var_impl(&self, name: &VariableName) -> Result<&Val, EnvironmentError> {
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

    fn lookup_var_mut_impl(&mut self, name: &VariableName) -> Result<&mut Val, EnvironmentError> {
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
        self.last_access = Some(name.clone());
        self.variables.last_mut().unwrap().emplace(name)
    }

    pub fn last_access(&mut self) -> Result<&Val, EnvironmentError> {
        self.last_access
            .as_ref()
            .ok_or(EnvironmentError::MissingPronounReferent)
            .and_then(|name| self.lookup_var_impl(name))
    }

    pub fn last_access_mut(&mut self) -> Result<&mut Val, EnvironmentError> {
        self.last_access
            .clone() // TODO
            .ok_or(EnvironmentError::MissingPronounReferent)
            .and_then(|name| self.lookup_var_mut_impl(&name))
    }
}
