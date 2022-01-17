use std::{
    cell::RefCell,
    io::{stdin, stdout, BufRead, BufReader, Read, Stdin, Stdout, Write},
    sync::Arc,
};

use derive_more::From;

use crate::frontend::ast::{FunctionData, VariableName};

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
    IOError(String),
}

#[derive(Debug)]
pub struct Environment<In, Out> {
    symbols: Vec<SymTable>,
    last_access: Option<VariableName>,
    input_buf: BufReader<In>,
    output_buf: Out,
}

impl<In: Read, Out: Write> Environment<In, Out> {
    pub fn raw(input_buf: In, output_buf: Out) -> Self {
        Self {
            symbols: vec![SymTable::new()],
            last_access: None,
            input_buf: BufReader::new(input_buf),
            output_buf,
        }
    }

    pub fn refcell_raw(input_buf: In, output_buf: Out) -> RefCell<Self> {
        RefCell::new(Self::raw(input_buf, output_buf))
    }

    pub fn output(&mut self, text: &str) -> Result<(), EnvironmentError> {
        writeln!(self.output_buf, "{}", text)
            .map(|_| ())
            .map_err(|e| EnvironmentError::IOError(e.to_string()))
    }

    pub fn input(&mut self) -> Result<String, EnvironmentError> {
        let mut buf = String::new();
        self.input_buf
            .read_line(&mut buf)
            .map_err(|e| EnvironmentError::IOError(e.to_string()))?;
        if buf.ends_with('\n') {
            buf.pop();
        }
        Ok(buf)
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
        self.symbols.push(SymTable::new())
    }
    pub fn pop_scope(&mut self) {
        debug_assert!(self.symbols.len() > 1);
        self.symbols.pop();
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

    fn stop_searching<T>(r: &Result<T, SymTableError>) -> bool {
        match r {
            Ok(_) => true,
            Err(e) => !e.is_name_not_found(),
        }
    }

    fn lookup_var_impl(&self, name: &VariableName) -> Result<&Val, EnvironmentError> {
        self.symbols
            .iter()
            .rev()
            .map(|table| table.lookup_var(name))
            .find(Self::stop_searching)
            .map_or_else(
                || Err(SymTableError::NameNotFound(name.clone()).into()),
                |r| r.map_err(Into::into),
            )
    }

    fn lookup_var_mut_impl(&mut self, name: &VariableName) -> Result<&mut Val, EnvironmentError> {
        self.symbols
            .iter_mut()
            .rev()
            .map(|table| table.lookup_var_mut(name))
            .find(Self::stop_searching)
            .map_or_else(
                || Err(SymTableError::NameNotFound(name.clone()).into()),
                |r| r.map_err(Into::into),
            )
    }

    pub fn create_var(&mut self, name: &VariableName) -> Result<&mut Val, EnvironmentError> {
        self.last_access = Some(name.clone());
        self.symbols
            .last_mut()
            .unwrap()
            .emplace_var(name)
            .map_err(Into::into)
    }

    pub fn lookup_func(&self, name: &VariableName) -> Result<&FunctionData, EnvironmentError> {
        self.symbols
            .iter()
            .rev()
            .map(|table| table.lookup_func(name))
            .find(Self::stop_searching)
            .map_or_else(
                || Err(SymTableError::NameNotFound(name.clone()).into()),
                |r| r.map_err(Into::into),
            )
    }

    pub fn create_func(
        &mut self,
        name: &VariableName,
        data: Arc<FunctionData>,
    ) -> Result<&FunctionData, EnvironmentError> {
        self.symbols
            .last_mut()
            .unwrap()
            .emplace_func(name, data)
            .map_err(Into::into)
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
