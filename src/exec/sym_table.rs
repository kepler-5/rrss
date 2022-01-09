use std::{
    collections::{BTreeMap, HashMap},
    hash::Hash,
};

use crate::{
    exec::val::Val,
    frontend::ast::{CommonIdentifier, ProperIdentifier, SimpleIdentifier, VariableName},
};

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub enum SymTableError {
    NameNotFound(VariableName),
}

#[derive(Debug, Default, PartialEq)]
pub struct SymTable {
    simple: HashMap<SimpleIdentifier, Val>,
    common: HashMap<CommonIdentifier, Val>,
    proper: BTreeMap<ProperIdentifier, Val>,
}

#[derive(Debug, PartialEq)]
enum Lowercased<'a, T> {
    Ref(&'a T),
    New(T),
}

impl<'a, T> Lowercased<'a, T> {
    fn as_ref(&self) -> &T {
        match self {
            Lowercased::Ref(t) => t,
            Lowercased::New(t) => &t,
        }
    }
}
impl<'a, T: Clone> Lowercased<'a, T> {
    fn take(self) -> T {
        match self {
            Lowercased::Ref(t) => t.clone(),
            Lowercased::New(t) => t,
        }
    }
}

trait ToLowercase: Sized {
    fn to_lowercase(&self) -> Lowercased<Self>;
}

impl ToLowercase for SimpleIdentifier {
    fn to_lowercase(&self) -> Lowercased<Self> {
        self.0
            .chars()
            .all(|c| c.is_lowercase())
            .then(|| Lowercased::Ref(self))
            .unwrap_or_else(|| {
                Lowercased::New(Self(
                    self.0.chars().map(|c| c.to_lowercase()).flatten().collect(),
                ))
            })
    }
}

impl ToLowercase for CommonIdentifier {
    fn to_lowercase(&self) -> Lowercased<Self> {
        self.0
            .chars()
            .chain(self.1.chars())
            .all(|c| c.is_lowercase())
            .then(|| Lowercased::Ref(self))
            .unwrap_or_else(|| {
                Lowercased::New(Self(
                    self.0.chars().map(|c| c.to_lowercase()).flatten().collect(),
                    self.1.chars().map(|c| c.to_lowercase()).flatten().collect(),
                ))
            })
    }
}

impl ToLowercase for ProperIdentifier {
    fn to_lowercase(&self) -> Lowercased<Self> {
        self.0
            .iter()
            .map(|s| s.chars())
            .flatten()
            .all(|c| c.is_lowercase())
            .then(|| Lowercased::Ref(self))
            .unwrap_or_else(|| {
                Lowercased::New(Self(
                    self.0
                        .iter()
                        .map(|s| {
                            s.chars()
                                .map(|c| c.to_lowercase())
                                .flatten()
                                .collect::<String>()
                        })
                        .collect(),
                ))
            })
    }
}

trait Lookup<T> {
    fn lookup(&self, name: &T) -> Result<&Val, SymTableError>;
    fn lookup_mut(&mut self, name: &T) -> Result<&mut Val, SymTableError>;
    fn emplace(&mut self, name: &T) -> &mut Val;
}

impl<T: Clone + Eq + Hash + ToLowercase + Into<VariableName>> Lookup<T> for HashMap<T, Val> {
    fn lookup(&self, name: &T) -> Result<&Val, SymTableError> {
        self.get(name.to_lowercase().as_ref())
            .ok_or_else(|| SymTableError::NameNotFound(name.clone().into()))
    }
    fn lookup_mut(&mut self, name: &T) -> Result<&mut Val, SymTableError> {
        self.get_mut(name.to_lowercase().as_ref())
            .ok_or_else(|| SymTableError::NameNotFound(name.clone().into()))
    }
    fn emplace(&mut self, name: &T) -> &mut Val {
        self.entry(name.to_lowercase().take()).or_default()
    }
}

impl Lookup<ProperIdentifier> for BTreeMap<ProperIdentifier, Val> {
    fn lookup(&self, name: &ProperIdentifier) -> Result<&Val, SymTableError> {
        self.get(name.to_lowercase().as_ref())
            .ok_or_else(|| SymTableError::NameNotFound(name.clone().into()))
    }
    fn lookup_mut(&mut self, name: &ProperIdentifier) -> Result<&mut Val, SymTableError> {
        self.get_mut(name.to_lowercase().as_ref())
            .ok_or_else(|| SymTableError::NameNotFound(name.clone().into()))
    }
    fn emplace(&mut self, name: &ProperIdentifier) -> &mut Val {
        self.entry(name.to_lowercase().take()).or_default()
    }
}

impl SymTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn lookup(&self, name: &VariableName) -> Result<&Val, SymTableError> {
        match name {
            VariableName::Simple(name) => self.simple.lookup(name),
            VariableName::Common(name) => self.common.lookup(name),
            VariableName::Proper(name) => self.proper.lookup(name),
        }
    }
    pub fn lookup_mut(&mut self, name: &VariableName) -> Result<&mut Val, SymTableError> {
        match name {
            VariableName::Simple(name) => self.simple.lookup_mut(name),
            VariableName::Common(name) => self.common.lookup_mut(name),
            VariableName::Proper(name) => self.proper.lookup_mut(name),
        }
    }
    pub fn emplace(&mut self, name: &VariableName) -> &mut Val {
        match name {
            VariableName::Simple(name) => self.simple.emplace(name),
            VariableName::Common(name) => self.common.emplace(name),
            VariableName::Proper(name) => self.proper.emplace(name),
        }
    }
}
