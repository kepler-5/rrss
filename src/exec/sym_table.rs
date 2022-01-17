use std::{
    collections::{BTreeMap, HashMap},
    hash::Hash,
    sync::Arc,
};

use derive_more::From;

use crate::{
    exec::val::Val,
    frontend::ast::{
        CommonIdentifier, FunctionData, ProperIdentifier, SimpleIdentifier, VariableName,
    },
};

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub enum SymTableError {
    NameNotFound(VariableName),
    ExpectedVarFoundFunc(VariableName),
    ExpectedFuncFoundVar(VariableName),
}

#[derive(Debug, From, PartialEq)]
enum SymTableEntry {
    Var(Val),
    Func(Arc<FunctionData>),
}

impl SymTableEntry {
    fn as_var(&self) -> Option<&Val> {
        match self {
            SymTableEntry::Var(v) => Some(v),
            SymTableEntry::Func(_) => None,
        }
    }
    fn as_var_mut(&mut self) -> Option<&mut Val> {
        match self {
            SymTableEntry::Var(v) => Some(v),
            SymTableEntry::Func(_) => None,
        }
    }
    fn as_func(&self) -> Option<&FunctionData> {
        match self {
            SymTableEntry::Var(_) => None,
            SymTableEntry::Func(f) => Some(&f),
        }
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct SymTable {
    simple: HashMap<SimpleIdentifier, SymTableEntry>,
    common: HashMap<CommonIdentifier, SymTableEntry>,
    proper: BTreeMap<ProperIdentifier, SymTableEntry>,
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
    fn lookup(&self, name: &T) -> Result<&SymTableEntry, SymTableError>;
    fn lookup_mut(&mut self, name: &T) -> Result<&mut SymTableEntry, SymTableError>;
    fn emplace(&mut self, name: &T, entry: SymTableEntry) -> &mut SymTableEntry;
}

impl<T> Lookup<T> for HashMap<T, SymTableEntry>
where
    T: Clone + Eq + Hash + ToLowercase + Into<VariableName>,
{
    fn lookup(&self, name: &T) -> Result<&SymTableEntry, SymTableError> {
        self.get(name.to_lowercase().as_ref())
            .ok_or_else(|| SymTableError::NameNotFound(name.clone().into()))
    }
    fn lookup_mut(&mut self, name: &T) -> Result<&mut SymTableEntry, SymTableError> {
        self.get_mut(name.to_lowercase().as_ref())
            .ok_or_else(|| SymTableError::NameNotFound(name.clone().into()))
    }
    fn emplace(&mut self, name: &T, entry: SymTableEntry) -> &mut SymTableEntry {
        let key = name.to_lowercase().take();
        debug_assert!(!self.contains_key(&key));
        self.entry(key).or_insert(entry)
    }
}

impl Lookup<ProperIdentifier> for BTreeMap<ProperIdentifier, SymTableEntry> {
    fn lookup(&self, name: &ProperIdentifier) -> Result<&SymTableEntry, SymTableError> {
        self.get(name.to_lowercase().as_ref())
            .ok_or_else(|| SymTableError::NameNotFound(name.clone().into()))
    }
    fn lookup_mut(&mut self, name: &ProperIdentifier) -> Result<&mut SymTableEntry, SymTableError> {
        self.get_mut(name.to_lowercase().as_ref())
            .ok_or_else(|| SymTableError::NameNotFound(name.clone().into()))
    }
    fn emplace(&mut self, name: &ProperIdentifier, entry: SymTableEntry) -> &mut SymTableEntry {
        let key = name.to_lowercase().take();
        debug_assert!(!self.contains_key(&key));
        self.entry(key).or_insert(entry)
    }
}

impl SymTable {
    pub fn new() -> Self {
        Default::default()
    }

    fn lookup(&self, name: &VariableName) -> Result<&SymTableEntry, SymTableError> {
        match name {
            VariableName::Simple(name) => self.simple.lookup(name),
            VariableName::Common(name) => self.common.lookup(name),
            VariableName::Proper(name) => self.proper.lookup(name),
        }
    }
    fn lookup_mut(&mut self, name: &VariableName) -> Result<&mut SymTableEntry, SymTableError> {
        match name {
            VariableName::Simple(name) => self.simple.lookup_mut(name),
            VariableName::Common(name) => self.common.lookup_mut(name),
            VariableName::Proper(name) => self.proper.lookup_mut(name),
        }
    }
    pub fn emplace_var(&mut self, name: &VariableName) -> &mut Val {
        let val = Val::default().into();
        match name {
            VariableName::Simple(name) => self.simple.emplace(name, val),
            VariableName::Common(name) => self.common.emplace(name, val),
            VariableName::Proper(name) => self.proper.emplace(name, val),
        }
        .as_var_mut()
        .unwrap()
    }
    pub fn emplace_func(&mut self, name: &VariableName, func: Arc<FunctionData>) -> &FunctionData {
        let func = func.into();
        match name {
            VariableName::Simple(name) => self.simple.emplace(name, func),
            VariableName::Common(name) => self.common.emplace(name, func),
            VariableName::Proper(name) => self.proper.emplace(name, func),
        }
        .as_func()
        .unwrap()
    }

    pub fn lookup_var(&self, name: &VariableName) -> Result<&Val, SymTableError> {
        self.lookup(name)?
            .as_var()
            .ok_or_else(|| SymTableError::ExpectedVarFoundFunc(name.clone()))
    }
    pub fn lookup_var_mut(&mut self, name: &VariableName) -> Result<&mut Val, SymTableError> {
        self.lookup_mut(name)?
            .as_var_mut()
            .ok_or_else(|| SymTableError::ExpectedVarFoundFunc(name.clone()))
    }

    pub fn lookup_func(&self, name: &VariableName) -> Result<&FunctionData, SymTableError> {
        self.lookup(name)?
            .as_func()
            .ok_or_else(|| SymTableError::ExpectedFuncFoundVar(name.clone()))
    }
}
