use std::hint::unreachable_unchecked;

use derive_more::{Constructor, IsVariant};

use crate::{
    analysis::walk::{combine_all, Combine, Visitor},
    frontend::ast::Program,
};

pub mod passes;
pub mod render;
#[cfg(test)]
mod tests;

#[derive(Clone, Constructor, Debug, PartialEq, Eq)]
pub struct Diag {
    pub issue: String,
    pub suggestions: Vec<String>,
}

#[derive(Clone, Debug, IsVariant, PartialEq, Eq)]
enum DiagsBuilder {
    Empty,
    One(Diag),
    List(Vec<Diag>),
}

impl Default for DiagsBuilder {
    fn default() -> Self {
        DiagsBuilder::Empty
    }
}

impl DiagsBuilder {
    fn into_diags(self) -> Diags {
        match self {
            DiagsBuilder::Empty => Diags(Vec::new()),
            DiagsBuilder::One(d) => Diags(vec![d]),
            DiagsBuilder::List(ds) => Diags(ds),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Diags(pub Vec<Diag>);

impl Combine for DiagsBuilder {
    fn combine(self, other: Self) -> Self {
        if other.is_empty() {
            return self;
        }
        if self.is_empty() {
            return other;
        }
        let mut ds = match self {
            DiagsBuilder::Empty => unsafe { unreachable_unchecked() },
            DiagsBuilder::One(d) => vec![d],
            DiagsBuilder::List(ds) => ds,
        };
        match other {
            DiagsBuilder::Empty => unsafe { unreachable_unchecked() },
            DiagsBuilder::One(od) => ds.push(od),
            DiagsBuilder::List(ods) => ds.extend(ods),
        };
        DiagsBuilder::List(ds)
    }
}

trait Pass: Visitor<Output = DiagsBuilder, Error = ()> {}

pub struct Linter {
    passes: Vec<Box<dyn Pass>>,
}

impl Linter {
    pub fn run(&mut self, program: &Program) -> Diags {
        combine_all(self.passes.iter_mut().map(|p| p.visit_program(&program)))
            .unwrap_or_default()
            .into_diags()
    }
}
