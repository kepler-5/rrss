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
enum ListBuilder<T> {
    Empty,
    One(T),
    List(Vec<T>),
}

impl<T> Default for ListBuilder<T> {
    fn default() -> Self {
        ListBuilder::Empty
    }
}

impl<T> ListBuilder<T> {
    fn build(self) -> Vec<T> {
        match self {
            ListBuilder::Empty => Vec::new(),
            ListBuilder::One(d) => vec![d],
            ListBuilder::List(ds) => ds,
        }
    }
}

impl<T> Combine for ListBuilder<T> {
    fn combine(self, other: Self) -> Self {
        if other.is_empty() {
            return self;
        }
        if self.is_empty() {
            return other;
        }
        let mut ds = match self {
            ListBuilder::Empty => unsafe { unreachable_unchecked() },
            ListBuilder::One(d) => vec![d],
            ListBuilder::List(ds) => ds,
        };
        match other {
            ListBuilder::Empty => unsafe { unreachable_unchecked() },
            ListBuilder::One(od) => ds.push(od),
            ListBuilder::List(ods) => ds.extend(ods),
        };
        ListBuilder::List(ds)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Diags(pub Vec<Diag>);

type DiagsBuilder = ListBuilder<Diag>;

trait Pass: Visitor<Output = DiagsBuilder, Error = ()> {}

pub struct Linter {
    passes: Vec<Box<dyn Pass>>,
}

impl Linter {
    pub fn run(&mut self, program: &Program) -> Vec<Diag> {
        combine_all(self.passes.iter_mut().map(|p| p.visit_program(&program)))
            .unwrap_or_default()
            .build()
    }
}
