use std::hint::unreachable_unchecked;

use derive_more::{Constructor, IsVariant};

use crate::{
    analysis::visit::{combine_all, Combine, VisitProgram},
    frontend::ast::Program,
    linter::passes::boring_assignment::BoringAssignmentPass,
};

pub mod display;
pub mod passes;
pub mod render;
#[cfg(test)]
mod tests;

#[derive(Clone, Constructor, Debug, PartialEq, Eq)]
pub struct Diag {
    pub issue: String,
    pub suggestions: Vec<String>,
    pub line: u32,
}

#[derive(Clone, Debug, IsVariant, PartialEq, Eq)]
pub enum ListBuilder<T> {
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

type DiagsBuilder = ListBuilder<Diag>;

pub trait Pass: VisitProgram<Output = DiagsBuilder, Error = ()> {}

pub type Passes = Vec<Box<dyn Pass>>;

#[derive(Constructor)]
pub struct Linter {
    passes: Passes,
}

#[derive(Constructor)]
pub struct LinterResult {
    diags: Vec<Diag>,
}

impl Linter {
    pub fn run(&mut self, program: &Program) -> LinterResult {
        LinterResult::new(
            combine_all(self.passes.iter_mut().map(|p| p.visit_program(&program)))
                .unwrap_or_default()
                .build(),
        )
    }
}

pub fn standard_passes() -> Passes {
    vec![Box::new(BoringAssignmentPass)]
}

pub fn standard_linter() -> Linter {
    Linter::new(standard_passes())
}
