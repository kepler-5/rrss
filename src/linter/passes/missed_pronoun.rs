use crate::{
    analysis::visit::{self, combine_all, ExprVisitorRunner, Visit, VisitExpr},
    frontend::{
        ast::{FunctionCall, VariableName, WithRange},
        source_range::Line,
    },
    linter::{Diag, DiagsBuilder, Pass, Render},
};
use std::iter;

#[cfg(test)]
pub mod tests;

fn build_diag(name: &VariableName, line: u32) -> DiagsBuilder {
    DiagsBuilder::One(Diag {
        issue: format!(
            "Using identifier `{}` more than once in a row sounds kinda bad",
            name.render()
        ),
        suggestions: vec!["Consider using a pronoun such as `it`".into()],
        line,
    })
}

pub struct MissedPronounPassImpl {
    last: Option<VariableName>,
    in_function_call: bool,
}

impl MissedPronounPassImpl {
    fn new() -> Self {
        Self {
            last: None,
            in_function_call: false,
        }
    }

    fn match_or_update(&mut self, name: &VariableName) -> bool {
        let is_match = !self.in_function_call && self.last.as_ref().map_or(false, |n| n == name);
        if !is_match {
            self.last = Some(name.clone());
        }
        is_match
    }
}

impl Visit for MissedPronounPassImpl {
    type Output = DiagsBuilder;
    type Error = ();
}

impl VisitExpr for MissedPronounPassImpl {
    fn visit_variable_name(&mut self, n: WithRange<&VariableName>) -> visit::Result<Self> {
        Ok(self
            .match_or_update(n.0)
            .then(|| build_diag(n.0, n.line()))
            .unwrap_or_default())
    }
    fn visit_function_call(&mut self, f: &FunctionCall) -> visit::Result<Self> {
        debug_assert!(!self.in_function_call);
        self.in_function_call = true;
        let name = self.visit_variable_name(f.name.as_ref());
        self.in_function_call = false;
        combine_all(iter::once(name).chain(f.args.iter().map(|e| self.visit_expression(e))))
    }
}

pub type MissedPronounPass = ExprVisitorRunner<MissedPronounPassImpl>;

impl MissedPronounPass {
    pub fn new() -> Self {
        Self::with_inner(MissedPronounPassImpl::new())
    }
}

impl Pass for MissedPronounPass {}
