use std::fmt::Display;

use crate::{
    analysis::{
        tools::{NumericConstant, NumericConstantFolder},
        walk::{self, Visitor},
    },
    frontend::ast::*,
    linter::{render::Render, Diag, DiagsBuilder, Pass},
};

fn issue_text(var: &impl Render, value: &impl Display) -> String {
    format!(
        "Assignment of literal value `{}` into `{}` isn't very rock'n'roll",
        value,
        var.render()
    )
}

fn build_numeric_diag<E>(var: &impl Render, val: Result<NumericConstant, E>) -> DiagsBuilder {
    match val {
        Ok(val) => DiagsBuilder::One(Diag {
            issue: issue_text(var, &val.value),
            suggestions: Vec::new(),
        }),
        Err(_) => DiagsBuilder::Empty,
    }
}

#[cfg(test)]
mod tests;

struct BoringAssignmentPass;

impl Visitor for BoringAssignmentPass {
    type Output = DiagsBuilder;
    type Error = ();

    fn visit_assignment(&mut self, a: &Assignment) -> walk::Result<Self> {
        Ok(if a.operator.is_some() {
            Self::Output::Empty
        } else {
            build_numeric_diag(
                &a.dest,
                NumericConstantFolder.visit_expression_list(&a.value),
            )
        })
    }
    fn visit_poetic_number_assignment(&mut self, a: &PoeticNumberAssignment) -> walk::Result<Self> {
        Ok(build_numeric_diag(
            &a.dest,
            NumericConstantFolder.visit_poetic_number_assignment_rhs(&a.rhs),
        ))
    }
}

impl Pass for BoringAssignmentPass {}
