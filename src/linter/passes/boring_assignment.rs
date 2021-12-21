use std::fmt::Display;

use itertools::repeat_n;

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

enum PoeticNumberLiteralTemplateItem {
    Word { len: usize },
    Dot,
}

struct PoeticNumberLiteralTemplate(Vec<PoeticNumberLiteralTemplateItem>);

impl PoeticNumberLiteralTemplate {
    fn from_value(val: f64) -> Self {
        Self(
            val.to_string()
                .chars()
                .map(|c| match c {
                    '.' => PoeticNumberLiteralTemplateItem::Dot,
                    _ => PoeticNumberLiteralTemplateItem::Word {
                        len: c as usize - '0' as usize,
                    },
                })
                .collect(),
        )
    }
    fn mod10(len: usize) -> usize {
        match len {
            0 => 10,
            _ => len,
        }
    }
    fn estimate_text_size(&self) -> usize {
        self.0
            .iter()
            .map(|item| match item {
                PoeticNumberLiteralTemplateItem::Word { len } => Self::mod10(*len) + 1,
                PoeticNumberLiteralTemplateItem::Dot => 1,
            })
            .sum()
    }
    fn as_text(&self) -> String {
        let mut bytes = Vec::with_capacity(self.estimate_text_size());
        let mut first = true;
        for item in self.0.iter() {
            match item {
                PoeticNumberLiteralTemplateItem::Word { len } => {
                    if !first {
                        bytes.push(' ' as u8);
                    }
                    bytes.extend(repeat_n('*' as u8, Self::mod10(*len)))
                }
                PoeticNumberLiteralTemplateItem::Dot => bytes.push('.' as u8),
            }
            first = false;
        }
        unsafe { String::from_utf8_unchecked(bytes) }
    }
}

fn numeric_suggestion_payload(var: &impl Render, val: f64) -> String {
    format!(
        "{} is {}",
        var.render(),
        PoeticNumberLiteralTemplate::from_value(val).as_text()
    )
}

fn suggestion_text(payload: &str) -> String {
    format!("Consider using a poetic literal such as: `{}`", payload)
}

fn build_numeric_diag(var: &impl Render, val: Option<NumericConstant>) -> DiagsBuilder {
    val.map(|val| {
        DiagsBuilder::One(Diag {
            issue: issue_text(var, &val.value),
            suggestions: vec![suggestion_text(&numeric_suggestion_payload(var, val.value))],
        })
    })
    .unwrap_or(DiagsBuilder::Empty)
}

#[cfg(test)]
mod tests;

struct BoringAssignmentPass;

impl BoringAssignmentPass {
    fn find_boring_poetic_number_assignment_rhs(
        p: &PoeticNumberAssignmentRHS,
    ) -> Option<NumericConstant> {
        match p {
            PoeticNumberAssignmentRHS::Expression(e) => {
                NumericConstantFolder.visit_expression(e).ok()
            }
            PoeticNumberAssignmentRHS::PoeticNumberLiteral(_) => None,
        }
    }
}

impl Visitor for BoringAssignmentPass {
    type Output = DiagsBuilder;
    type Error = ();

    fn visit_assignment(&mut self, a: &Assignment) -> walk::Result<Self> {
        Ok(if a.operator.is_some() {
            Self::Output::Empty
        } else {
            build_numeric_diag(
                &a.dest,
                NumericConstantFolder.visit_expression_list(&a.value).ok(),
            )
        })
    }
    fn visit_poetic_number_assignment(&mut self, a: &PoeticNumberAssignment) -> walk::Result<Self> {
        Ok(build_numeric_diag(
            &a.dest,
            Self::find_boring_poetic_number_assignment_rhs(&a.rhs),
        ))
    }
}

impl Pass for BoringAssignmentPass {}
