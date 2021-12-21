use std::fmt::Display;

use itertools::repeat_n;

use crate::{
    analysis::{
        tools::{
            ConstantFoldingError, NumericConstant, NumericConstantFolder,
            SimpleStringConstantFolder, StringConstant,
        },
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
    fn from_value(val: NumericConstant) -> Self {
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

fn numeric_suggestion_payload(var: &impl Render, val: NumericConstant) -> String {
    format!(
        "{} is {}",
        var.render(),
        PoeticNumberLiteralTemplate::from_value(val).as_text()
    )
}

fn string_suggestion_payload(var: &impl Render, val: &StringConstant) -> String {
    format!("{} says {}", var.render(), val.value)
}

fn suggestion_text(payload: &str) -> String {
    format!("Consider using a poetic literal such as: `{}`", payload)
}

fn build_diag<Constant: Display>(
    var: &impl Render,
    suggestion: &str,
    val: Constant,
) -> DiagsBuilder {
    DiagsBuilder::One(Diag {
        issue: issue_text(var, &val),
        suggestions: vec![suggestion_text(suggestion)],
    })
}

fn build_numeric_diag(var: &impl Render, val: NumericConstant) -> DiagsBuilder {
    build_diag(var, &numeric_suggestion_payload(var, val), val)
}

fn maybe_build_string_diag(var: &impl Render, val: Option<StringConstant>) -> DiagsBuilder {
    val.map(|val| build_diag(var, &string_suggestion_payload(var, &val), val))
        .unwrap_or_default()
}

fn array_push_suggestion_payload(var: &impl Render, val: NumericConstant) -> String {
    format!(
        "Rock {} like {}",
        var.render(),
        PoeticNumberLiteralTemplate::from_value(val).as_text()
    )
}

fn maybe_build_numeric_array_push_diag(
    var: &impl Render,
    val: Option<NumericConstant>,
) -> DiagsBuilder {
    val.map(|val| build_diag(var, &array_push_suggestion_payload(var, val), val))
        .unwrap_or_default()
}

#[cfg(test)]
mod tests;

struct BoringAssignmentPass;

impl BoringAssignmentPass {
    fn find_boring_array_push_rhs(a: &ArrayPushRHS) -> Option<NumericConstant> {
        match a {
            ArrayPushRHS::ExpressionList(el) => {
                NumericConstantFolder.visit_expression_list(el).ok()
            }
            ArrayPushRHS::PoeticNumberLiteral(_) => None,
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
            match NumericConstantFolder.visit_expression_list(&a.value) {
                Ok(x) => build_numeric_diag(&a.dest, x),
                Err(ConstantFoldingError::WrongType) => maybe_build_string_diag(
                    &a.dest,
                    SimpleStringConstantFolder
                        .visit_expression_list(&a.value)
                        .ok(),
                ),
                _ => Self::Output::Empty,
            }
        })
    }
    fn visit_poetic_number_assignment(&mut self, a: &PoeticNumberAssignment) -> walk::Result<Self> {
        Ok(match &a.rhs {
            PoeticNumberAssignmentRHS::Expression(e) => {
                match NumericConstantFolder.visit_expression(e) {
                    Ok(x) => build_numeric_diag(&a.dest, x),
                    Err(ConstantFoldingError::WrongType) => maybe_build_string_diag(
                        &a.dest,
                        SimpleStringConstantFolder.visit_expression(e).ok(),
                    ),
                    Err(_) => Self::Output::Empty,
                }
            }
            PoeticNumberAssignmentRHS::PoeticNumberLiteral(_) => Self::Output::Empty,
        })
    }
    fn visit_array_push(&mut self, a: &ArrayPush) -> walk::Result<Self> {
        Ok(maybe_build_numeric_array_push_diag(
            &a.array,
            Self::find_boring_array_push_rhs(&a.value),
        ))
    }
}

impl Pass for BoringAssignmentPass {}
