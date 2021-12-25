use std::{
    fmt::{Display, Write},
    iter,
};

use derive_more::{Add, From, Neg, Sub};

use super::visit::{self, Combine, Visit};
use crate::frontend::{ast::*, source_range::SourceRange};

#[cfg(test)]
mod tests;

#[derive(Add, Sub, Neg, Copy, Clone, Debug, Default, From, PartialEq, PartialOrd)]
pub struct NumericConstant {
    pub value: f64,
}

impl Display for NumericConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

// would like to derive_more these but the generated impls seem to have issues
impl std::ops::Mul for NumericConstant {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        (self.value * rhs.value).into()
    }
}
impl std::ops::Div for NumericConstant {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        (self.value / rhs.value).into()
    }
}

impl Combine for NumericConstant {
    fn combine(self, _: Self) -> Self {
        unimplemented!()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConstantFoldingError {
    NoType,
    UnknownValue,
    WrongType,
    NeedMoreInfo,
    PossibleValueIgnored,
}

use ConstantFoldingError::*;

pub struct NumericConstantFolder;

impl Visit for NumericConstantFolder {
    type Output = NumericConstant;
    type Error = ConstantFoldingError;

    fn visit_program(&mut self, _: &Program) -> visit::Result<Self> {
        Err(NoType)
    }

    fn visit_block(&mut self, _: &Block) -> visit::Result<Self> {
        Err(NoType)
    }

    fn visit_statement(&mut self, _: &Statement) -> visit::Result<Self> {
        Err(NoType)
    }

    // Statements
    fn visit_assignment(&mut self, _: &Assignment) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_poetic_assignment(&mut self, _: &PoeticAssignment) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_if(&mut self, _: &If) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_while(&mut self, _: &While) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_until(&mut self, _: &Until) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_inc(&mut self, _: &Inc) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_dec(&mut self, _: &Dec) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_input(&mut self, _: &Input) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_output(&mut self, _: &Output) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_mutation(&mut self, _: &Mutation) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_rounding(&mut self, _: &Rounding) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_continue(&mut self) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_break(&mut self) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_array_push(&mut self, _: &ArrayPush) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_array_pop(&mut self, _: &ArrayPop) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_return(&mut self, _: &Return) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_function(&mut self, _: &Function) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_function_call(&mut self, _: &FunctionCall) -> visit::Result<Self> {
        Err(UnknownValue)
    }

    // Statement helper types
    fn visit_poetic_number_assignment(
        &mut self,
        _: &PoeticNumberAssignment,
    ) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_poetic_string_assignment(
        &mut self,
        _: &PoeticStringAssignment,
    ) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_poetic_number_literal(&mut self, p: &PoeticNumberLiteral) -> visit::Result<Self> {
        Ok(p.compute_value().into())
    }

    // Expressions
    fn visit_expression_list(&mut self, e: &ExpressionList) -> visit::Result<Self> {
        e.rest
            .is_empty()
            .then(|| self.visit_expression(&e.first))
            .unwrap_or(Err(NeedMoreInfo))
    }
    fn visit_binary_expression(&mut self, e: &BinaryExpression) -> visit::Result<Self> {
        let lhs = self.visit_expression(&e.lhs)?;
        let mut rhs = iter::once(&e.rhs.first)
            .chain(e.rhs.rest.iter())
            .map(|e| self.visit_expression(e));
        let op = |a: NumericConstant, b: visit::Result<Self>| match e.operator {
            BinaryOperator::Plus => b.map(|b| a + b),
            BinaryOperator::Minus => b.map(|b| a - b),
            BinaryOperator::Multiply => b.map(|b| a * b),
            BinaryOperator::Divide => b.map(|b| a / b),
            _ => Err(WrongType),
        };
        rhs.try_fold(lhs, op)
    }
    fn visit_unary_expression(&mut self, e: &UnaryExpression) -> visit::Result<Self> {
        let operand = self.visit_expression(&e.operand)?;
        match e.operator {
            UnaryOperator::Minus => Ok(-operand),
            UnaryOperator::Not => Err(WrongType),
        }
    }
    fn visit_array_subsript(&mut self, _: &ArraySubscript) -> visit::Result<Self> {
        Err(UnknownValue)
    }
    fn visit_literal_expression(
        &mut self,
        e: &WithRange<LiteralExpression>,
    ) -> visit::Result<Self> {
        match e.0 {
            LiteralExpression::Number(x) => Ok(NumericConstant::from(x)),
            _ => Err(WrongType),
        }
    }

    // Identifiers
    fn visit_pronoun(&mut self, _: SourceRange) -> visit::Result<Self> {
        Err(UnknownValue)
    }
    fn visit_simple_identifier(&mut self, _: WithRange<&SimpleIdentifier>) -> visit::Result<Self> {
        Err(UnknownValue)
    }
    fn visit_common_identifier(&mut self, _: WithRange<&CommonIdentifier>) -> visit::Result<Self> {
        Err(UnknownValue)
    }
    fn visit_proper_identifier(&mut self, _: WithRange<&ProperIdentifier>) -> visit::Result<Self> {
        Err(UnknownValue)
    }
}

#[derive(Clone, Debug, Default, From, PartialEq, PartialOrd)]
pub struct StringConstant {
    pub value: String,
}

impl Display for StringConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('"')?;
        f.write_str(&self.value)?;
        f.write_char('"')
    }
}

impl std::ops::Add for StringConstant {
    type Output = String;
    fn add(self, rhs: Self) -> Self::Output {
        (self.value + &rhs.value).into()
    }
}

impl Combine for StringConstant {
    fn combine(self, _: Self) -> Self {
        unimplemented!()
    }
}

pub struct SimpleStringConstantFolder;

impl Visit for SimpleStringConstantFolder {
    type Output = StringConstant;
    type Error = ConstantFoldingError;

    fn visit_program(&mut self, _: &Program) -> visit::Result<Self> {
        Err(NoType)
    }

    fn visit_block(&mut self, _: &Block) -> visit::Result<Self> {
        Err(NoType)
    }

    fn visit_statement(&mut self, _: &Statement) -> visit::Result<Self> {
        Err(NoType)
    }

    // Statements
    fn visit_assignment(&mut self, _: &Assignment) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_poetic_assignment(&mut self, _: &PoeticAssignment) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_if(&mut self, _: &If) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_while(&mut self, _: &While) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_until(&mut self, _: &Until) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_inc(&mut self, _: &Inc) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_dec(&mut self, _: &Dec) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_input(&mut self, _: &Input) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_output(&mut self, _: &Output) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_mutation(&mut self, _: &Mutation) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_rounding(&mut self, _: &Rounding) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_continue(&mut self) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_break(&mut self) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_array_push(&mut self, _: &ArrayPush) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_array_pop(&mut self, _: &ArrayPop) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_return(&mut self, _: &Return) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_function(&mut self, _: &Function) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_function_call(&mut self, _: &FunctionCall) -> visit::Result<Self> {
        Err(UnknownValue)
    }

    // Statement helper types
    fn visit_poetic_number_assignment(
        &mut self,
        _: &PoeticNumberAssignment,
    ) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_poetic_string_assignment(
        &mut self,
        _: &PoeticStringAssignment,
    ) -> visit::Result<Self> {
        Err(NoType)
    }
    fn visit_poetic_number_literal(&mut self, _: &PoeticNumberLiteral) -> visit::Result<Self> {
        Err(WrongType)
    }

    // Expressions
    fn visit_expression_list(&mut self, e: &ExpressionList) -> visit::Result<Self> {
        e.rest
            .is_empty()
            .then(|| self.visit_expression(&e.first))
            .unwrap_or(Err(NeedMoreInfo))
    }
    fn visit_binary_expression(&mut self, _: &BinaryExpression) -> visit::Result<Self> {
        Err(PossibleValueIgnored)
    }
    fn visit_unary_expression(&mut self, _: &UnaryExpression) -> visit::Result<Self> {
        Err(WrongType)
    }
    fn visit_array_subsript(&mut self, _: &ArraySubscript) -> visit::Result<Self> {
        Err(UnknownValue)
    }
    fn visit_literal_expression(
        &mut self,
        e: &WithRange<LiteralExpression>,
    ) -> visit::Result<Self> {
        match &e.0 {
            LiteralExpression::String(x) => Ok(x.clone().into()),
            _ => Err(WrongType),
        }
    }

    // Identifiers
    fn visit_pronoun(&mut self, _: SourceRange) -> visit::Result<Self> {
        Err(UnknownValue)
    }
    fn visit_simple_identifier(&mut self, _: WithRange<&SimpleIdentifier>) -> visit::Result<Self> {
        Err(UnknownValue)
    }
    fn visit_common_identifier(&mut self, _: WithRange<&CommonIdentifier>) -> visit::Result<Self> {
        Err(UnknownValue)
    }
    fn visit_proper_identifier(&mut self, _: WithRange<&ProperIdentifier>) -> visit::Result<Self> {
        Err(UnknownValue)
    }
}
