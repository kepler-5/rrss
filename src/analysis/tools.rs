use std::iter;

use derive_more::{Add, From, Neg, Sub};

use super::walk::{self, Combine, Visitor};
use crate::frontend::ast::*;

#[cfg(test)]
mod tests;

#[derive(Add, Sub, Neg, Copy, Clone, Debug, Default, From, PartialEq, PartialOrd)]
pub struct NumericConstant {
    pub value: f64,
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
}

use ConstantFoldingError::*;

pub struct NumericConstantFolder;

impl Visitor for NumericConstantFolder {
    type Output = NumericConstant;
    type Error = ConstantFoldingError;

    fn visit_program(&mut self, _: &Program) -> walk::Result<Self> {
        Err(NoType)
    }

    fn visit_block(&mut self, _: &Block) -> walk::Result<Self> {
        Err(NoType)
    }

    fn visit_statement(&mut self, _: &Statement) -> walk::Result<Self> {
        Err(NoType)
    }

    // Statements
    fn visit_assignment(&mut self, _: &Assignment) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_poetic_assignment(&mut self, _: &PoeticAssignment) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_if(&mut self, _: &If) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_while(&mut self, _: &While) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_until(&mut self, _: &Until) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_inc(&mut self, _: &Inc) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_dec(&mut self, _: &Dec) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_input(&mut self, _: &Input) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_output(&mut self, _: &Output) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_mutation(&mut self, _: &Mutation) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_rounding(&mut self, _: &Rounding) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_continue(&mut self) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_break(&mut self) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_array_push(&mut self, _: &ArrayPush) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_array_pop(&mut self, _: &ArrayPop) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_return(&mut self, _: &Return) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_function(&mut self, _: &Function) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_function_call(&mut self, _: &FunctionCall) -> walk::Result<Self> {
        Err(UnknownValue)
    }

    // Statement helper types
    fn visit_poetic_number_assignment(&mut self, _: &PoeticNumberAssignment) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_poetic_string_assignment(&mut self, _: &PoeticStringAssignment) -> walk::Result<Self> {
        Err(NoType)
    }
    fn visit_poetic_number_literal(&mut self, p: &PoeticNumberLiteral) -> walk::Result<Self> {
        Ok(p.compute_value().into())
    }

    // Expressions
    fn visit_expression_list(&mut self, e: &ExpressionList) -> walk::Result<Self> {
        e.rest
            .is_empty()
            .then(|| self.visit_expression(&e.first))
            .unwrap_or(Err(NeedMoreInfo))
    }
    fn visit_primary_expression(&mut self, e: &PrimaryExpression) -> walk::Result<Self> {
        match e {
            PrimaryExpression::Literal(e) => self.visit_literal_expression(e),
            PrimaryExpression::Identifier(i) => self.visit_identifier(i),
            PrimaryExpression::ArraySubscript(a) => self.visit_array_subsript(a),
            PrimaryExpression::FunctionCall(f) => self.visit_function_call(f),
        }
    }
    fn visit_binary_expression(&mut self, e: &BinaryExpression) -> walk::Result<Self> {
        let lhs = self.visit_expression(&e.lhs)?;
        let mut rhs = iter::once(&e.rhs.first)
            .chain(e.rhs.rest.iter())
            .map(|e| self.visit_expression(e));
        let op = |a: NumericConstant, b: walk::Result<Self>| match e.operator {
            BinaryOperator::Plus => b.map(|b| a + b),
            BinaryOperator::Minus => b.map(|b| a - b),
            BinaryOperator::Multiply => b.map(|b| a * b),
            BinaryOperator::Divide => b.map(|b| a / b),
            _ => Err(WrongType),
        };
        rhs.try_fold(lhs, op)
    }
    fn visit_unary_expression(&mut self, e: &UnaryExpression) -> walk::Result<Self> {
        let operand = self.visit_expression(&e.operand)?;
        match e.operator {
            UnaryOperator::Minus => Ok(-operand),
            UnaryOperator::Not => Err(WrongType),
        }
    }
    fn visit_array_subsript(&mut self, _: &ArraySubscript) -> walk::Result<Self> {
        Err(UnknownValue)
    }
    fn visit_literal_expression(&mut self, e: &LiteralExpression) -> walk::Result<Self> {
        match e {
            LiteralExpression::Number(x) => Ok(NumericConstant::from(*x)),
            _ => Err(WrongType),
        }
    }

    // Identifiers
    fn visit_pronoun(&mut self) -> walk::Result<Self> {
        Err(UnknownValue)
    }
    fn visit_simple_identifier(&mut self, _: &SimpleIdentifier) -> walk::Result<Self> {
        Err(UnknownValue)
    }
    fn visit_common_identifier(&mut self, _: &CommonIdentifier) -> walk::Result<Self> {
        Err(UnknownValue)
    }
    fn visit_proper_identifier(&mut self, _: &ProperIdentifier) -> walk::Result<Self> {
        Err(UnknownValue)
    }
}
