use std::{borrow::Cow, cmp::Ordering, iter::once};

use derive_more::Constructor;

use crate::{
    analysis::visit::{self, Combine, Visit, VisitExpr},
    frontend::{ast::*, source_range::SourceRange},
};

use super::{environment::Environment, val::Val, RuntimeError};

#[cfg(test)]
mod tests;

#[derive(Constructor)]
pub struct ProduceVal<'a> {
    env: &'a Environment,
}

pub struct ProduceValOutput<'a>(pub Cow<'a, Val>);

impl<'a> Combine for ProduceValOutput<'a> {
    fn combine(self, _: Self) -> Self {
        unimplemented!()
    }
}

impl<'a> Default for ProduceValOutput<'a> {
    fn default() -> Self {
        unimplemented!()
    }
}

impl<'a> Visit for ProduceVal<'a> {
    type Output = ProduceValOutput<'a>;
    type Error = RuntimeError;
}

impl<'a> VisitExpr for ProduceVal<'a> {
    fn visit_poetic_number_literal(&mut self, p: &PoeticNumberLiteral) -> visit::Result<Self> {
        Ok(ProduceValOutput(Cow::Owned(Val::Number(p.compute_value()))))
    }

    fn visit_array_push_rhs(&mut self, a: &ArrayPushRHS) -> visit::Result<Self> {
        match a {
            ArrayPushRHS::ExpressionList(e) => self.visit_expression_list(e),
            ArrayPushRHS::PoeticNumberLiteral(p) => self.visit_poetic_number_literal(p),
        }
    }

    fn visit_binary_expression(&mut self, e: &BinaryExpression) -> visit::Result<Self> {
        let lhs = self.visit_expression(&e.lhs)?;
        let mut rhs = once(&e.rhs.first)
            .chain(e.rhs.rest.iter())
            .map(|e| self.visit_expression(e));
        let op = |a: ProduceValOutput, b: visit::Result<Self>| {
            match e.operator {
                BinaryOperator::Plus => Ok(a.0.plus(&b?.0)?),
                BinaryOperator::Minus => Ok(a.0.subtract(&b?.0)?),
                BinaryOperator::Multiply => Ok(a.0.multiply(&b?.0)?),
                BinaryOperator::Divide => Ok(a.0.divide(&b?.0)?),

                BinaryOperator::And => Ok(Val::Boolean(a.0.is_truthy() && b?.0.is_truthy())),
                BinaryOperator::Or => Ok(Val::Boolean(a.0.is_truthy() || b?.0.is_truthy())),
                BinaryOperator::Nor => Ok(Val::Boolean(!a.0.is_truthy() && !b?.0.is_truthy())),

                BinaryOperator::Eq => Ok(Val::Boolean(a.0.equals(&b?.0))),
                BinaryOperator::NotEq => Ok(Val::Boolean(!a.0.equals(&b?.0))),

                BinaryOperator::Greater => Ok(Val::Boolean(
                    a.0.compare(&b?.0)?
                        .map(|o| o == Ordering::Greater)
                        .unwrap_or(false),
                )),
                BinaryOperator::GreaterEq => Ok(Val::Boolean(
                    a.0.compare(&b?.0)?
                        .map(|o| o != Ordering::Less)
                        .unwrap_or(false),
                )),
                BinaryOperator::Less => Ok(Val::Boolean(
                    a.0.compare(&b?.0)?
                        .map(|o| o == Ordering::Less)
                        .unwrap_or(false),
                )),
                BinaryOperator::LessEq => Ok(Val::Boolean(
                    a.0.compare(&b?.0)?
                        .map(|o| o != Ordering::Greater)
                        .unwrap_or(false),
                )),
            }
            .map(|val| ProduceValOutput(Cow::Owned(val)))
        };
        rhs.try_fold(lhs, op)
    }

    fn visit_unary_expression(&mut self, e: &UnaryExpression) -> visit::Result<Self> {
        Ok(self
            .visit_unary_operator(e.operator)?
            .combine(self.visit_expression(&e.operand)?))
    }

    fn visit_array_subsript(&mut self, a: &ArraySubscript) -> visit::Result<Self> {
        Ok(self
            .visit_primary_expression(&a.array)?
            .combine(self.visit_primary_expression(&a.subscript)?))
    }

    fn visit_literal_expression(
        &mut self,
        e: &WithRange<LiteralExpression>,
    ) -> visit::Result<Self> {
        match &e.0 {
            LiteralExpression::Boolean(b) => Ok(ProduceValOutput(Cow::Owned(Val::Boolean(*b)))),
            LiteralExpression::Null => Ok(ProduceValOutput(Cow::Owned(Val::Null))),
            LiteralExpression::Number(n) => Ok(ProduceValOutput(Cow::Owned(Val::Number(*n)))),
            LiteralExpression::String(s) => {
                Ok(ProduceValOutput(Cow::Owned(Val::String(s.clone()))))
            }
        }
    }

    fn visit_function_call(&mut self, f: &FunctionCall) -> visit::Result<Self> {
        crate::analysis::visit::combine_all(
            std::iter::once(self.visit_variable_name(f.name.as_ref()))
                .chain(f.args.iter().map(|e| self.visit_primary_expression(e))),
        )
    }

    fn visit_pronoun(&mut self, _: SourceRange) -> visit::Result<Self> {
        self.env
            .last_access()
            .map(|val| ProduceValOutput(Cow::Borrowed(val)))
            .map_err(Into::into)
    }

    fn visit_variable_name(
        &mut self,
        n: WithRange<&VariableName>,
    ) -> std::result::Result<ProduceValOutput<'a>, Self::Error> {
        self.env
            .lookup_var(n.0)
            .map(|val| ProduceValOutput(Cow::Borrowed(val)))
            .map_err(Into::into)
    }
}
