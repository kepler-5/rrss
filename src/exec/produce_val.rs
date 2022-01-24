use std::{
    cell::RefCell,
    cmp::Ordering,
    io::{Read, Write},
    iter::once,
};

use derive_more::{Constructor, From};
use smallvec::SmallVec;
use unchecked_unwrap::UncheckedUnwrap;

use crate::{
    analysis::visit::{self, Combine, Visit, VisitExpr, VisitProgram},
    exec::{environment::Environment, val::Val, write_val::WriteVal, RuntimeError},
    frontend::{ast::*, source_range::SourceRange},
};

use super::exec_stmt::ExecStmt;

#[derive(Debug, PartialEq)]
pub enum ProduceValError {
    WrongNumberOfFunctionArguments { expected: usize, actual: usize },
}

#[cfg(test)]
mod tests;

#[derive(Constructor)]
pub struct ProduceVal<'a, I, O> {
    env: &'a RefCell<Environment<I, O>>,
}

#[derive(Debug, From, PartialEq)]
pub struct ProduceValOutput(pub Val);

impl<'a> Combine for ProduceValOutput {
    fn combine(self, _: Self) -> Self {
        unimplemented!()
    }
}

impl<'a> Default for ProduceValOutput {
    fn default() -> Self {
        unimplemented!()
    }
}

impl<'a, I, O> Visit for ProduceVal<'a, I, O> {
    type Output = ProduceValOutput;
    type Error = RuntimeError;
}

impl<'a, I, O> VisitExpr for ProduceVal<'a, I, O>
where
    I: Read,
    O: Write,
{
    fn visit_poetic_number_literal(&mut self, p: &PoeticNumberLiteral) -> visit::Result<Self> {
        Ok(Val::Number(p.compute_value()).into())
    }

    fn visit_array_push_rhs(&mut self, _: &ArrayPushRHS) -> visit::Result<Self> {
        unimplemented!("expression list arm must be handled by ExecStmt")
    }

    fn visit_array_pop_expr(&mut self, a: &ArrayPopExpr) -> visit::Result<Self> {
        let mut back = None;
        WriteVal::new(self.env, |val| {
            back = Some(val.pop()?);
            Ok(())
        })
        .visit_primary_expression(&a.array)
        .unwrap()
        .0?;
        Ok(ProduceValOutput(unsafe { back.unchecked_unwrap() }))
    }

    fn visit_binary_expression(&mut self, e: &BinaryExpression) -> visit::Result<Self> {
        let lhs = self.visit_expression(&e.lhs)?.0;
        let rhs = once(&e.rhs.first)
            .chain(e.rhs.rest.iter())
            .map(|e| self.visit_expression(e).map(|p| p.0));
        binary_operator_fold(e.operator, lhs, rhs).map(ProduceValOutput)
    }

    fn visit_unary_expression(&mut self, e: &UnaryExpression) -> visit::Result<Self> {
        let operand = self.visit_expression(&e.operand)?.0;
        Ok(ProduceValOutput(match e.operator {
            UnaryOperator::Minus => operand.negate()?,
            UnaryOperator::Not => Val::Boolean(!operand.is_truthy()),
        }))
    }

    fn visit_array_subscript(&mut self, a: &ArraySubscript) -> visit::Result<Self> {
        let array = self.visit_primary_expression(&a.array)?.0;
        let subscript = self.visit_primary_expression(&a.subscript)?.0;
        Ok(array
            .index(&subscript)
            .map(|cow| ProduceValOutput(cow.into_owned()))?)
    }

    fn visit_literal_expression(
        &mut self,
        e: &WithRange<LiteralExpression>,
    ) -> visit::Result<Self> {
        match &e.0 {
            LiteralExpression::Mysterious => Ok(Val::Undefined.into()),
            LiteralExpression::Boolean(b) => Ok(Val::Boolean(*b).into()),
            LiteralExpression::Null => Ok(Val::Null.into()),
            LiteralExpression::Number(n) => Ok(Val::Number(*n).into()),
            LiteralExpression::String(s) => Ok(Val::from(s.clone()).into()),
        }
    }

    fn visit_function_call(&mut self, f: &FunctionCall) -> visit::Result<Self> {
        let data = self.env.borrow().lookup_func(&f.name.0)?;
        if data.params.len() != f.args.len() {
            return Err(ProduceValError::WrongNumberOfFunctionArguments {
                expected: data.params.len(),
                actual: f.args.len(),
            }
            .into());
        }
        let mut args = SmallVec::<[Val; 8]>::new();
        for arg in &f.args {
            args.push(self.visit_expression(arg)?.0);
        }

        self.env
            .borrow_mut()
            .push_function_scope(data.params.iter().map(|a| &a.0).zip(args.into_iter()))?;
        let mut exec = ExecStmt::new(self.env);
        exec.visit_block(&data.body)?;
        self.env.borrow_mut().pop_scope();
        Ok(ProduceValOutput(exec.return_val()))
    }

    fn visit_pronoun(&mut self, _: SourceRange) -> visit::Result<Self> {
        self.env
            .borrow_mut()
            .last_access()
            .map(|val| val.clone().into())
            .map_err(Into::into)
    }

    fn visit_variable_name(&mut self, n: WithRange<&VariableName>) -> visit::Result<Self> {
        self.env
            .borrow_mut()
            .lookup_var(n.0)
            .map(|val| val.clone().into())
            .map_err(Into::into)
    }
}

pub fn binary_operator_fold(
    operator: BinaryOperator,
    lhs: Val,
    mut rhs: impl Iterator<Item = Result<Val, RuntimeError>>,
) -> Result<Val, RuntimeError> {
    let op = |a: Val, b: Result<Val, RuntimeError>| match operator {
        BinaryOperator::Plus => Ok(a.plus(&b?)),
        BinaryOperator::Minus => Ok(a.subtract(&b?)),
        BinaryOperator::Multiply => Ok(a.multiply(&b?)),
        BinaryOperator::Divide => Ok(a.divide(&b?)),

        BinaryOperator::And => Ok(Val::Boolean(a.is_truthy() && b?.is_truthy())),
        BinaryOperator::Or => Ok(Val::Boolean(a.is_truthy() || b?.is_truthy())),
        BinaryOperator::Nor => Ok(Val::Boolean(!a.is_truthy() && !b?.is_truthy())),

        BinaryOperator::Eq => Ok(Val::Boolean(a.equals(&b?))),
        BinaryOperator::NotEq => Ok(Val::Boolean(!a.equals(&b?))),

        BinaryOperator::Greater => Ok(Val::Boolean(
            a.compare(&b?)?
                .map(|o| o == Ordering::Greater)
                .unwrap_or(false),
        )),
        BinaryOperator::GreaterEq => Ok(Val::Boolean(
            a.compare(&b?)?
                .map(|o| o != Ordering::Less)
                .unwrap_or(false),
        )),
        BinaryOperator::Less => Ok(Val::Boolean(
            a.compare(&b?)?
                .map(|o| o == Ordering::Less)
                .unwrap_or(false),
        )),
        BinaryOperator::LessEq => Ok(Val::Boolean(
            a.compare(&b?)?
                .map(|o| o != Ordering::Greater)
                .unwrap_or(false),
        )),
    };
    rhs.try_fold(lhs, op)
}
