use std::{cell::RefCell, iter::once};

use derive_more::Constructor;

use crate::{
    analysis::visit::{self, Visit, VisitExpr, VisitProgram},
    exec::{
        environment::Environment,
        produce_val::{binary_operator_fold, ProduceVal},
        val::Val,
        write_val::WriteVal,
        RuntimeError,
    },
    frontend::ast::*,
};

#[derive(Debug, PartialEq)]
pub enum ExecError {
    NonCompoundAssignmentExpressionListInvalid,
}

#[cfg(test)]
pub mod tests;

#[derive(Constructor)]
pub struct ExecStmt<'a> {
    env: &'a RefCell<Environment>,
}

impl<'a> ExecStmt<'a> {
    fn producer(&self) -> ProduceVal {
        ProduceVal::new(self.env)
    }

    fn writer(&self, val: Val) -> WriteVal {
        WriteVal::new(self.env, val)
    }
}

impl<'a> Visit for ExecStmt<'a> {
    type Output = ();
    type Error = RuntimeError;
}

impl<'a> VisitProgram for ExecStmt<'a> {
    fn visit_block(&mut self, b: &Block) -> visit::Result<Self> {
        match b {
            Block::Empty(_) => Ok(()),
            Block::NonEmpty(statements) => {
                for s in statements {
                    self.visit_statement(s)?;
                }
                Ok(())
            }
        }
    }

    fn visit_assignment(&mut self, a: &Assignment) -> visit::Result<Self> {
        if let Some(op) = a.operator {
            let lhs = self.producer().visit_assignment_lhs(&a.dest)?.0;
            let rhs = once(&a.value.first)
                .chain(a.value.rest.iter())
                .map(|e| self.producer().visit_expression(e).map(|p| p.0));
            let new_val = binary_operator_fold(op, lhs, rhs)?;
            self.writer(new_val)
                .visit_assignment_lhs(&a.dest)
                .unwrap()
                .0?;
            Ok(())
        } else if a.value.has_multiple() {
            Err(ExecError::NonCompoundAssignmentExpressionListInvalid.into())
        } else {
            let rhs = self.producer().visit_expression(&a.value.first)?.0;
            self.writer(rhs).visit_assignment_lhs(&a.dest).unwrap().0?;
            Ok(())
        }
    }

    fn visit_poetic_number_assignment(
        &mut self,
        a: &PoeticNumberAssignment,
    ) -> visit::Result<Self> {
        let val = self
            .producer()
            .visit_poetic_number_assignment_rhs(&a.rhs)?
            .0;
        self.writer(val).visit_assignment_lhs(&a.dest).unwrap().0
    }

    fn visit_poetic_string_assignment(
        &mut self,
        a: &PoeticStringAssignment,
    ) -> visit::Result<Self> {
        self.writer(Val::from(a.rhs.clone()))
            .visit_assignment_lhs(&a.dest)
            .unwrap()
            .0
    }

    fn visit_if(&mut self, i: &If) -> visit::Result<Self> {
        todo!()
    }

    fn visit_while(&mut self, w: &While) -> visit::Result<Self> {
        Ok(self.visit_block(&w.block)?)
    }

    fn visit_until(&mut self, u: &Until) -> visit::Result<Self> {
        Ok(self.visit_block(&u.block)?)
    }

    fn visit_inc(&mut self, i: &Inc) -> visit::Result<Self> {
        crate::analysis::visit::leaf(i)
    }

    fn visit_dec(&mut self, d: &Dec) -> visit::Result<Self> {
        crate::analysis::visit::leaf(d)
    }

    fn visit_input(&mut self, i: &Input) -> visit::Result<Self> {
        crate::analysis::visit::leaf(i)
    }

    fn visit_output(&mut self, o: &Output) -> visit::Result<Self> {
        crate::analysis::visit::leaf(o)
    }

    fn visit_mutation(&mut self, m: &Mutation) -> visit::Result<Self> {
        self.visit_mutation_operator(m.operator)
    }

    fn visit_rounding(&mut self, r: &Rounding) -> visit::Result<Self> {
        self.visit_rounding_direction(r.direction)
    }

    fn visit_continue(&mut self, c: &Continue) -> visit::Result<Self> {
        crate::analysis::visit::leaf(c)
    }

    fn visit_break(&mut self, b: &Break) -> visit::Result<Self> {
        crate::analysis::visit::leaf(b)
    }

    fn visit_array_push(&mut self, a: &ArrayPush) -> visit::Result<Self> {
        crate::analysis::visit::leaf(a)
    }

    fn visit_array_pop(&mut self, a: &ArrayPop) -> visit::Result<Self> {
        crate::analysis::visit::leaf(a)
    }

    fn visit_return(&mut self, r: &Return) -> visit::Result<Self> {
        crate::analysis::visit::leaf(r)
    }

    fn visit_function(&mut self, f: &Function) -> visit::Result<Self> {
        self.visit_block(&f.body)
    }

    fn visit_function_call_statement(&mut self, f: &FunctionCall) -> visit::Result<Self> {
        crate::analysis::visit::leaf(f)
    }

    fn visit_mutation_operator(&mut self, o: MutationOperator) -> visit::Result<Self> {
        crate::analysis::visit::leaf(o)
    }

    fn visit_rounding_direction(&mut self, r: RoundingDirection) -> visit::Result<Self> {
        crate::analysis::visit::leaf(r)
    }
}
