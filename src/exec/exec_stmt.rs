use std::{cell::RefCell, iter::once};

use derive_more::IsVariant;

use crate::{
    analysis::visit::{self, Visit, VisitExpr, VisitProgram},
    exec::{
        environment::Environment,
        produce_val::{binary_operator_fold, ProduceVal},
        val::{Val, ValueError},
        write_val::WriteVal,
        RuntimeError,
    },
    frontend::ast::*,
};

#[cfg(test)]
pub mod tests;

#[derive(Debug, PartialEq)]
pub enum ExecError {
    NonCompoundAssignmentExpressionListInvalid,
}

#[derive(Debug, IsVariant)]
enum ControlFlowState {
    Normal,
    Breaking,
    Continuing,
    Returning,
}

impl ControlFlowState {
    fn skip_rest_of_block(&self) -> bool {
        match self {
            ControlFlowState::Breaking => true,
            ControlFlowState::Continuing => true,
            ControlFlowState::Returning => true,
            _ => false,
        }
    }
}

pub struct ExecStmt<'a> {
    env: &'a RefCell<Environment>,
    control_flow_state: ControlFlowState,
    return_val: Val,
}

impl<'a> ExecStmt<'a> {
    pub fn new(env: &'a RefCell<Environment>) -> Self {
        Self {
            env,
            control_flow_state: ControlFlowState::Normal,
            return_val: Val::Undefined,
        }
    }

    pub fn return_val(self) -> Val {
        self.return_val
    }

    fn producer(&self) -> ProduceVal {
        ProduceVal::new(self.env)
    }

    fn writer(&self, val: Val) -> WriteVal<impl Fn(&mut Val) -> Result<(), ValueError>> {
        self.raw_writer(move |v| {
            *v = val.clone();
            Ok(())
        })
    }

    fn raw_writer<W: Fn(&mut Val) -> Result<(), ValueError>>(&self, w: W) -> WriteVal<W> {
        WriteVal::new(self.env, w)
    }

    fn visit_loop<const INVERT: bool>(
        &mut self,
        condition: &Expression,
        block: &Block,
    ) -> visit::Result<Self> {
        while INVERT ^ self.producer().visit_expression(&condition)?.0.is_truthy() {
            self.env.borrow_mut().push_scope();
            self.visit_block(block)?;
            self.env.borrow_mut().pop_scope();

            match self.control_flow_state {
                ControlFlowState::Normal => {}
                ControlFlowState::Continuing => self.control_flow_state = ControlFlowState::Normal,
                ControlFlowState::Breaking => {
                    self.control_flow_state = ControlFlowState::Normal;
                    break;
                }
                ControlFlowState::Returning => break,
            }
        }
        Ok(())
    }

    fn visit_inc_dec(
        &mut self,
        dest: &WithRange<Identifier>,
        amount: isize,
    ) -> visit::Result<Self> {
        self.raw_writer(|val| val.inc(amount))
            .visit_identifier(&dest)
            .unwrap()
            .0
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
                    if self.control_flow_state.skip_rest_of_block() {
                        break;
                    }
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
        let condition = self
            .producer()
            .visit_expression(&i.condition)?
            .0
            .is_truthy();
        self.env.borrow_mut().push_scope();
        if condition {
            self.visit_block(&i.then_block)?;
        } else if let Some(else_block) = &i.else_block {
            self.visit_block(else_block)?;
        }
        self.env.borrow_mut().pop_scope();
        Ok(())
    }

    fn visit_while(&mut self, w: &While) -> visit::Result<Self> {
        self.visit_loop::<false>(&w.condition, &w.block)
    }

    fn visit_until(&mut self, u: &Until) -> visit::Result<Self> {
        self.visit_loop::<true>(&u.condition, &u.block)
    }

    fn visit_inc(&mut self, i: &Inc) -> visit::Result<Self> {
        self.visit_inc_dec(&i.dest, i.amount)
    }

    fn visit_dec(&mut self, d: &Dec) -> visit::Result<Self> {
        self.visit_inc_dec(&d.dest, -d.amount)
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

    fn visit_continue(&mut self, _: &Continue) -> visit::Result<Self> {
        debug_assert!(self.control_flow_state.is_normal());
        self.control_flow_state = ControlFlowState::Continuing;
        Ok(())
    }

    fn visit_break(&mut self, _: &Break) -> visit::Result<Self> {
        debug_assert!(self.control_flow_state.is_normal());
        self.control_flow_state = ControlFlowState::Breaking;
        Ok(())
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
