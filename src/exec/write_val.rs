use std::cell::RefCell;

use derive_more::{Constructor, From};

use crate::{
    analysis::visit::{self, Combine, Visit, VisitExpr},
    exec::{environment::Environment, produce_val::ProduceVal, val::Val, RuntimeError},
    frontend::{ast::*, source_range::SourceRange},
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum WriteValError {
    ValueNotWritable,
}

#[cfg(test)]
pub mod tests;

#[derive(Constructor)]
pub struct WriteVal<'a, W: Fn(&mut Val) -> ()> {
    env: &'a RefCell<Environment>,
    write: W,
}

#[must_use = "may contain error that must be propagated"]
#[derive(Debug, From, PartialEq)]
pub struct WriteValOutput(pub Result<(), RuntimeError>);

fn not_writable_error() -> Result<(), RuntimeError> {
    Err(WriteValError::ValueNotWritable.into()).into()
}

impl<'a> Combine for WriteValOutput {
    fn combine(self, _: Self) -> Self {
        not_writable_error().into()
    }
}

impl<'a> Default for WriteValOutput {
    fn default() -> Self {
        not_writable_error().into()
    }
}

impl<'a, W: Fn(&mut Val) -> ()> Visit for WriteVal<'a, W> {
    type Output = WriteValOutput;
    type Error = ();
}

fn wrap<'a, F: Fn() -> Result<(), RuntimeError>>(f: F) -> Result<WriteValOutput, ()> {
    Ok(WriteValOutput(f()))
}

// macro instead of a function to work around borrow checking woes
macro_rules! lookup_or_create {
    ($e:expr, $name:expr) => {
        if let Ok(var) = $e.lookup_var_mut($name) {
            var
        } else {
            $e.create_var($name)
        }
    };
}

fn subscript_val(e: &RefCell<Environment>, a: &ArraySubscript) -> Result<Val, RuntimeError> {
    Ok(ProduceVal::new(e).visit_primary_expression(&a.subscript)?.0)
}

impl<'a, W: Fn(&mut Val) -> ()> VisitExpr for WriteVal<'a, W> {
    fn visit_array_subsript(&mut self, a: &ArraySubscript) -> visit::Result<Self> {
        wrap(|| {
            // drill down through nested subscripts until we find a stopping point (an identifier)
            let mut subscripts = vec![subscript_val(self.env, a)?];
            let mut arr: &PrimaryExpression = &a.array;
            loop {
                match arr {
                    PrimaryExpression::Identifier(i) => {
                        let mut e = self.env.borrow_mut();
                        let mut var = match &i.0 {
                            Identifier::VariableName(name) => lookup_or_create!(e, &name),
                            Identifier::Pronoun => e.last_access_mut()?,
                        };
                        while let Some(back) = subscripts.pop() {
                            var = var.index_or_insert(&back)?;
                        }
                        return Ok((self.write)(var));
                    }
                    PrimaryExpression::ArraySubscript(a) => {
                        subscripts.push(subscript_val(self.env, a)?);
                        arr = &a.array;
                    }
                    _ => return not_writable_error(),
                }
            }
        })
    }

    fn visit_pronoun(&mut self, _: SourceRange) -> visit::Result<Self> {
        wrap(|| Ok((self.write)(self.env.borrow_mut().last_access_mut()?)))
    }

    fn visit_variable_name(&mut self, n: WithRange<&VariableName>) -> visit::Result<Self> {
        wrap(|| {
            let mut e = self.env.borrow_mut();
            Ok((self.write)(lookup_or_create!(e, &n.0)))
        })
    }
}
