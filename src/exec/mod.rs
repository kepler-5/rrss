use derive_more::From;

use crate::{analysis::visit::VisitProgram, frontend::ast::Program};

use self::{
    environment::{Environment, EnvironmentError},
    exec_stmt::{ExecError, ExecStmt},
    produce_val::ProduceValError,
    val::ValError,
    write_val::WriteValError,
};

pub mod display;
pub mod environment;
pub mod exec_stmt;
pub mod produce_val;
pub mod sym_table;
pub mod val;
pub mod write_val;

#[derive(Debug, From, PartialEq)]
pub enum RuntimeError {
    EnvironmentError(EnvironmentError),
    ValError(ValError),
    WriteValError(WriteValError),
    ExecError(ExecError),
    ProduceValError(ProduceValError),
}

pub fn exec(program: &Program) -> Result<(), RuntimeError> {
    let env = Environment::refcell();
    ExecStmt::new(&env).visit_program(program)
}
