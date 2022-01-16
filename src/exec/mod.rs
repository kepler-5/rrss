use derive_more::From;

use self::{
    environment::EnvironmentError, exec_stmt::ExecError, val::ValueError, write_val::WriteValError,
};

pub mod environment;
pub mod exec_stmt;
pub mod interpreter;
pub mod produce_val;
pub mod sym_table;
pub mod val;
pub mod write_val;

#[derive(Debug, From, PartialEq)]
pub enum RuntimeError {
    EnvironmentError(EnvironmentError),
    ValueError(ValueError),
    WriteValError(WriteValError),
    ExecError(ExecError),
}
