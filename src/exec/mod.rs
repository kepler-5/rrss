use derive_more::From;

use self::{environment::EnvironmentError, val::ValueError, write_val::WriteValError};

pub mod environment;
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
}
