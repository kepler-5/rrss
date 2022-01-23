use super::{
    sym_table::SymTableError, EnvironmentError, ExecError, ProduceValError, RuntimeError, ValError,
    WriteValError,
};

use crate::linter::render::Render; // TODO bad dependency to have, factor Render out somewhere else

impl std::fmt::Display for SymTableError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymTableError::NameNotFound(n) => {
                write!(f, "the name '{}' could not be found", n.render())
            }
            SymTableError::ExpectedVarFoundFunc(n) => {
                write!(
                    f,
                    "expected '{}' to be a variable, found function",
                    n.render()
                )
            }
            SymTableError::ExpectedFuncFoundVar(n) => {
                write!(
                    f,
                    "expected '{}' to be a function, found variable",
                    n.render()
                )
            }
            SymTableError::DuplicateSymbol(n) => write!(f, "duplicate symbol '{}'", n.render()),
            SymTableError::DuplicateFunctionArgName(n) => {
                write!(f, "duplicate function argument name '{}'", n.render())
            }
        }
    }
}

impl std::fmt::Display for EnvironmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EnvironmentError::SymTableError(s) => write!(f, "{}", s),
            EnvironmentError::MissingPronounReferent => {
                f.write_str("pronoun doesn't refer to anything yet")
            }
            EnvironmentError::IOError(s) => f.write_str(s),
        }
    }
}

impl std::fmt::Display for ValError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValError::NotIndexable => f.write_str("value not indexable"),
            ValError::InvalidKey => f.write_str("invalid key"),
            ValError::NoValueForKey => f.write_str("no value for key"),
            ValError::IndexOutOfBounds => f.write_str("index out of bounds"),
            ValError::IndexNotAssignable => f.write_str("index not assignable"),
            ValError::InvalidOperationForType => f.write_str("invalid operation for type"),
            ValError::InvalidComparison => f.write_str("invalid comparison"),
            ValError::InvalidSplitDelimiter => f.write_str("invalid split delimiter"),
            ValError::InvalidJoinDelimiter => f.write_str("invalid join delimiter"),
            ValError::InvalidArrayElementForJoin(_) => {
                f.write_str("invalid array element for join")
            }
            ValError::ParsingStringAsNumberFailed(_) => {
                f.write_str("parsing string as number failed")
            }
            ValError::InvalidStringToIntegerRadix(_) => {
                f.write_str("invalid string-to-integer radix")
            }
            ValError::ConvertingNumberToCharacterFailed(_) => {
                f.write_str("converting number to character failed")
            }
            ValError::UnexpectedParameterToNumberToCharacterCast(_) => {
                f.write_str("converting number to character shouldn't take a parameter")
            }
        }
    }
}

impl std::fmt::Display for WriteValError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WriteValError::ValueNotWritable => f.write_str("value not writable"),
        }
    }
}

impl std::fmt::Display for ExecError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecError::NonCompoundAssignmentExpressionListInvalid => {
                f.write_str("expression list is invalid when not doing a compound assignment")
            }
        }
    }
}

impl std::fmt::Display for ProduceValError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProduceValError::WrongNumberOfFunctionArguments { expected, actual } => write!(
                f,
                "wrong number of function arguments; expected {}, got {}",
                expected, actual
            ),
        }
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::EnvironmentError(e) => write!(f, "{}", e),
            RuntimeError::ValError(v) => write!(f, "{}", v),
            RuntimeError::WriteValError(w) => write!(f, "{}", w),
            RuntimeError::ExecError(e) => write!(f, "{}", e),
            RuntimeError::ProduceValError(p) => write!(f, "{}", p),
        }
    }
}
