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
            ValError::NotIndexable(v) => write!(f, "value {} not indexable", v),
            ValError::InvalidKey(v) => write!(f, "invalid key {}", v),
            ValError::NoValueForKey(k, v) => write!(f, "no value for key {} in {}", k, v),
            ValError::IndexOutOfBounds(k, v) => write!(f, "index {} out of bounds for {}", k, v),
            ValError::IndexNotAssignable(k, v) => write!(f, "index {} not assignable for {}", k, v),
            ValError::InvalidOperationForType(op, v) => write!(f, "cannot {} value {}", op, v),
            ValError::InvalidBinaryOperationForType(op, a, b) => {
                write!(f, "cannot {} values {} and {}", op, a, b)
            }
            ValError::InvalidComparison(a, b) => {
                write!(f, "invalid comparison between {} and {}", a, b)
            }
            ValError::InvalidSplitDelimiter(v) => write!(f, "invalid split delimiter {}", v),
            ValError::InvalidJoinDelimiter(v) => write!(f, "invalid join delimiter {}", v),
            ValError::InvalidArrayElementForJoin(v) => {
                write!(f, "invalid array element for join {}", v)
            }
            ValError::ParsingStringAsNumberFailed(v) => {
                write!(f, "parsing {} as number failed", v)
            }
            ValError::InvalidStringToIntegerRadix(v) => {
                write!(f, "invalid string-to-integer radix {}", v)
            }
            ValError::ConvertingNumberToCharacterFailed(v) => {
                write!(f, "converting {} to character failed", v)
            }
            ValError::UnexpectedParameterToNumberToCharacterCast(v) => {
                write!(
                    f,
                    "converting number to character shouldn't take a parameter, found {}",
                    v
                )
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
