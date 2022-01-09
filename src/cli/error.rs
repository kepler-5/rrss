use colored::Colorize;
use derive_more::Constructor;
use std::fmt;

use crate::frontend::parser::ParseError;

use super::cli_output::CLIOutput;

#[derive(Constructor, Debug)]
pub struct Error {
    payload: CLIOutput,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.payload.fmt(f)
    }
}

impl std::error::Error for Error {}

impl From<ParseError<'_>> for Error {
    fn from(p: ParseError) -> Self {
        Error::new(CLIOutput::new(vec![
            "Error: ".red().bold(),
            p.to_string().normal(),
        ]))
    }
}
