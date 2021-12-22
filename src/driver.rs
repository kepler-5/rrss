use std::fmt::Display;

use derive_more::Constructor;

use crate::{
    frontend::{
        ast::Program,
        parser::{parse, ParseError, ParseErrorWithLine},
    },
    linter::{standard_linter, LinterResult},
};

// no tests because this API is just a rough draft

#[derive(Constructor, Debug)]
pub struct Error {
    payload: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.payload.fmt(f)
    }
}

impl From<ParseError<'_>> for Error {
    fn from(p: ParseError) -> Self {
        Error::new(p.to_string())
    }
}

impl From<ParseErrorWithLine<'_>> for Error {
    fn from(p: ParseErrorWithLine<'_>) -> Self {
        p.0.into()
    }
}

fn prettify(program: Program) -> String {
    format!("{:#?}", program)
}

pub fn run_parser(source_code: &str) -> Result<Program, Error> {
    parse(source_code).map_err(Into::into)
}

pub fn run_linter(source_code: &str) -> Result<LinterResult, Error> {
    run_parser(source_code).map(|program| standard_linter().run(&program))
}

pub enum Command {
    Parse,
    Lint,
}

macro_rules! dump {
    ($result:expr) => {
        match $result {
            Ok(r) => println!("{}", r),
            Err(e) => println!("{}", e),
        }
    };
}

pub fn run_from_command_line(command: Command, source_code: &str) {
    match command {
        Command::Parse => dump!(run_parser(source_code).map(prettify)),
        Command::Lint => dump!(run_linter(source_code)),
    }
}
