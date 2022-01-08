use std::{ffi::OsString, fmt::Display, fs::read_to_string};

use derive_more::Constructor;

use crate::{
    frontend::{
        ast::Program,
        parser::{parse, ParseError},
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

impl std::error::Error for Error {}

impl From<ParseError<'_>> for Error {
    fn from(p: ParseError) -> Self {
        Error::new(p.to_string())
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
            Err(e) => eprintln!("{}", e),
        }
    };
}

pub fn run_from_command_line(command: Command, source_code: &str) {
    match command {
        Command::Parse => dump!(run_parser(source_code).map(prettify)),
        Command::Lint => dump!(run_linter(source_code)),
    }
}

pub fn load_and_run_from_command_line(
    command: Command,
    file_path: &str,
) -> Result<(), std::io::Error> {
    read_to_string(file_path).map(|source_code| run_from_command_line(command, &source_code))
}

pub fn cli<I, T>(args: I) -> Result<(), Box<dyn std::error::Error>>
where
    I: IntoIterator<Item = T>,
    T: Into<OsString> + Clone,
{
    let matches = clap::App::new("rrss")
        .about("Rockstar programming language tools")
        .subcommands([
            clap::SubCommand::with_name("lint").arg(
                clap::Arg::with_name("file")
                    .required(true)
                    .takes_value(false),
            ),
            clap::SubCommand::with_name("parse").arg(
                clap::Arg::with_name("file")
                    .required(true)
                    .takes_value(false),
            ),
        ])
        .get_matches_from_safe(args)?;
    match matches.subcommand() {
        ("lint", Some(matches)) => {
            load_and_run_from_command_line(Command::Lint, matches.value_of("file").unwrap())?
        }
        ("parse", Some(matches)) => {
            load_and_run_from_command_line(Command::Parse, matches.value_of("file").unwrap())?
        }
        _ => {}
    }
    Ok(())
}
