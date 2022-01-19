pub mod cli_output;
pub mod error;
pub mod exec;
pub mod linter;
pub mod parser;

use std::{ffi::OsString, fs::read_to_string};

use self::{cli_output::CLIOutput, error::Error};

// no tests because this API is just a rough draft

pub enum Command {
    Parse,
    Lint,
    Exec,
}

fn dump_output(output: Result<CLIOutput, Error>) {
    match output {
        Ok(o) => println!("{}", o),
        Err(e) => eprintln!("{}", e),
    }
}

pub fn run_from_command_line(command: Command, source_code: &str) {
    dump_output(match command {
        Command::Parse => parser::run(source_code),
        Command::Lint => linter::run(source_code),
        Command::Exec => exec::run(source_code),
    })
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
            clap::App::new("lint").arg(clap::Arg::new("file").required(true).takes_value(false)),
            clap::App::new("parse").arg(clap::Arg::new("file").required(true).takes_value(false)),
            clap::App::new("exec").arg(clap::Arg::new("file").required(true).takes_value(false)),
        ])
        .try_get_matches_from(args)?;
    match matches.subcommand() {
        Some(("lint", matches)) => {
            load_and_run_from_command_line(Command::Lint, matches.value_of("file").unwrap())?
        }
        Some(("parse", matches)) => {
            load_and_run_from_command_line(Command::Parse, matches.value_of("file").unwrap())?
        }
        Some(("exec", matches)) => {
            load_and_run_from_command_line(Command::Exec, matches.value_of("file").unwrap())?
        }
        _ => {}
    }
    Ok(())
}
