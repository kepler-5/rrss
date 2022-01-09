use std::{env, process};

pub mod analysis;
pub mod cli;
pub mod exec;
pub mod frontend;
pub mod linter;

fn main() {
    let exit_code = match cli::cli(env::args()) {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{}", e);
            1
        }
    };
    process::exit(exit_code)
}
