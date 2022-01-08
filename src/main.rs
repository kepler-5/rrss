use std::{env, process};

pub mod analysis;
pub mod cli;
pub mod frontend;
pub mod linter;

use crate::cli::driver;

fn main() {
    let exit_code = match driver::cli(env::args()) {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{}", e);
            1
        }
    };
    process::exit(exit_code)
}
