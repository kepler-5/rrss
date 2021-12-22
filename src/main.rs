use std::{env, process};

pub mod analysis;
pub mod driver;
pub mod frontend;
pub mod linter;

#[cfg(test)]
#[macro_use]
extern crate inner;
#[macro_use]
extern crate lazy_static;

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
