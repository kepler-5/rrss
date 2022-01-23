pub mod analysis;
pub mod cli;
pub mod exec;
pub mod frontend;
pub mod linter;

pub fn run() -> i32 {
    let exit_code = match cli::cli(std::env::args()) {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{}", e);
            1
        }
    };
    exit_code
}
