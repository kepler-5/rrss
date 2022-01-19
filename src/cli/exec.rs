use crate::cli;
use crate::exec;

use super::cli_output::CLIOutput;

pub fn run(source_code: &str) -> Result<CLIOutput, cli::error::Error> {
    cli::parser::parse(source_code).and_then(|program| {
        exec::exec(&program)
            .map(|_| CLIOutput::empty())
            .map_err(Into::into)
    })
}
