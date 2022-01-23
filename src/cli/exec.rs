use crate::cli;
use crate::exec;

use super::cli_output::CLIOutput;

pub fn run_using<In, Out>(
    input: In,
    output: Out,
    source_code: &str,
) -> Result<CLIOutput, cli::Error>
where
    In: std::io::Read,
    Out: std::io::Write,
{
    cli::parser::parse(source_code).and_then(|program| {
        exec::exec_using(input, output, &program)
            .map(|_| CLIOutput::empty())
            .map_err(Into::into)
    })
}

pub fn run(source_code: &str) -> Result<CLIOutput, cli::Error> {
    cli::parser::parse(source_code).and_then(|program| {
        exec::exec(&program)
            .map(|_| CLIOutput::empty())
            .map_err(Into::into)
    })
}
