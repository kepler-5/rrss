use crate::{
    cli::{self, cli_output::CLIOutput},
    frontend::{self, ast::Program},
};

pub fn parse(source_code: &str) -> Result<Program, cli::error::Error> {
    frontend::parser::parse(source_code).map_err(Into::into)
}

pub fn run(source_code: &str) -> Result<CLIOutput, cli::error::Error> {
    parse(source_code).map(prettify)
}

fn prettify(program: Program) -> CLIOutput {
    CLIOutput::one_str(&format!("{:#?}", program))
}
