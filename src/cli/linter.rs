use std::iter::once;

use colored::{ColoredString, Colorize};

use crate::{
    cli::{self},
    linter::{standard_linter, Diag, LinterResult},
};

use super::cli_output::CLIOutput;

pub fn lint(source_code: &str) -> Result<LinterResult, cli::error::Error> {
    cli::parser::parse(source_code).map(|program| standard_linter().run(&program))
}

fn colorized(diag: Diag) -> Vec<ColoredString> {
    once("Lint issue: ".yellow().bold())
        .chain(once(format!("(line {}) ", diag.line).normal()))
        .chain(once(diag.issue.bold()))
        .chain(
            diag.suggestions
                .iter()
                .map(|s| once("\n\t".normal()).chain(once(s.normal())))
                .flatten(),
        )
        .collect()
}

fn build_output(result: LinterResult) -> CLIOutput {
    if result.diags.is_empty() {
        CLIOutput::one("No lint issues found :)".green())
    } else {
        CLIOutput::new(result.diags.into_iter().map(colorized).flatten().collect())
    }
}

pub fn run(source_code: &str) -> Result<CLIOutput, cli::error::Error> {
    lint(source_code).map(build_output)
}
