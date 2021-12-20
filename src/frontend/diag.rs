use std::fmt::Display;

use derive_more::Constructor;

use crate::frontend::parser::ParseErrorWithLine;

#[cfg(test)]
mod tests;

#[derive(Constructor, Clone, PartialEq, Eq)]
pub struct Diag {
    text: String,
}

impl Diag {
    pub fn text(&self) -> &str {
        &self.text
    }
}

impl Display for Diag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.text())
    }
}

pub trait IntoDiag {
    fn into_diag(&self) -> Diag;
}

impl IntoDiag for ParseErrorWithLine<'_> {
    fn into_diag(&self) -> Diag {
        let ParseErrorWithLine(error, line) = self;
        Diag::new(format!("Parse error (line {}): {}", line, error))
    }
}
