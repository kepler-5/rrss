use std::fmt::Display;

use derive_more::Constructor;

use crate::frontend::parser::ParseError;

#[cfg(test)]
mod tests;

#[derive(Constructor, Clone, Debug, PartialEq, Eq)]
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

impl IntoDiag for ParseError<'_> {
    fn into_diag(&self) -> Diag {
        Diag::new(self.to_string())
    }
}
