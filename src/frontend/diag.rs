use derive_more::Constructor;

use crate::frontend::parser::{ParseError, ParseErrorCode};

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

pub trait IntoDiag {
    fn into_diag(&self) -> Diag;
}
