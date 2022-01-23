use std::fmt::{self};

use super::*;

// no tests because the format is not (even close to) final yet

fn write_list<T: fmt::Display>(
    f: &mut fmt::Formatter<'_>,
    list: &[T],
    sep: &'static str,
) -> fmt::Result {
    let mut first = true;
    for elem in list {
        if !first {
            f.write_str(sep)?;
        }
        first = false;
        write!(f, "{}", elem)?;
    }
    Ok(())
}

impl fmt::Display for Diag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Linter issue: {}", self.issue)?;
        for elem in self.suggestions.iter() {
            f.write_str("\n\t")?;
            f.write_str(elem)?;
        }
        Ok(())
    }
}

impl fmt::Display for LinterResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_list(f, &self.diags, "\n")
    }
}
