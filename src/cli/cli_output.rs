use colored::{ColoredString, Colorize};
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct CLIOutput {
    lines: Vec<ColoredString>,
}

impl CLIOutput {
    pub fn one(s: ColoredString) -> Self {
        Self::new(vec![s])
    }
    pub fn one_str(s: &str) -> Self {
        Self::one(s.normal())
    }
    pub fn new(lines: Vec<ColoredString>) -> Self {
        Self { lines }
    }
}

impl fmt::Display for CLIOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        for line in &self.lines {
            write!(f, "{}", line)?
        }
        Ok(())
    }
}
