use itertools::Itertools;

use super::{Array, DictKey, Val};
use std::fmt::{Display, Write};

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Array(a) => a.fmt(f),
            Val::String(s) => {
                f.write_char('"')?;
                f.write_str(&s)?;
                f.write_char('"')
            }
            _ => f.write_str(&self.to_string_for_output()),
        }
    }
}

impl Display for DictKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            DictKey::Undefined => f.write_str("mysterious"),
            DictKey::Null => f.write_str("null"),
            DictKey::Boolean(b) => f.write_str(if *b { "true" } else { "false" }),
            DictKey::String(s) => {
                f.write_char('"')?;
                f.write_str(&s)?;
                f.write_char('"')
            }
        }
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.arr
                .iter()
                .map(|val| val.to_string())
                .chain(
                    self.dict
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, v))
                        .sorted_unstable()
                )
                .join(", ")
        )
    }
}
