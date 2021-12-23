use derive_more::{Constructor, From};

#[cfg(test)]
mod tests;

#[derive(Constructor, Copy, Clone, Debug, From, PartialEq, Eq, PartialOrd, Ord)]
pub struct SourceLocation {
    pub line: u32,
    pub column: u32,
}

fn minmax<T: Ord>(a: T, b: T) -> (T, T) {
    if a <= b {
        (a, b)
    } else {
        (b, a)
    }
}

#[derive(Constructor, Clone, Debug, From, PartialEq, Eq, PartialOrd, Ord)]
pub struct SourceRange {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl From<((u32, u32), (u32, u32))> for SourceRange {
    fn from(x: ((u32, u32), (u32, u32))) -> Self {
        SourceRange::from((SourceLocation::from(x.0), SourceLocation::from(x.1)))
    }
}

impl SourceRange {
    pub fn normalized(self) -> Self {
        if self.end < self.start {
            Self::new(self.end, self.start)
        } else {
            self
        }
    }
    pub fn concat(self, other: Self) -> Self {
        let (low, high) = minmax(self.normalized(), other.normalized());
        Self {
            start: low.start,
            end: high.end,
        }
    }
}
