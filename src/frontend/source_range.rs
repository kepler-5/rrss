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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SourceRange {
    start: SourceLocation,
    end: SourceLocation,
}

pub trait Range {
    fn range(&self) -> SourceRange;
}

impl From<((u32, u32), (u32, u32))> for SourceRange {
    fn from(x: ((u32, u32), (u32, u32))) -> Self {
        SourceRange::from((SourceLocation::from(x.0), SourceLocation::from(x.1)))
    }
}

impl From<(SourceLocation, SourceLocation)> for SourceRange {
    fn from((start, end): (SourceLocation, SourceLocation)) -> Self {
        SourceRange::new(start, end)
    }
}

impl SourceRange {
    fn new(start: SourceLocation, end: SourceLocation) -> Self {
        SourceRange { start, end }.normalized()
    }

    pub fn concat(self, other: Self) -> Self {
        let (low, high) = minmax(self, other);
        Self {
            start: low.start,
            end: high.end,
        }
    }

    pub fn to(self, other: SourceLocation) -> Self {
        self.concat(Self::new(other, other))
    }

    fn normalized(self) -> Self {
        if self.end < self.start {
            Self::new(self.end, self.start)
        } else {
            self
        }
    }
}

impl SourceLocation {
    pub fn to(self, other: Self) -> SourceRange {
        SourceRange::new(self, other).normalized()
    }
}
