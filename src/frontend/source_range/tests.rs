use super::*;

#[test]
fn normalized_from_locs() {
    assert_eq!(
        SourceRange::from((SourceLocation::new(1, 3), SourceLocation::new(2, 4))),
        SourceRange {
            start: SourceLocation::new(1, 3),
            end: SourceLocation::new(2, 4)
        }
    );
    assert_eq!(
        SourceRange::from((SourceLocation::new(2, 4), SourceLocation::new(1, 3))),
        SourceRange {
            start: SourceLocation::new(1, 3),
            end: SourceLocation::new(2, 4)
        }
    );
    assert_eq!(
        SourceRange::from((SourceLocation::new(1, 3), SourceLocation::new(1, 2))),
        SourceRange {
            start: SourceLocation::new(1, 2),
            end: SourceLocation::new(1, 3)
        }
    );
    assert_eq!(
        SourceRange::from((SourceLocation::new(1, 1), SourceLocation::new(1, 1))),
        SourceRange {
            start: SourceLocation::new(1, 1),
            end: SourceLocation::new(1, 1)
        }
    );
}

#[test]
fn normalized_from_tuple() {
    assert_eq!(
        SourceRange::from(((1, 3), (2, 4))),
        SourceRange {
            start: SourceLocation::new(1, 3),
            end: SourceLocation::new(2, 4)
        }
    );
    assert_eq!(
        SourceRange::from(((2, 4), (1, 3))),
        SourceRange {
            start: SourceLocation::new(1, 3),
            end: SourceLocation::new(2, 4)
        }
    );
    assert_eq!(
        SourceRange::from(((1, 3), (1, 2))),
        SourceRange {
            start: SourceLocation::new(1, 2),
            end: SourceLocation::new(1, 3)
        }
    );
    assert_eq!(
        SourceRange::from(((1, 1), (1, 1))),
        SourceRange {
            start: SourceLocation::new(1, 1),
            end: SourceLocation::new(1, 1)
        }
    );
}

#[test]
fn concat() {
    assert_eq!(
        SourceRange::from(((1, 2), (1, 5))).concat(SourceRange::from(((1, 7), (1, 9)))),
        SourceRange::from(((1, 2), (1, 9)))
    );
    assert_eq!(
        SourceRange::from(((1, 7), (1, 9))).concat(SourceRange::from(((1, 2), (1, 5)))),
        SourceRange::from(((1, 2), (1, 9)))
    );
    assert_eq!(
        SourceRange::from(((27, 9), (1, 7))).concat(SourceRange::from(((1, 2), (26, 576)))),
        SourceRange::from(((1, 2), (27, 9)))
    );
}

#[test]
fn source_range_to() {
    assert_eq!(
        SourceRange::from(((1, 2), (1, 5))).to(SourceLocation::new(2, 10)),
        SourceRange::from(((1, 2), (2, 10)))
    );
    assert_eq!(
        SourceRange::from(((10, 2), (10, 5))).to(SourceLocation::new(1, 0)),
        SourceRange::from(((1, 0), (10, 5)))
    );

    assert_eq!(
        SourceLocation::new(1, 0)
            .to(SourceLocation::new(10, 10))
            .to(SourceLocation::new(20, 20)),
        SourceRange::from(((1, 0), (20, 20)))
    );
    assert_eq!(
        SourceLocation::new(10, 10)
            .to(SourceLocation::new(100, 100))
            .to(SourceLocation::new(0, 0)),
        SourceRange::from(((0, 0), (100, 100)))
    );
}

#[test]
fn source_location_to() {
    assert_eq!(
        SourceLocation::new(1, 0).to(SourceLocation::new(10, 10)),
        SourceRange::from(((1, 0), (10, 10)))
    );
    assert_eq!(
        SourceLocation::new(10, 10).to(SourceLocation::new(1, 0)),
        SourceRange::from(((1, 0), (10, 10)))
    );
}

// there could be BUGS in the getters, okay?
#[test]
fn accessors() {
    assert_eq!(
        SourceLocation::new(1, 0)
            .to(SourceLocation::new(10, 10))
            .start(),
        SourceLocation::new(1, 0)
    );
    assert_eq!(
        SourceLocation::new(1, 0)
            .to(SourceLocation::new(10, 10))
            .end(),
        SourceLocation::new(10, 10)
    );
    assert_eq!(
        SourceLocation::new(10, 10)
            .to(SourceLocation::new(20, 15))
            .start(),
        SourceLocation::new(10, 10)
    );
    assert_eq!(
        SourceLocation::new(10, 10)
            .to(SourceLocation::new(20, 15))
            .end(),
        SourceLocation::new(20, 15)
    );
}
