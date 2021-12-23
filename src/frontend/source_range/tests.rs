use super::*;

#[test]
fn normalized() {
    assert_eq!(
        SourceRange::from(((1, 3), (2, 4))).normalized(),
        SourceRange::from(((1, 3), (2, 4)))
    );
    assert_eq!(
        SourceRange::from(((2, 4), (1, 3))).normalized(),
        SourceRange::from(((1, 3), (2, 4)))
    );
    assert_eq!(
        SourceRange::from(((1, 3), (1, 2))).normalized(),
        SourceRange::from(((1, 2), (1, 3)))
    );
    assert_eq!(
        SourceRange::from(((1, 1), (1, 1))).normalized(),
        SourceRange::from(((1, 1), (1, 1)))
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
