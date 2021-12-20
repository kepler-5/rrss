use super::*;

#[test]
fn diags_builder_combine() {
    let bogus = |text| Diag::new(String::from(text), Vec::new());
    assert_eq!(
        DiagsBuilder::Empty.combine(DiagsBuilder::Empty),
        DiagsBuilder::Empty
    );
    assert_eq!(
        DiagsBuilder::One(bogus("a")).combine(DiagsBuilder::Empty),
        DiagsBuilder::One(bogus("a"))
    );
    assert_eq!(
        DiagsBuilder::Empty.combine(DiagsBuilder::One(bogus("a"))),
        DiagsBuilder::One(bogus("a"))
    );
    assert_eq!(
        DiagsBuilder::One(bogus("a")).combine(DiagsBuilder::One(bogus("b"))),
        DiagsBuilder::List(vec![bogus("a"), bogus("b")])
    );
    assert_eq!(
        DiagsBuilder::List(vec![bogus("a"), bogus("b")]).combine(DiagsBuilder::Empty),
        DiagsBuilder::List(vec![bogus("a"), bogus("b")])
    );
    assert_eq!(
        DiagsBuilder::Empty.combine(DiagsBuilder::List(vec![bogus("a"), bogus("b")])),
        DiagsBuilder::List(vec![bogus("a"), bogus("b")])
    );
    assert_eq!(
        DiagsBuilder::List(vec![bogus("a"), bogus("b")])
            .combine(DiagsBuilder::List(vec![bogus("c"),])),
        DiagsBuilder::List(vec![bogus("a"), bogus("b"), bogus("c")])
    );
}
