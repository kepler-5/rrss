use super::*;

#[test]
fn list_builder_combine() {
    assert_eq!(
        ListBuilder::<()>::Empty.combine(ListBuilder::Empty),
        ListBuilder::Empty
    );
    assert_eq!(
        ListBuilder::One("a").combine(ListBuilder::Empty),
        ListBuilder::One("a")
    );
    assert_eq!(
        ListBuilder::Empty.combine(ListBuilder::One("a")),
        ListBuilder::One("a")
    );
    assert_eq!(
        ListBuilder::One("a").combine(ListBuilder::One("b")),
        ListBuilder::List(vec!["a", "b"])
    );
    assert_eq!(
        ListBuilder::List(vec!["a", "b"]).combine(ListBuilder::Empty),
        ListBuilder::List(vec!["a", "b"])
    );
    assert_eq!(
        ListBuilder::Empty.combine(ListBuilder::List(vec!["a", "b"])),
        ListBuilder::List(vec!["a", "b"])
    );
    assert_eq!(
        ListBuilder::List(vec!["a", "b"]).combine(ListBuilder::List(vec!["c",])),
        ListBuilder::List(vec!["a", "b", "c"])
    );
}
