use super::*;

#[test]
fn poetic_number_literal_iterator() {
    fn items<'a>(
        elems: &'a Vec<PoeticNumberLiteralElem>,
    ) -> Vec<PoeticNumberLiteralIteratorItem<'a>> {
        PoeticNumberLiteralIterator::new(elems.iter()).collect()
    }

    assert_eq!(
        items(&vec![PoeticNumberLiteralElem::Dot]),
        vec![PoeticNumberLiteralIteratorItem::Dot]
    );
    assert_eq!(
        items(&vec![PoeticNumberLiteralElem::Word("foo".into())]),
        vec![PoeticNumberLiteralIteratorItem::Word(&"foo".into())]
    );
    assert_eq!(
        items(&vec![
            PoeticNumberLiteralElem::Word("foo".into()),
            PoeticNumberLiteralElem::WordSuffix("'s".into())
        ]),
        vec![PoeticNumberLiteralIteratorItem::SuffixedWord(
            &"foo".into(),
            vec![&"'s".into()],
        )]
    );
}

#[test]
fn test_find_or_end() {
    assert_eq!(position_or_end(vec![].into_iter(), |x: &i32| *x == 1), 0);
    assert_eq!(position_or_end(vec![1, 2, 3].into_iter(), |x| *x == 1), 0);
    assert_eq!(position_or_end(vec![1, 2, 3].into_iter(), |x| *x == 2), 1);
    assert_eq!(position_or_end(vec![1, 2, 3].into_iter(), |x| *x == 3), 2);
    assert_eq!(position_or_end(vec![1, 2, 3].into_iter(), |x| *x == 4), 3);
    assert_eq!(position_or_end(vec![1, 2, 3].into_iter(), |x| *x == 9), 3);
}
