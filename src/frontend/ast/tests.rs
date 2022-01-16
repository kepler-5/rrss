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
fn expression_list_iter() {
    let bogus = || SourceRange::from(((0, 0), (0, 0)));
    assert_eq!(
        ExpressionList {
            first: WithRange(LiteralExpression::Null, bogus()).into(),
            rest: vec![]
        }
        .len(),
        1
    );
    assert_eq!(
        ExpressionList {
            first: WithRange(LiteralExpression::Null, bogus()).into(),
            rest: vec![]
        }
        .iter()
        .cloned()
        .collect::<Vec<_>>(),
        [WithRange(LiteralExpression::Null, bogus()).into()]
    );
    assert_eq!(
        ExpressionList {
            first: WithRange(LiteralExpression::Null, bogus()).into(),
            rest: vec![
                WithRange(LiteralExpression::Number(0.0), bogus()).into(),
                WithRange(LiteralExpression::Number(1.0), bogus()).into()
            ]
        }
        .iter()
        .cloned()
        .collect::<Vec<_>>(),
        [
            WithRange(LiteralExpression::Null, bogus()).into(),
            WithRange(LiteralExpression::Number(0.0), bogus()).into(),
            WithRange(LiteralExpression::Number(1.0), bogus()).into()
        ]
    );
    assert_eq!(
        ExpressionList {
            first: WithRange(LiteralExpression::Null, bogus()).into(),
            rest: vec![
                WithRange(LiteralExpression::Number(0.0), bogus()).into(),
                WithRange(LiteralExpression::Number(1.0), bogus()).into()
            ]
        }
        .len(),
        3
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

fn line_range(start: u32, end: u32) -> SourceRange {
    ((1, start), (1, end)).into()
}

#[test]
fn range() {
    // interdependences ftw
    let expr_range = |text| {
        crate::frontend::parser::Parser::for_source_code(text)
            .parse_expression()
            .unwrap()
            .range()
    };

    assert_eq!(expr_range("1 + 1 * 1, 2, 3"), line_range(0, 15));
    assert_eq!(expr_range("1 + 1 * 1, 2, 3        "), line_range(0, 15));
    assert_eq!(expr_range("        1 + 1 * 1, 2, 3"), line_range(8, 23));
    assert_eq!(
        expr_range("        1 + 1 * 1, 2, 3        "),
        line_range(8, 23)
    );
    assert_eq!(expr_range("the arrray at 3"), line_range(0, 15));
    assert_eq!(expr_range("the arrray at 3        "), line_range(0, 15));
    assert_eq!(expr_range("        the arrray at 3"), line_range(8, 23));
    assert_eq!(
        expr_range("        the arrray at 3        "),
        line_range(8, 23)
    );
    assert_eq!(expr_range("a func taking 3"), line_range(0, 15));
    assert_eq!(expr_range("a func taking 3        "), line_range(0, 15));
    assert_eq!(expr_range("        a func taking 3"), line_range(8, 23));
    assert_eq!(
        expr_range("        a func taking 3        "),
        line_range(8, 23)
    );
}
