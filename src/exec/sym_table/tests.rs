use super::*;

#[test]
fn to_lowercase() {
    assert_eq!(
        SimpleIdentifier("foo".into()).to_lowercase(),
        Lowercased::Ref(&SimpleIdentifier("foo".into()))
    );
    assert_eq!(
        SimpleIdentifier("fOo".into()).to_lowercase(),
        Lowercased::New(SimpleIdentifier("foo".into()))
    );
    assert_eq!(
        CommonIdentifier("foo".into(), "bar".into()).to_lowercase(),
        Lowercased::Ref(&CommonIdentifier("foo".into(), "bar".into()))
    );
    assert_eq!(
        CommonIdentifier("fOo".into(), "bar".into()).to_lowercase(),
        Lowercased::New(CommonIdentifier("foo".into(), "bar".into()))
    );
    assert_eq!(
        CommonIdentifier("foo".into(), "bAr".into()).to_lowercase(),
        Lowercased::New(CommonIdentifier("foo".into(), "bar".into()))
    );
    assert_eq!(
        ProperIdentifier(vec!["foo".into(), "bar".into(), "baz".into()]).to_lowercase(),
        Lowercased::Ref(&ProperIdentifier(vec![
            "foo".into(),
            "bar".into(),
            "baz".into()
        ]))
    );
    assert_eq!(
        ProperIdentifier(vec!["Foo".into(), "bar".into(), "baz".into()]).to_lowercase(),
        Lowercased::New(ProperIdentifier(vec![
            "foo".into(),
            "bar".into(),
            "baz".into()
        ]))
    );
    assert_eq!(
        ProperIdentifier(vec!["foo".into(), "Bar".into(), "baz".into()]).to_lowercase(),
        Lowercased::New(ProperIdentifier(vec![
            "foo".into(),
            "bar".into(),
            "baz".into()
        ]))
    );
    assert_eq!(
        ProperIdentifier(vec!["foo".into(), "bar".into(), "Baz".into()]).to_lowercase(),
        Lowercased::New(ProperIdentifier(vec![
            "foo".into(),
            "bar".into(),
            "baz".into()
        ]))
    );
}

#[test]
fn emplace_and_lookup() {
    let mut table = SymTable::new();
    table.emplace(&SimpleIdentifier("FOO".into()).into());
    assert_eq!(
        table.lookup(&SimpleIdentifier("foO".into()).into()),
        Ok(&Val::Undefined)
    );
    assert_eq!(
        table.lookup(&SimpleIdentifier("bAr".into()).into()),
        Err(SymTableError::NameNotFound(
            SimpleIdentifier("bAr".into()).into()
        ))
    );

    *table.emplace(&CommonIdentifier("baR".into(), "bAz".into()).into()) = Val::Null;
    assert_eq!(
        table.lookup(&SimpleIdentifier("foO".into()).into()),
        Ok(&Val::Undefined)
    );
    assert_eq!(
        table.lookup(&CommonIdentifier("Bar".into(), "BAZ".into()).into()),
        Ok(&Val::Null)
    );
    assert_eq!(
        table.lookup(&SimpleIdentifier("bAr".into()).into()),
        Err(SymTableError::NameNotFound(
            SimpleIdentifier("bAr".into()).into()
        ))
    );
    *table.emplace(&ProperIdentifier(vec!["baR".into(), "bAz".into()]).into()) =
        Val::Boolean(false);
    assert_eq!(
        table.lookup(&SimpleIdentifier("foO".into()).into()),
        Ok(&Val::Undefined)
    );
    assert_eq!(
        table.lookup(&CommonIdentifier("Bar".into(), "BAZ".into()).into()),
        Ok(&Val::Null)
    );
    assert_eq!(
        table.lookup(&ProperIdentifier(vec!["Bar".into(), "BAZ".into()]).into()),
        Ok(&Val::Boolean(false))
    );
    assert_eq!(
        table.lookup(&SimpleIdentifier("bAr".into()).into()),
        Err(SymTableError::NameNotFound(
            SimpleIdentifier("bAr".into()).into()
        ))
    );
}
