use crate::frontend::ast::{Block, Continue, WithRange};

use super::*;

fn some_func_data() -> FunctionData {
    FunctionData {
        params: vec![],
        body: Block::Empty((0, 0).into()),
    }
}
fn other_func_data() -> FunctionData {
    let bogus_range = || ((0, 0), (0, 0)).into();
    FunctionData {
        params: ["foo", "bar"]
            .into_iter()
            .map(|s| WithRange(SimpleIdentifier(s.into()).into(), bogus_range()))
            .collect(),
        body: Block::NonEmpty(vec![Continue(bogus_range()).into()]),
    }
}

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
fn emplace_and_lookup_var() {
    let mut table = SymTable::new();
    table.emplace_var(&SimpleIdentifier("FOO".into()).into());
    assert_eq!(
        table.lookup_var(&SimpleIdentifier("foO".into()).into()),
        Ok(&Val::Undefined)
    );
    assert_eq!(
        table.lookup_var(&SimpleIdentifier("bAr".into()).into()),
        Err(SymTableError::NameNotFound(
            SimpleIdentifier("bAr".into()).into()
        ))
    );

    *table.emplace_var(&CommonIdentifier("baR".into(), "bAz".into()).into()) = Val::Null;
    assert_eq!(
        table.lookup_var(&SimpleIdentifier("foO".into()).into()),
        Ok(&Val::Undefined)
    );
    assert_eq!(
        table.lookup_var(&CommonIdentifier("Bar".into(), "BAZ".into()).into()),
        Ok(&Val::Null)
    );
    assert_eq!(
        table.lookup_var(&SimpleIdentifier("bAr".into()).into()),
        Err(SymTableError::NameNotFound(
            SimpleIdentifier("bAr".into()).into()
        ))
    );
    *table.emplace_var(&ProperIdentifier(vec!["baR".into(), "bAz".into()]).into()) =
        Val::Boolean(false);
    assert_eq!(
        table.lookup_var(&SimpleIdentifier("foO".into()).into()),
        Ok(&Val::Undefined)
    );
    assert_eq!(
        table.lookup_var(&CommonIdentifier("Bar".into(), "BAZ".into()).into()),
        Ok(&Val::Null)
    );
    assert_eq!(
        table.lookup_var(&ProperIdentifier(vec!["Bar".into(), "BAZ".into()]).into()),
        Ok(&Val::Boolean(false))
    );
    assert_eq!(
        table.lookup_var(&SimpleIdentifier("bAr".into()).into()),
        Err(SymTableError::NameNotFound(
            SimpleIdentifier("bAr".into()).into()
        ))
    );

    table.emplace_func(
        &SimpleIdentifier("func".into()).into(),
        Arc::new(some_func_data()),
    );
    assert_eq!(
        table.lookup_var(&SimpleIdentifier("func".into()).into()),
        Err(SymTableError::ExpectedVarFoundFunc(
            SimpleIdentifier("func".into()).into()
        ))
    );
}

#[test]
fn emplace_and_lookup_func() {
    let mut table = SymTable::new();
    table.emplace_func(
        &SimpleIdentifier("func".into()).into(),
        Arc::new(some_func_data()),
    );
    assert_eq!(
        table.lookup_func(&SimpleIdentifier("func".into()).into()),
        Ok(&some_func_data())
    );
    table.emplace_func(
        &SimpleIdentifier("func2".into()).into(),
        Arc::new(other_func_data()),
    );
    assert_eq!(
        table.lookup_func(&SimpleIdentifier("func2".into()).into()),
        Ok(&other_func_data())
    );
    assert_eq!(
        table.lookup_func(&SimpleIdentifier("func".into()).into()),
        Ok(&some_func_data())
    );

    table.emplace_var(&SimpleIdentifier("var".into()).into());
    assert_eq!(
        table.lookup_func(&SimpleIdentifier("var".into()).into()),
        Err(SymTableError::ExpectedFuncFoundVar(
            SimpleIdentifier("var".into()).into()
        ))
    );
}
