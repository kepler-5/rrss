use crate::frontend::ast::{Block, Continue, SimpleIdentifier, WithRange};

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
fn lookup() {
    let mut e = Environment::new();
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("foo".into()).into()),
        Err(SymTableError::NameNotFound(SimpleIdentifier("foo".into()).into()).into())
    );
    assert!(e.create_var(&SimpleIdentifier("foo".into()).into()).is_ok());
    assert!(e
        .create_func(
            &SimpleIdentifier("func".into()).into(),
            Arc::new(some_func_data())
        )
        .is_ok());
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("foo".into()).into()),
        Ok(&Val::Undefined)
    );
    assert_eq!(
        e.lookup_func(&SimpleIdentifier("func".into()).into()),
        Ok(&some_func_data())
    );
    e.push_scope();
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("foo".into()).into()),
        Ok(&Val::Undefined)
    );
    assert_eq!(
        e.lookup_func(&SimpleIdentifier("func".into()).into()),
        Ok(&some_func_data())
    );
    assert!(e.create_var(&SimpleIdentifier("bar".into()).into()).is_ok());
    assert!(e
        .create_func(
            &SimpleIdentifier("func".into()).into(),
            Arc::new(other_func_data())
        )
        .is_ok());
    assert_eq!(
        e.lookup_func(&SimpleIdentifier("func".into()).into()),
        Ok(&other_func_data())
    );
    assert!(e
        .create_func(
            &SimpleIdentifier("func2".into()).into(),
            Arc::new(other_func_data())
        )
        .is_ok());
    assert_eq!(
        e.lookup_func(&SimpleIdentifier("func2".into()).into()),
        Ok(&other_func_data())
    );
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("foo".into()).into()),
        Ok(&Val::Undefined)
    );
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("bar".into()).into()),
        Ok(&Val::Undefined)
    );
    e.pop_scope();
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("foo".into()).into()),
        Ok(&Val::Undefined)
    );
    assert_eq!(
        e.lookup_func(&SimpleIdentifier("func".into()).into()),
        Ok(&some_func_data())
    );
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("bar".into()).into()),
        Err(SymTableError::NameNotFound(SimpleIdentifier("bar".into()).into()).into())
    );
    assert_eq!(
        e.lookup_func(&SimpleIdentifier("func2".into()).into()),
        Err(SymTableError::NameNotFound(SimpleIdentifier("func2".into()).into()).into())
    );
}

#[test]
fn lookup_wrong_type() {
    let mut e = Environment::new();

    assert!(e.create_var(&SimpleIdentifier("foo".into()).into()).is_ok());
    assert!(e
        .create_func(
            &SimpleIdentifier("func".into()).into(),
            Arc::new(some_func_data())
        )
        .is_ok());

    assert_eq!(
        e.lookup_func(&SimpleIdentifier("foo".into()).into()),
        Err(SymTableError::ExpectedFuncFoundVar(SimpleIdentifier("foo".into()).into()).into())
    );
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("func".into()).into()),
        Err(SymTableError::ExpectedVarFoundFunc(SimpleIdentifier("func".into()).into()).into())
    );
}

#[test]
fn push_function_scope() {
    let mut e = Environment::new();
    assert!(e.push_function_scope([].into_iter()).is_ok());
    assert!(e
        .push_function_scope([(&SimpleIdentifier("foo".into()).into(), Val::Null)].into_iter())
        .is_ok());
    assert_eq!(
        e.last_access(),
        Err(EnvironmentError::MissingPronounReferent)
    );
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("foo".into()).into()),
        Ok(&Val::Null)
    );
    assert!(e
        .push_function_scope(
            [
                (&SimpleIdentifier("foo".into()).into(), Val::Undefined),
                (&SimpleIdentifier("bar".into()).into(), Val::Null)
            ]
            .into_iter()
        )
        .is_ok());
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("foo".into()).into()),
        Ok(&Val::Undefined)
    );
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("bar".into()).into()),
        Ok(&Val::Null)
    );
    e.pop_scope();
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("foo".into()).into()),
        Ok(&Val::Null)
    );
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("bar".into()).into()),
        Err(SymTableError::NameNotFound(SimpleIdentifier("bar".into()).into()).into())
    );

    assert_eq!(
        e.push_function_scope(
            [
                (&SimpleIdentifier("foo".into()).into(), Val::Undefined),
                (&SimpleIdentifier("foo".into()).into(), Val::Null)
            ]
            .into_iter()
        ),
        Err(SymTableError::DuplicateFunctionArgName(SimpleIdentifier("foo".into()).into()).into())
    );
}

#[test]
fn output() {
    let mut output = Vec::new();
    let mut e = Environment::raw(stdin(), &mut output);
    assert!(e.output("hello").is_ok());
    // assert_eq!(std::str::from_utf8(&output).unwrap().to_owned(), "hello\n"); // TODO borrow checker doesn't like this
    assert!(e.output("world").is_ok());
    assert_eq!(
        std::str::from_utf8(&output).unwrap().to_owned(),
        "hello\nworld\n"
    );
}

#[test]
fn input() {
    let input = "hello\nworld";
    let mut e = Environment::raw(input.as_bytes(), stdout());
    assert_eq!(e.input(), Ok("hello".into()));
    assert_eq!(e.input(), Ok("world".into()));
    assert_eq!(e.input(), Ok("".into()));
}
