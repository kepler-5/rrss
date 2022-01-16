use crate::frontend::ast::SimpleIdentifier;

use super::*;

#[test]
fn lookup() {
    let mut e = Environment::new();
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("foo".into()).into()),
        Err(SymTableError::NameNotFound(SimpleIdentifier("foo".into()).into()).into())
    );
    e.create_var(&SimpleIdentifier("foo".into()).into());
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("foo".into()).into()),
        Ok(&Val::Undefined)
    );
    e.push_scope();
    assert_eq!(
        e.lookup_var(&SimpleIdentifier("foo".into()).into()),
        Ok(&Val::Undefined)
    );
    e.create_var(&SimpleIdentifier("bar".into()).into());
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
        e.lookup_var(&SimpleIdentifier("bar".into()).into()),
        Err(SymTableError::NameNotFound(SimpleIdentifier("bar".into()).into()).into())
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
