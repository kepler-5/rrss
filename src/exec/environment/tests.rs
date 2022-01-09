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
