use crate::frontend::{
    ast::{ArraySubscript, WithRange},
    source_range::SourceRange,
};

use super::*;

fn bogus_range() -> SourceRange {
    ((0, 0), (0, 0)).into()
}

#[test]
fn render_variable_name() {
    assert_eq!(
        VariableName::from(SimpleIdentifier("foo".into())).render(),
        "foo"
    );
    assert_eq!(
        VariableName::from(SimpleIdentifier("Foo".into())).render(),
        "Foo"
    );
    assert_eq!(
        VariableName::from(CommonIdentifier("my".into(), "foo".into())).render(),
        "my foo"
    );
    assert_eq!(
        VariableName::from(CommonIdentifier("My".into(), "foo".into())).render(),
        "My foo"
    );
    assert_eq!(
        VariableName::from(ProperIdentifier(
            ["John", "Jacob", "Jingleheimer", "Schmidt"]
                .into_iter()
                .map(Into::into)
                .collect()
        ))
        .render(),
        "John Jacob Jingleheimer Schmidt"
    );
}

#[test]
fn render_identifier() {
    assert_eq!(
        Identifier::from(SimpleIdentifier("foo".into())).render(),
        "foo"
    );
    assert_eq!(Identifier::Pronoun.render(), "<pronoun>");
}

#[test]
fn render_assignment_lhs() {
    assert_eq!(
        AssignmentLHS::from(WithRange(SimpleIdentifier("foo".into()), bogus_range())).render(),
        "foo"
    );
    assert_eq!(
        AssignmentLHS::from(ArraySubscript {
            array: Box::new(WithRange(SimpleIdentifier("foo".into()), bogus_range()).into()),
            subscript: Box::new(WithRange(SimpleIdentifier("bar".into()), bogus_range()).into())
        })
        .render(),
        "<expression>"
    );
}
