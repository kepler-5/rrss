use crate::{
    exec::{
        environment::EnvironmentError,
        val::{Array, ValError},
    },
    frontend::parser::Parser,
};

use super::*;

fn parse_expr(code: &str) -> Expression {
    Parser::for_source_code(code).parse_expression().unwrap()
}

fn expr_val<I, O>(e: &RefCell<Environment<I, O>>, code: &str) -> Result<Val, RuntimeError>
where
    I: Read,
    O: Write,
{
    ProduceVal::new(e)
        .visit_expression(&parse_expr(code))
        .map(|pvo| pvo.0)
}

fn write_then_read<I, O>(
    e: &RefCell<Environment<I, O>>,
    code: &str,
    new_val: Val,
) -> Result<Val, RuntimeError>
where
    I: Read,
    O: Write,
{
    WriteVal::new(e, |val| {
        *val = new_val.clone();
        Ok(())
    })
    .visit_expression(&parse_expr(code))
    .unwrap()
    .0?;
    expr_val(e, code)
}

#[test]
fn write_named_val() {
    let e = RefCell::new(Environment::new());
    assert_eq!(write_then_read(&e, "foo", Val::Null), Ok(Val::Null));
    assert_eq!(
        write_then_read(&e, "foo", Val::Boolean(false)),
        Ok(Val::Boolean(false))
    );
    assert_eq!(write_then_read(&e, "bar", Val::Null), Ok(Val::Null));
    assert_eq!(expr_val(&e, "foo"), Ok(Val::Boolean(false)));
}

#[test]
fn write_pronoun_val() {
    let e = RefCell::new(Environment::new());
    assert_eq!(
        write_then_read(&e, "it", Val::Null),
        Err(EnvironmentError::MissingPronounReferent.into())
    );
    assert_eq!(write_then_read(&e, "foo", Val::Null), Ok(Val::Null));

    assert_eq!(
        write_then_read(&e, "it", Val::Boolean(false)),
        Ok(Val::Boolean(false))
    );
    assert_eq!(
        write_then_read(&e, "foo", Val::Boolean(false)),
        Ok(Val::Boolean(false))
    );
}

#[test]
fn write_array_subscript() {
    let e = RefCell::new(Environment::new());
    assert_eq!(write_then_read(&e, "foo at 5", Val::Null), Ok(Val::Null));
    assert_eq!(
        write_then_read(&e, "foo at 5 at 5", Val::Null),
        Err(ValError::NotIndexable.into())
    );

    assert_eq!(
        write_then_read(&e, "foo at 6", Val::from(Array::new())),
        Ok(Val::from(Array::new())),
    );
    assert_eq!(
        write_then_read(&e, "foo at 6 at 5", Val::Number(42.0)),
        Ok(Val::from(Val::Number(42.0))),
    );
    assert_eq!(
        expr_val(&e, "foo at 6").unwrap().decay().into_owned(),
        Val::Number(6.0)
    );
}
