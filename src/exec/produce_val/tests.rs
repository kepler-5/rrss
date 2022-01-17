use super::*;
use crate::{
    exec::{environment::EnvironmentError, sym_table::SymTableError},
    frontend::parser::Parser,
};

fn parse_expr(code: &str) -> Expression {
    Parser::for_source_code(code).parse_expression().unwrap()
}

fn expr_val<I, O>(e: &RefCell<Environment<I, O>>, code: &str) -> Result<Val, RuntimeError> {
    ProduceVal::new(e)
        .visit_expression(&parse_expr(code))
        .map(|pvo| pvo.0)
}

#[test]
fn produce_literal_val() {
    let e = RefCell::new(Environment::new());
    assert_eq!(expr_val(&e, "42"), Ok(Val::Number(42.0)));
    assert_eq!(expr_val(&e, "null"), Ok(Val::Null));
    assert_eq!(expr_val(&e, "false"), Ok(Val::Boolean(false)));
    assert_eq!(expr_val(&e, "yes"), Ok(Val::Boolean(true)));
    assert_eq!(expr_val(&e, "\"foo\""), Ok(Val::from("foo")));
    assert_eq!(expr_val(&e, "silence"), Ok(Val::from("")));
}

#[test]
fn produce_named_val() {
    let e = RefCell::new(Environment::new());
    assert!(e
        .borrow_mut()
        .create_var(&SimpleIdentifier("foo".into()).into())
        .is_ok());
    assert_eq!(expr_val(&e, "foo"), Ok(Val::Undefined));
    assert_eq!(
        expr_val(&e, "bar"),
        Err(EnvironmentError::from(SymTableError::NameNotFound(
            SimpleIdentifier("bar".into()).into()
        ))
        .into())
    );
}

#[test]
fn produce_pronoun_val() {
    let e = RefCell::new(Environment::new());
    assert_eq!(
        expr_val(&e, "it"),
        Err(EnvironmentError::MissingPronounReferent.into())
    );
    assert!(e
        .borrow_mut()
        .create_var(&SimpleIdentifier("foo".into()).into())
        .is_ok());
    assert_eq!(expr_val(&e, "foo"), Ok(Val::Undefined));
    assert_eq!(expr_val(&e, "it"), Ok(Val::Undefined));
}

#[test]
fn produce_binary_expr_val() {
    let e = RefCell::new(Environment::new());
    *e.borrow_mut()
        .create_var(&SimpleIdentifier("foo".into()).into())
        .unwrap() = Val::Number(3.0);
    assert_eq!(expr_val(&e, "foo + foo"), Ok(Val::Number(6.0)));
    assert_eq!(expr_val(&e, "foo - foo"), Ok(Val::Number(0.0)));
    assert_eq!(expr_val(&e, "foo * foo"), Ok(Val::Number(9.0)));
    assert_eq!(expr_val(&e, "foo * foo, 3"), Ok(Val::Number(27.0)));
    assert_eq!(expr_val(&e, "foo / foo"), Ok(Val::Number(1.0)));

    assert_eq!(expr_val(&e, "true and false"), Ok(Val::Boolean(false)));
    assert_eq!(expr_val(&e, "true or false"), Ok(Val::Boolean(true)));
    assert_eq!(expr_val(&e, "true nor false"), Ok(Val::Boolean(false)));

    assert_eq!(expr_val(&e, "16 is 60"), Ok(Val::Boolean(false)));
    assert_eq!(expr_val(&e, "16 isn't 60"), Ok(Val::Boolean(true)));

    assert_eq!(expr_val(&e, "16 > 60"), Ok(Val::Boolean(false)));
    assert_eq!(expr_val(&e, "16 >= 60"), Ok(Val::Boolean(false)));
    assert_eq!(expr_val(&e, "16 < 60"), Ok(Val::Boolean(true)));
    assert_eq!(expr_val(&e, "16 <= 60"), Ok(Val::Boolean(true)));
}

#[test]
fn produce_unary_expr_val() {
    let e = RefCell::new(Environment::new());
    *e.borrow_mut()
        .create_var(&SimpleIdentifier("foo".into()).into())
        .unwrap() = Val::Number(3.0);
    assert_eq!(expr_val(&e, "-foo"), Ok(Val::Number(-3.0)));
    assert_eq!(expr_val(&e, "not foo"), Ok(Val::Boolean(false)));
}

#[test]
fn produce_subscript_val() {
    let e = RefCell::new(Environment::new());
    {
        let mut e = e.borrow_mut();
        let arr = e
            .create_var(&SimpleIdentifier("foo".into()).into())
            .unwrap();
        assert!(arr.push(once(Val::Null)).is_ok());
    }
    assert_eq!(expr_val(&e, "foo at 0"), Ok(Val::Null));

    *e.borrow_mut()
        .create_var(&SimpleIdentifier("bar".into()).into())
        .unwrap() = Val::from("text");
    assert_eq!(expr_val(&e, "bar at 2"), Ok(Val::from("x")));
}
