use super::*;

use crate::{
    exec::{environment::EnvironmentError, sym_table::SymTableError, val::Val},
    frontend::parser::{self, Parser},
};

fn parse(code: &str) -> Program {
    parser::parse(code).unwrap()
}

fn parse_expr(code: &str) -> Expression {
    Parser::for_source_code(code).parse_expression().unwrap()
}

fn expr_val(e: &RefCell<Environment>, code: &str) -> Result<Val, RuntimeError> {
    ProduceVal::new(e)
        .visit_expression(&parse_expr(code))
        .map(|pvo| pvo.0)
}

fn exec(e: &RefCell<Environment>, code: &str) -> Result<(), RuntimeError> {
    ExecStmt::new(e).visit_program(&parse(code))
}

#[test]
fn assignment() {
    let e = Environment::refcell();
    assert!(exec(&e, "let x be 5").is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(5.0)));
    assert!(exec(&e, "put 0 into x").is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(0.0)));

    assert!(exec(&e, "let y be x with 1, 2, 3").is_ok());
    assert_eq!(expr_val(&e, "y"), Ok(Val::Number(6.0)));
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(0.0)));

    assert!(exec(&e, "Let my array at 255 be \"some value\"").is_ok());
    assert!(expr_val(&e, "my array").unwrap().is_array());
    assert_eq!(expr_val(&e, "MY array at 255"), Ok(Val::from("some value")));

    assert_eq!(
        exec(&e, "let the error be 1, 2, 3"),
        Err(ExecError::NonCompoundAssignmentExpressionListInvalid.into())
    );
}

#[test]
fn poetic_number_assignment() {
    let e = Environment::refcell();
    assert!(exec(&e, "x is 5").is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(5.0)));
    assert!(exec(&e, "x is something").is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(9.0)));

    assert!(exec(&e, "y is x").is_ok());
    assert_eq!(expr_val(&e, "y"), Ok(Val::Number(1.0)));
    assert!(exec(&e, "let y be without 1").is_ok());
    assert_eq!(expr_val(&e, "y"), Ok(Val::Number(0.0)));
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(9.0)));
}

#[test]
fn poetic_string_assignment() {
    let e = Environment::refcell();
    assert!(exec(&e, "x says 5").is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::from("5")));
    assert!(exec(&e, "x says somethin or  other").is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::from("somethin or  other")));
}

#[test]
fn if_statement() {
    let e = Environment::refcell();
    assert!(exec(
        &e,
        "
    x is 0
    if x ain't 0
    put 1 into x
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(0.0)));
    assert!(exec(
        &e,
        "
    x is 0
    if x ain't 0
    put 1 into x
    else
    put 2 into x
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(2.0)));
    assert!(exec(
        &e,
        "
    x is 0
    if x is 0
    put 1 into x
    else
    put 2 into x
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(1.0)));

    assert!(exec(
        &e,
        "
    x is 0
    if x is 0
    put 1 into y
    else
    put 2 into z
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(0.0)));
    assert_eq!(
        expr_val(&e, "y"),
        Err(EnvironmentError::from(SymTableError::NameNotFound(
            SimpleIdentifier("y".into()).into()
        ))
        .into())
    );
    assert_eq!(
        expr_val(&e, "z"),
        Err(EnvironmentError::from(SymTableError::NameNotFound(
            SimpleIdentifier("z".into()).into()
        ))
        .into())
    );
}
