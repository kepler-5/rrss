use std::io::stdin;

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

fn expr_val<I, O>(e: &RefCell<Environment<I, O>>, code: &str) -> Result<Val, RuntimeError> {
    ProduceVal::new(e)
        .visit_expression(&parse_expr(code))
        .map(|pvo| pvo.0)
}

fn exec<I: Read, O: Write>(e: &RefCell<Environment<I, O>>, code: &str) -> Result<(), RuntimeError> {
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

#[test]
fn loop_statement() {
    let e = Environment::refcell();
    assert!(exec(
        &e,
        "
    x is 0
    while x is smaller than 10
    let x be plus 3
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(12.0)));
    assert!(exec(
        &e,
        "
    x is 0
    until x is smaller than 10
    let x be plus 3
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(0.0)));
    assert!(exec(
        &e,
        "
    x is 0
    while x is smaller than 10
    let y be x plus 3
    x is 50
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(50.0)));
    assert_eq!(
        expr_val(&e, "y"),
        Err(EnvironmentError::from(SymTableError::NameNotFound(
            SimpleIdentifier("y".into()).into()
        ))
        .into())
    );
}

#[test]
fn continue_statement() {
    let e = Environment::refcell();
    assert!(exec(
        &e,
        "
    x is 0
    y is 0
    while x is smaller than 10
    let x be plus 1
    continue
    let y be plus 1
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(10.0)));
    assert_eq!(expr_val(&e, "y"), Ok(Val::Number(0.0)));
    assert!(exec(
        &e,
        "
    x is 0
    y is 0
    while x is smaller than 10
    let x be plus 1
    if x is not 5
    continue

    y is 1
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(10.0)));
    assert_eq!(expr_val(&e, "y"), Ok(Val::Number(1.0)));
    assert!(exec(
        &e,
        "
    x is 0
    while x is smaller than 10
    let x be plus 1
    put x + 5 into y
    while x is smaller than y
    let x be plus 5
    continue
    let x be plus 1
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(12.0)));
}

#[test]
fn break_statement() {
    let e = Environment::refcell();
    assert!(exec(
        &e,
        "
    x is 0
    y is 0
    while x is smaller than 10
    let x be plus 1
    break
    let y be plus 1
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(1.0)));
    assert_eq!(expr_val(&e, "y"), Ok(Val::Number(0.0)));
    assert!(exec(
        &e,
        "
    x is 0
    y is 0
    while x is smaller than 10
    let x be plus 1
    if x is 5
    break

    let y be with 1
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(5.0)));
    assert_eq!(expr_val(&e, "y"), Ok(Val::Number(4.0)));
    assert!(exec(
        &e,
        "
    x is 0
    y is 0
    while x is smaller than 10
    let x be plus 1
    while true
    break

    let y be plus 1
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(10.0)));
    assert_eq!(expr_val(&e, "y"), Ok(Val::Number(10.0)));
}

#[test]
fn inc_dec() {
    let e = Environment::refcell();
    assert!(exec(
        &e,
        "
    x is 0
    build x up
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(1.0)));
    assert!(exec(
        &e,
        "
    x is 0
    build x up, up
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(2.0)));
    assert!(exec(
        &e,
        "
    x is 0
    knock x down
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(-1.0)));
}

#[test]
fn rounding() {
    let e = Environment::refcell();
    assert!(exec(
        &e,
        "
    x is 1.5
    turn it up
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(2.0)));
    assert!(exec(
        &e,
        "
    x is 1.5
    turn it down
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(1.0)));
    assert!(exec(
        &e,
        "
    x is 1.7
    turn it around
    "
    )
    .is_ok());
    assert_eq!(expr_val(&e, "x"), Ok(Val::Number(2.0)));
}

#[test]
fn output() {
    let capture_output = |code| {
        let mut output = Vec::new();
        let e = Environment::refcell_raw(stdin(), &mut output);
        assert!(exec(&e, code).is_ok());
        std::str::from_utf8(&output).unwrap().to_owned()
    };

    assert_eq!(capture_output(""), "");
    assert_eq!(
        capture_output(
            "
    x says hello
    shout it
    "
        ),
        "hello"
    );
    assert_eq!(
        capture_output(
            "
    say 1 + 1
    "
        ),
        "2"
    );
    assert_eq!(
        capture_output(
            "
    x is 1
    if x is 2
    say x
    else
    say x * 5
    "
        ),
        "5"
    );
}
