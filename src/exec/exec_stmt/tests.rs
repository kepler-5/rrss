use super::*;

use crate::{
    exec::val::Val,
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
fn exec_assignment() {
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
