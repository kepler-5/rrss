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

fn expr_val<I, O>(e: &RefCell<Environment<I, O>>, code: &str) -> Result<Val, RuntimeError>
where
    I: Read,
    O: Write,
{
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
        "hello\n"
    );
    assert_eq!(
        capture_output(
            "
    say 1 + 1
    "
        ),
        "2\n"
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
        "5\n"
    );
}

#[test]
fn input() {
    let capture_output = |code, input: &str| {
        let mut output = Vec::new();
        let e = Environment::refcell_raw(input.as_bytes(), &mut output);
        assert!(exec(&e, code).is_ok());
        std::str::from_utf8(&output).unwrap().to_owned()
    };

    assert_eq!(capture_output("", ""), "");
    assert_eq!(
        capture_output(
            "
    listen to your heart
    shout your heart
    ",
            "hello\nworld"
        ),
        "hello\n"
    );
    assert_eq!(
        capture_output(
            "
    listen
    listen to your heart
    shout your heart
    ",
            "hello\nworld"
        ),
        "world\n"
    );
    assert_eq!(
        capture_output(
            "
    listen to my heart
    listen to your heart
    shout your heart with my heart
    ",
            "hello\nworld"
        ),
        "worldhello\n"
    );
}

#[test]
fn array_push() {
    let capture_output = |code| {
        let mut output = Vec::new();
        let e = Environment::refcell_raw(stdin(), &mut output);
        assert!(exec(&e, code).is_ok());
        std::str::from_utf8(&output).unwrap().to_owned()
    };

    assert_eq!(
        capture_output(
            "
    rock the array
    shout the array
    "
        ),
        "0\n"
    );
    assert_eq!(
        capture_output(
            "
    rock the array with 1
    shout the array
    "
        ),
        "1\n"
    );
    assert_eq!(
        capture_output(
            "
    rock the array with 12
    shout the array
    shout the array at 0
    "
        ),
        "1\n12\n"
    );
    assert_eq!(
        capture_output(
            "
    rock the array with 12
    shout the array
    shout the array at 0
    rock the array with 13
    shout the array
    shout the array at 0
    shout the array at 1
    "
        ),
        "1\n12\n2\n12\n13\n"
    );
    assert_eq!(
        capture_output(
            "
    rock the array with 1, 2, 3
    shout the array
    shout the array at 0
    shout the array at 1
    shout the array at 2
    "
        ),
        "3\n1\n2\n3\n"
    );
    assert_eq!(
        capture_output(
            "
    rock the array like the poetic literal
    shout the array
    shout the array at 0
    "
        ),
        "1\n367\n"
    );
}

#[test]
fn array_pop() {
    let capture_output = |code| {
        let mut output = Vec::new();
        let e = Environment::refcell_raw(stdin(), &mut output);
        assert!(exec(&e, code).is_ok());
        std::str::from_utf8(&output).unwrap().to_owned()
    };

    assert_eq!(
        capture_output(
            "
        Rock the list with 4, 5, 6
        Shout the list
        Roll the list
        Shout the list
        Roll the list
        Shout the list
        Roll the list
        Shout the list
    "
        ),
        "3\n2\n1\n0\n"
    );
    assert_eq!(
        capture_output(
            "
        Rock the list with 4, 5, 6
        Roll the list into foo
        Roll the list into bar
        Roll the list into baz
        Shout foo
        Shout bar
        Shout baz
        Shout the list
    "
        ),
        "4\n5\n6\n0\n"
    );

    assert_eq!(
        capture_output(
            "
        Rock the list with 4, 5, 6
        Roll the list into the list at 2
        Shout the list at 2
        Roll the list into the list at 3
        Shout the list at 3
        Shout the list at 2
        Roll the list into the list
        Shout the list
    "
        ),
        "4\n5\nmysterious\n6\n"
    );
}

#[test]
fn split() {
    let exec = |code| {
        let mut output = Vec::new();
        let e = Environment::refcell_raw(stdin(), &mut output);
        exec(&e, code).map(|_| output)
    };
    let capture_output = |code| {
        let output = exec(code).unwrap();
        std::str::from_utf8(&output).unwrap().to_owned()
    };

    assert_eq!(
        capture_output(
            "
            Split \"a,b,c\" into the array
            shout the array at 0
            shout the array at 1
            shout the array at 2

            Split \"a,b,c\" into the array with \",\"
            shout the array at 0
            shout the array at 1
            shout the array at 2
    "
        ),
        "a\n,\nb\na\nb\nc\n"
    );
}

#[test]
fn join() {
    let exec = |code| {
        let mut output = Vec::new();
        let e = Environment::refcell_raw(stdin(), &mut output);
        exec(&e, code).map(|_| output)
    };
    let capture_output = |code| {
        let output = exec(code).unwrap();
        std::str::from_utf8(&output).unwrap().to_owned()
    };

    assert_eq!(
        capture_output(
            "
            Let the string be \"abcde\"
            Split the string into tokens
            Join tokens with \";\"
            shout tokens
    "
        ),
        "a;b;c;d;e\n"
    );
    assert_eq!(
        capture_output(
            "
            The input says hey now hey now now
            Split the input into words with \" \"
            Unite words into the output with \"! \"
            shout the output plus \"!\"
    "
        ),
        "hey! now! hey! now! now!\n"
    );
}

#[test]
fn cast() {
    let exec = |code| {
        let mut output = Vec::new();
        let e = Environment::refcell_raw(stdin(), &mut output);
        exec(&e, code).map(|_| output)
    };
    let capture_output = |code| {
        let output = exec(code).unwrap();
        std::str::from_utf8(&output).unwrap().to_owned()
    };

    assert_eq!(
        capture_output(
            "
            Let the string be \"abcde\"
            Split the string into tokens
            Join tokens with \";\"
            shout tokens
    "
        ),
        "a;b;c;d;e\n"
    );
    assert_eq!(
        capture_output(
            "
            Let X be \"123.45\"
            shout X
            Cast X
            shout X
            Let X be \"ff\"
            shout X
            Cast X with 16
            shout X
            Cast \"12345\" into result
            shout result
            shout X
            Cast \"aa\" into result with 16
            shout result
            Cast X into result
            shout X
            shout result
    "
        ),
        "123.45\n123.45\nff\n255\n12345\n255\n170\n255\nÿ\n"
    );
}

#[test]
fn function() {
    let e = Environment::refcell();
    assert!(e
        .borrow()
        .lookup_func(&SimpleIdentifier("polly".into()).into())
        .is_err());
    assert!(exec(
        &e,
        "
    polly wants a cracker
    give a cracker back
    "
    )
    .is_ok());
    assert!(e
        .borrow()
        .lookup_func(&SimpleIdentifier("polly".into()).into())
        .is_ok());
}

#[test]
fn return_statement() {
    let ret_val = |code| {
        let env = Environment::refcell();
        let block = Parser::for_source_code(code).parse_block().unwrap();
        let mut exec = ExecStmt::new(&env);
        assert!(exec.visit_block(&block).is_ok());
        exec.return_val()
    };

    assert_eq!(ret_val(""), Val::Undefined);
    assert_eq!(ret_val("return null"), Val::Null);
    assert_eq!(
        ret_val(
            "\
    x is 5
    if x is 5
    return x

    return x + 1
    "
        ),
        Val::Number(5.0)
    );
    assert_eq!(
        ret_val(
            "\
    x is 5
    if x ain't 5
    return x

    return x + 1
    "
        ),
        Val::Number(6.0)
    );
    assert_eq!(
        ret_val(
            "\
    x is 0
    until x is 10
    let x be with 1
    if x is 5
    return x
    "
        ),
        Val::Number(5.0)
    );
    assert_eq!(
        ret_val(
            "\
    x is 0
    until x is 10
    let x be with 1
    if x is 5
    return x


    return null
    "
        ),
        Val::Number(5.0)
    );
    assert_eq!(
        ret_val(
            "\
    x is 0
    until x is 10
    let x be with 1
    if x is 15
    return x


    return null
    "
        ),
        Val::Null
    );
}

#[test]
fn function_call() {
    let exec = |code| {
        let mut output = Vec::new();
        let e = Environment::refcell_raw(stdin(), &mut output);
        exec(&e, code).map(|_| output)
    };
    let capture_output = |code| {
        let output = exec(code).unwrap();
        std::str::from_utf8(&output).unwrap().to_owned()
    };

    assert_eq!(
        capture_output(
            "
    Polly wants a cracker
    Cheese is delicious
    Put a cracker with cheese into your mouth
    Give it back


    shout polly taking 5
    shout polly taking 15
    put -9 into x
    shout polly taking x
    "
        ),
        "14\n24\n0\n"
    );

    assert_eq!(
        capture_output(
            "
    Multiply takes X and Y
    return x * y


    shout multiply taking 3, and 5
    shout multiply taking \"3\", and 5
    "
        ),
        "15\n33333\n"
    );

    // some of the below are copied from the official rockstar test suite
    // https://github.com/RockstarLang/rockstar/tree/main/tests/fixtures/functions

    assert_eq!(
        capture_output(
            "
    Echo takes X
    say X
    (end function)
    Echo taking true
    Echo taking \"hello world\"
    put 5 into Temp
    Echo taking Temp

    AddAndPrint takes X, and Y
    put X plus Y into Value
    say Value
    Give back Value
    (end function)
    say AddAndPrint taking 3, 4

    AddOrSub takes X, and B
    if B
    Give Back X plus 1
    (end if)
    say \"else\"
    Give Back X minus 1
    (end function)
    say AddOrSub taking 4, true
    say AddOrSub taking 4, false"
        ),
        "true\nhello world\n5\n7\n7\n5\nelse\n3\n"
    );

    // nested function scopes
    assert_eq!(
        capture_output(
            "
    OuterFunction takes X and Y
    SameNameFunction takes X
    Give back X with \"NESTED\"
    
    Put SameNameFunction taking X into ResultX
    Put SameNameFunction taking Y into ResultY
    Put ResultX with ResultY into ResultXY
    Give back ResultXY
    
    SameNameFunction takes X
    Give back X with \"GLOBAL\"
    
    Shout OuterFunction taking \"foo\", \"bar\" (should print \"fooNESTEDbarNESTED\")
    Shout SameNameFunction taking \"foo\" (should print \"fooGLOBAL\")"
        ),
        "fooNESTEDbarNESTED\nfooGLOBAL\n"
    );

    // array arguments
    assert_eq!(
        capture_output(
            "
    the function takes array
    shout \"The parameters that were passed:\"
    shout array at 0
    shout array at 1
    shout array
    
    let param at 0 be 3
    let param at 1 be 4
    shout \"The parameters to be passed:\"
    shout param at 0
    shout param at 1
    shout param
    the function taking param"
        ),
        "The parameters to be passed:\n3\n4\n2\nThe parameters that were passed:\n3\n4\n2\n"
    );

    // recursion
    assert_eq!(
        capture_output(
            "
    Decrement takes X
    If X is nothing
    Give back X
    Else
    Put X minus 1 into NewX
    Give back Decrement taking NewX
    
    
    Say Decrement taking 5"
        ),
        "0\n"
    );
}
