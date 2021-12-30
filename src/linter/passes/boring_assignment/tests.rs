use super::*;
use crate::{frontend::parser::parse, linter::Diag};
use more_asserts::{assert_ge, assert_le};

#[test]
fn find_nothing() {
    assert_eq!(
        BoringAssignmentPass
            .visit_program(&parse("").unwrap())
            .unwrap()
            .build(),
        []
    );
    assert_eq!(
        BoringAssignmentPass
            .visit_program(
                &parse(
                    "\
            if x is 5
            shout x"
                )
                .unwrap()
            )
            .unwrap()
            .build(),
        []
    );
}

#[test]
fn find_boring_assignments() {
    assert_eq!(
        BoringAssignmentPass
            .visit_program(
                &parse(
                    "\
        if x isn't 6
        shout x
        put 26 + 9 into z
        let pi be 3.14
        let pi be times 2"
                )
                .unwrap()
            )
            .unwrap()
            .build(),
        [
            Diag {
                issue: "Assignment of literal value `35` into `z` isn't very rock'n'roll".into(),
                suggestions: vec![
                    "Consider using a poetic literal such as: `z is *** *****`".into()
                ],
                line: 3
            },
            Diag {
                issue: "Assignment of literal value `3.14` into `pi` isn't very rock'n'roll".into(),
                suggestions: vec![
                    "Consider using a poetic literal such as: `pi is ***. * ****`".into()
                ],
                line: 4
            },
        ]
    );
}

#[test]
fn find_boring_poetic_assignments() {
    assert_eq!(
        BoringAssignmentPass
            .visit_program(
                &parse(
                    "\
        x is 23
        if x isn't 6
        shout x
        counter is 0
        my heart is in your hands"
                )
                .unwrap()
            )
            .unwrap()
            .build(),
        [
            Diag {
                issue: "Assignment of literal value `23` into `x` isn't very rock'n'roll".into(),
                suggestions: vec!["Consider using a poetic literal such as: `x is ** ***`".into()],
                line: 1
            },
            Diag {
                issue: "Assignment of literal value `0` into `counter` isn't very rock'n'roll"
                    .into(),
                suggestions: vec![
                    "Consider using a poetic literal such as: `counter is **********`".into()
                ],
                line: 4
            }
        ]
    );
}

#[test]
fn find_boring_array_pushes() {
    assert_eq!(
        BoringAssignmentPass
            .visit_program(
                &parse(
                    "\
        if x isn't 6
        shout x
        rock the array with 45
        counter is tenletters
        rock the array with 1, 2, 3
        rock the array with 0
        rock you like a hurricane"
                )
                .unwrap()
            )
            .unwrap()
            .build(),
        [
            Diag {
                issue: "Assignment of literal value `45` into `the array` isn't very rock'n'roll"
                    .into(),
                suggestions: vec![
                    "Consider using a poetic literal such as: `Rock the array like **** *****`"
                        .into()
                ],
                line: 3
            },
            Diag {
                issue: "Assignment of literal value `0` into `the array` isn't very rock'n'roll"
                    .into(),
                suggestions: vec![
                    "Consider using a poetic literal such as: `Rock the array like **********`"
                        .into()
                ],
                line: 6
            }
        ]
    );
}

#[test]
fn find_boring_string_assignments() {
    assert_eq!(
        BoringAssignmentPass
            .visit_program(
                &parse(
                    r#"
        x is "hey"
        x is "  hey "
        if x isn't 6
        shout x
        put "Rock the house" into z
        let r be "let's search the sky""#
                )
                .unwrap()
            )
            .unwrap()
            .build(),
        [
            Diag {
                issue: r#"Assignment of literal value `"hey"` into `x` isn't very rock'n'roll"#.into(),
                suggestions: vec!["Consider using a poetic literal such as: `x says hey`".into()],
                line: 2
            },
            Diag {
                issue: r#"Assignment of literal value `"  hey "` into `x` isn't very rock'n'roll"#
                    .into(),
                suggestions: vec!["Consider using a poetic literal such as: `x says   hey `".into()],
                line: 3
            },
            Diag {
                issue: r#"Assignment of literal value `"Rock the house"` into `z` isn't very rock'n'roll"#
                    .into(),
                suggestions: vec![
                    "Consider using a poetic literal such as: `z says Rock the house`"
                        .into()
                ],
                line: 6
            },
            Diag {
                issue: r#"Assignment of literal value `"let's search the sky"` into `r` isn't very rock'n'roll"#
                    .into(),
                suggestions: vec![
                    "Consider using a poetic literal such as: `r says let's search the sky`"
                        .into()
                ],
                line: 7
            }
        ]
    );
}

#[test]
fn estimate_text_size() {
    macro_rules! check {
        ($estimate:expr, $actual:expr) => {
            assert_ge!($estimate, $actual.len()); // big enough
            assert_le!($estimate, ($actual.len() as f32 * 1.5) as usize); // not too big
        };
    }

    let estimate = |x: f64| PoeticNumberLiteralTemplate::from_value(x.into()).estimate_text_size();

    check!(estimate(0.0), "**********");
    check!(estimate(12.0), "* **");
    check!(estimate(3.14159), "***. * **** * ***** *********");
    check!(
        estimate(12345678.0),
        "* ** *** **** ***** ****** ******* ********"
    );
}
