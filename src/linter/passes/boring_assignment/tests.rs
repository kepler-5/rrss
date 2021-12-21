use super::*;
use crate::{
    frontend::parser::parse,
    linter::{Diag, Diags},
};

#[test]
fn find_nothing() {
    assert_eq!(
        BoringAssignmentPass
            .visit_program(&parse("").unwrap())
            .unwrap()
            .into_diags(),
        Diags(vec![])
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
            .into_diags(),
        Diags(vec![])
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
        let pi be 3.14"
                )
                .unwrap()
            )
            .unwrap()
            .into_diags(),
        Diags(vec![
            Diag {
                issue: "Assignment of literal value `35` into `z` isn't very rock'n'roll".into(),
                suggestions: vec![
                    "Consider using a poetic literal such as: `z is *** *****`".into()
                ],
            },
            Diag {
                issue: "Assignment of literal value `3.14` into `pi` isn't very rock'n'roll".into(),
                suggestions: vec![
                    "Consider using a poetic literal such as: `pi is ***. * ****`".into()
                ],
            },
        ])
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
            .into_diags(),
        Diags(vec![
            Diag {
                issue: "Assignment of literal value `23` into `x` isn't very rock'n'roll".into(),
                suggestions: vec!["Consider using a poetic literal such as: `x is ** ***`".into()],
            },
            Diag {
                issue: "Assignment of literal value `0` into `counter` isn't very rock'n'roll"
                    .into(),
                suggestions: vec![
                    "Consider using a poetic literal such as: `counter is **********`".into()
                ],
            }
        ])
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
            .into_diags(),
        Diags(vec![
            Diag {
                issue: "Assignment of literal value `45` into `the array` isn't very rock'n'roll"
                    .into(),
                suggestions: vec![
                    "Consider using a poetic literal such as: `Rock the array like **** *****`"
                        .into()
                ],
            },
            Diag {
                issue: "Assignment of literal value `0` into `the array` isn't very rock'n'roll"
                    .into(),
                suggestions: vec![
                    "Consider using a poetic literal such as: `Rock the array like **********`"
                        .into()
                ],
            }
        ])
    );
}
