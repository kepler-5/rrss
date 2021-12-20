use super::*;
use crate::{
    frontend::parser::parse,
    linter::{Diag, Diags},
};

#[test]
fn find_boring_assignments() {
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
    assert_eq!(
        BoringAssignmentPass
            .visit_program(
                &parse(
                    "\
        x is 5
        if x isn't 6
        shout x
        put 1 + 1 into z"
                )
                .unwrap()
            )
            .unwrap()
            .into_diags(),
        Diags(vec![
            Diag {
                issue: "Assignment of literal value `5` into `x` isn't very rock'n'roll".into(),
                suggestions: vec![],
            },
            Diag {
                issue: "Assignment of literal value `2` into `z` isn't very rock'n'roll".into(),
                suggestions: vec![],
            }
        ])
    );
}
