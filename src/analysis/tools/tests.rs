use std::collections::HashSet;

use super::*;
use crate::frontend::parser::*;
use ConstantFoldingError::*;

#[test]
fn numeric_constant_folding_expression() {
    let val = |text| {
        NumericConstantFolder
            .visit_expression(&Parser::for_source_code(text).parse_expression().unwrap())
    };
    let n = NumericConstant::from;

    assert_eq!(val("2"), Ok(n(2.0)));
    assert_eq!(val("-2"), Ok(n(-2.0)));
    assert_eq!(val("2 + 2"), Ok(n(4.0)));
    assert_eq!(val("2 - 2"), Ok(n(0.0)));
    assert_eq!(val("2 * 2"), Ok(n(4.0)));
    assert_eq!(val("2 / 2"), Ok(n(1.0)));
    assert_eq!(val("2 + 2 * 2"), Ok(n(6.0)));
    assert_eq!(val("2 + -2 * 2"), Ok(n(-2.0)));
    assert_eq!(val("2 with 2, 2 + 1, 2"), Ok(n(9.0)));

    assert_eq!(val("2 < 2"), Err(WrongType));
    assert_eq!(val("foo"), Err(UnknownValue));
    assert_eq!(val("1 + foo"), Err(UnknownValue));
    assert_eq!(val("\"hello\" + \", world\""), Err(WrongType));
}

#[test]
fn numeric_constant_folding_program() {
    let val = |text| NumericConstantFolder.visit_program(&parse(text).unwrap());
    assert_eq!(val(""), Err(NoType));
    assert_eq!(val("x is 5"), Err(NoType));
}

#[test]
fn find_all_constant_assignments() {
    struct ConstantAssignmentFinder;
    impl ConstantAssignmentFinder {
        fn capture(x: walk::Result<NumericConstantFolder>) -> walk::Result<Self> {
            Ok(match x {
                Ok(c) => [c.value.to_string()].into_iter().collect(),
                Err(_) => <Self as Visitor>::Output::new(),
            })
        }
    }
    impl Visitor for ConstantAssignmentFinder {
        type Output = HashSet<String>;
        type Error = ();

        fn visit_assignment(&mut self, a: &Assignment) -> walk::Result<Self> {
            Self::capture(NumericConstantFolder.visit_expression_list(&a.value))
        }
        fn visit_poetic_number_assignment(
            &mut self,
            p: &PoeticNumberAssignment,
        ) -> walk::Result<Self> {
            Self::capture(NumericConstantFolder.visit_poetic_number_assignment_rhs(&p.rhs))
        }
    }

    assert_eq!(
        ConstantAssignmentFinder.visit_program(&parse("").unwrap()),
        Ok(HashSet::new())
    );

    assert_eq!(
        ConstantAssignmentFinder.visit_program(
            &parse(
                "\
        let x be 5 with 5
        put 6 into y
        z is 10 with 20, 30, 40
        amber is the color of your energy
        amber was ice. A life unfulfilled; wakin' everybody up, taking booze and pills
        foo taking 13, 14, 15, and 16 (not an assignment)
        "
            )
            .unwrap()
        ),
        Ok(["10", "6", "100", "35246", "3.1415926535"]
            .into_iter()
            .map(Into::into)
            .collect())
    );
}
