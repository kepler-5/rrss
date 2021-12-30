use std::collections::HashSet;

use super::*;
use crate::{analysis::visit::VisitProgram, frontend::parser::*};
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
fn find_all_constant_assignments() {
    struct ConstantAssignmentFinder;
    impl ConstantAssignmentFinder {
        fn capture(x: visit::Result<NumericConstantFolder>) -> visit::Result<Self> {
            Ok(match x {
                Ok(c) => [c.value.to_string()].into_iter().collect(),
                Err(_) => <Self as Visit>::Output::new(),
            })
        }
    }
    impl Visit for ConstantAssignmentFinder {
        type Output = HashSet<String>;
        type Error = ();
    }
    impl VisitProgram for ConstantAssignmentFinder {
        fn visit_assignment(&mut self, a: &Assignment) -> visit::Result<Self> {
            Self::capture(NumericConstantFolder.visit_expression_list(&a.value))
        }
        fn visit_poetic_number_assignment(
            &mut self,
            p: &PoeticNumberAssignment,
        ) -> visit::Result<Self> {
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

#[test]
fn simple_string_constant_folding_expression() {
    let val = |text| {
        SimpleStringConstantFolder
            .visit_expression(&Parser::for_source_code(text).parse_expression().unwrap())
    };
    let s = |text| StringConstant::from(String::from(text));

    assert_eq!(val("\"\""), Ok(s("")));
    assert_eq!(val("\"hello, world\""), Ok(s("hello, world")));

    // intentionally not supported (for *Simple*StringConstantFolder)
    assert_eq!(val("\"foo\" + \"bar\""), Err(PossibleValueIgnored));
    assert_eq!(val("2 * \"bar\""), Err(PossibleValueIgnored));
    assert_eq!(val("\"foo\" times 2, 2, 2"), Err(PossibleValueIgnored));

    assert_eq!(val("2"), Err(WrongType));
    assert_eq!(val("2 + 2"), Err(PossibleValueIgnored));
    assert_eq!(val("foo"), Err(UnknownValue));
}

#[test]
fn find_all_constant_string_assignments() {
    struct ConstantStringAssignmentFinder;
    impl ConstantStringAssignmentFinder {
        fn capture(s: Option<String>) -> visit::Result<Self> {
            Ok(s.map(|s| [s].into_iter().collect())
                .unwrap_or_else(<Self as Visit>::Output::new))
        }
    }
    impl Visit for ConstantStringAssignmentFinder {
        type Output = HashSet<String>;
        type Error = ();
    }
    impl VisitProgram for ConstantStringAssignmentFinder {
        fn visit_assignment(&mut self, a: &Assignment) -> visit::Result<Self> {
            Self::capture(
                SimpleStringConstantFolder
                    .visit_expression_list(&a.value)
                    .ok()
                    .map(|c| c.value),
            )
        }
        fn visit_poetic_string_assignment(
            &mut self,
            p: &PoeticStringAssignment,
        ) -> visit::Result<Self> {
            Self::capture(Some(p.rhs.clone()))
        }
    }

    assert_eq!(
        ConstantStringAssignmentFinder.visit_program(&parse("").unwrap()),
        Ok(HashSet::new())
    );

    assert_eq!(
        ConstantStringAssignmentFinder.visit_program(
            &parse(
                r#"\
        let x be "hello"
        put "world" into y
        z is 10 with 20, 30, 40
        amber says the color of your energy
        amber said ice. A life unfulfilled; wakin' everybody up, taking booze and pills
        foo taking "blah" (not an assignment)
        "#
            )
            .unwrap()
        ),
        Ok([
            "hello",
            "world",
            "the color of your energy",
            "ice. A life unfulfilled; wakin' everybody up, taking booze and pills"
        ]
        .into_iter()
        .map(Into::into)
        .collect())
    );
}
