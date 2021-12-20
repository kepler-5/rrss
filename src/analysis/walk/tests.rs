use std::{collections::HashSet, hash::Hash};

use super::*;
use crate::frontend::parser::parse;
use derive_more::From;

#[test]
fn combine() {
    #[derive(Debug, From, PartialEq, Eq)]
    struct IsEmptyResult(bool);
    impl Combine for IsEmptyResult {
        fn combine(self, other: Self) -> Self {
            (self.0 && other.0).into()
        }
    }
    impl Default for IsEmptyResult {
        fn default() -> Self {
            true.into()
        }
    }
    struct IsEmpty;
    impl Visitor for IsEmpty {
        type Result = IsEmptyResult;
        fn visit_block(&mut self, b: &Block) -> Option<Self::Result> {
            Some(b.is_empty().into())
        }
    }

    assert_eq!(
        IsEmpty.visit_program(&Program { code: vec![] }),
        Some(true.into())
    );
    assert_eq!(
        IsEmpty.visit_program(&Program {
            code: vec![Block::empty(), Block::empty(), Block::empty()]
        }),
        Some(true.into())
    );
    assert_eq!(
        IsEmpty.visit_program(&Program {
            code: vec![
                Block::empty(),
                Block::empty(),
                Block(vec![StatementWithLine(Statement::Continue, 3)]),
                Block::empty()
            ]
        }),
        Some(false.into())
    );
}

#[test]
fn short_circuit() {
    struct ExplodeOnThird {
        count: i32,
    }
    impl ExplodeOnThird {
        fn new() -> Self {
            ExplodeOnThird { count: 0 }
        }
    }
    impl Visitor for ExplodeOnThird {
        type Result = ();
        fn visit_block(&mut self, _: &Block) -> Option<Self::Result> {
            self.count += 1;
            match self.count {
                2 => None,
                3 => panic!("explosion!"),
                _ => Some(()),
            }
        }
    }
    type E = ExplodeOnThird;

    assert_eq!(E::new().visit_program(&Program { code: vec![] }), Some(()));
    assert_eq!(
        E::new().visit_program(&Program {
            code: vec![Block::empty()]
        }),
        Some(())
    );
    assert_eq!(
        E::new().visit_program(&Program {
            code: vec![Block::empty(), Block::empty()]
        }),
        None
    );
    assert_eq!(
        E::new().visit_program(&Program {
            code: vec![Block::empty(), Block::empty(), Block::empty()]
        }),
        None
    );
}

#[derive(Debug, Default, From, PartialEq, Eq)]
struct Counter(i32);
impl Combine for Counter {
    fn combine(self, other: Self) -> Self {
        (self.0 + other.0).into()
    }
}

#[test]
fn count_xs() {
    struct CountXs;
    impl Visitor for CountXs {
        type Result = Counter;
        fn visit_simple_identifier(&mut self, n: &SimpleIdentifier) -> Option<Self::Result> {
            Some(((n.0 == "x") as i32).into())
        }
    }

    assert_eq!(
        CountXs.visit_program(&Program { code: vec![] }),
        Some(0.into())
    );
    assert_eq!(
        CountXs.visit_program(&Program {
            code: vec![Block::empty(), Block::empty(), Block::empty()]
        }),
        Some(0.into())
    );

    assert_eq!(
        CountXs.visit_program(
            &parse(
                "
        put 5 into x
        let x be 6
        continue

        if x isn't 5
        shout x
        continue

        break
        continue
        return x
        "
            )
            .unwrap()
        ),
        Some(5.into())
    );

    assert_eq!(
        CountXs.visit_program(
            &parse(
                "
        put 5 into y
        let y be 6
        continue

        if y isn't 5
        shout y
        continue

        break
        continue
        return y
        "
            )
            .unwrap()
        ),
        Some(0.into())
    );
}

#[test]
fn count_xs_unless_there_are_continues() {
    struct CountXsUnlessThereAreContinues;
    impl Visitor for CountXsUnlessThereAreContinues {
        type Result = Counter;
        fn visit_simple_identifier(&mut self, n: &SimpleIdentifier) -> Option<Self::Result> {
            Some(((n.0 == "x") as i32).into())
        }
        fn visit_continue(&mut self) -> Option<Self::Result> {
            None
        }
    }
    assert_eq!(
        CountXsUnlessThereAreContinues.visit_program(
            &parse(
                "
        put 5 into x
        let x be 6
        continue

        if x isn't 5
        shout x
        continue

        break
        continue
        return x
        "
            )
            .unwrap()
        ),
        None
    );
}

impl<T: Eq + Hash + Clone> Combine for HashSet<T> {
    fn combine(mut self, other: Self) -> Self {
        self.extend(other.into_iter());
        self
    }
}

#[test]
fn collect_all_number_literals_functional() {
    struct CollectAllNumberLiterals;
    impl Visitor for CollectAllNumberLiterals {
        type Result = HashSet<String>;

        fn visit_literal_expression(&mut self, e: &LiteralExpression) -> Option<Self::Result> {
            match e {
                LiteralExpression::Number(x) => Some([x.to_string()].into_iter().collect()),
                _ => Self::leaf(()),
            }
        }
        fn visit_poetic_number_literal(&mut self, p: &PoeticNumberLiteral) -> Option<Self::Result> {
            Some([p.compute_value().to_string()].into_iter().collect())
        }
    }
    assert_eq!(
        CollectAllNumberLiterals.visit_program(
            &parse(
                "
        put 5 into x
        let x be 6
        continue

        if x isn't 5
        shout x
        x is the rain on my skin

        break
        continue
        turn 13 up
        give back 1 + 1
        "
            )
            .unwrap()
        ),
        Some(
            ["5", "6", "34224", "13", "1"]
                .into_iter()
                .map(Into::into)
                .collect()
        )
    );
}

#[test]
fn collect_all_number_literals_stateful() {
    #[derive(Default)]
    struct CollectAllNumberLiterals {
        pub literals: HashSet<String>,
    }
    impl Visitor for CollectAllNumberLiterals {
        type Result = ();

        fn visit_literal_expression(&mut self, e: &LiteralExpression) -> Option<Self::Result> {
            if let LiteralExpression::Number(x) = e {
                self.literals.insert(x.to_string());
            }
            Some(())
        }
        fn visit_poetic_number_literal(&mut self, p: &PoeticNumberLiteral) -> Option<Self::Result> {
            self.literals.insert(p.compute_value().to_string());
            Some(())
        }
    }
    let literals = {
        let mut c = CollectAllNumberLiterals::default();
        c.visit_program(
            &parse(
                "
    put 5 into x
    let x be 6
    continue

    if x isn't 5
    shout x
    x is the rain on my skin

    break
    continue
    turn 13 up
    give back 1 + 1
    ",
            )
            .unwrap(),
        );
        c.literals
    };
    assert_eq!(
        literals,
        ["5", "6", "34224", "13", "1"]
            .into_iter()
            .map(Into::into)
            .collect()
    );
}
