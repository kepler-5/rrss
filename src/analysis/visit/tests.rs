use std::{collections::HashSet, hash::Hash};

use super::*;
use crate::{
    analysis::visit,
    frontend::{parser::parse, source_range::SourceLocation},
};
use derive_more::From;

fn bogus_loc() -> SourceLocation {
    (0, 0).into()
}

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
    impl Visit for IsEmpty {
        type Output = IsEmptyResult;
        type Error = ();
        fn visit_block(&mut self, b: &Block) -> visit::Result<Self> {
            Ok(b.is_empty().into())
        }
    }

    assert_eq!(
        IsEmpty.visit_program(&Program { code: vec![] }),
        Ok(true.into())
    );
    assert_eq!(
        IsEmpty.visit_program(&Program {
            code: vec![
                Block::Empty(bogus_loc()),
                Block::Empty(bogus_loc()),
                Block::Empty(bogus_loc())
            ]
        }),
        Ok(true.into())
    );
    assert_eq!(
        IsEmpty.visit_program(&Program {
            code: vec![
                Block::Empty(bogus_loc()),
                Block::Empty(bogus_loc()),
                Block::NonEmpty(vec![Statement::Continue]),
                Block::Empty(bogus_loc()),
            ]
        }),
        Ok(false.into())
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
    impl Visit for ExplodeOnThird {
        type Output = ();
        type Error = ();
        fn visit_block(&mut self, _: &Block) -> visit::Result<Self> {
            self.count += 1;
            match self.count {
                2 => Err(()),
                3 => panic!("explosion!"),
                _ => Ok(()),
            }
        }
    }
    type E = ExplodeOnThird;

    assert_eq!(E::new().visit_program(&Program { code: vec![] }), Ok(()));
    assert_eq!(
        E::new().visit_program(&Program {
            code: vec![Block::Empty(bogus_loc())]
        }),
        Ok(())
    );
    assert_eq!(
        E::new().visit_program(&Program {
            code: vec![Block::Empty(bogus_loc()), Block::Empty(bogus_loc())]
        }),
        Err(())
    );
    assert_eq!(
        E::new().visit_program(&Program {
            code: vec![
                Block::Empty(bogus_loc()),
                Block::Empty(bogus_loc()),
                Block::Empty(bogus_loc())
            ]
        }),
        Err(())
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
    impl Visit for CountXs {
        type Output = Counter;
        type Error = ();
        fn visit_simple_identifier(
            &mut self,
            n: WithRange<&SimpleIdentifier>,
        ) -> visit::Result<Self> {
            Ok(((n.0 .0 == "x") as i32).into())
        }
    }

    assert_eq!(
        CountXs.visit_program(&Program { code: vec![] }),
        Ok(0.into())
    );
    assert_eq!(
        CountXs.visit_program(&Program {
            code: vec![
                Block::Empty(bogus_loc()),
                Block::Empty(bogus_loc()),
                Block::Empty(bogus_loc())
            ]
        }),
        Ok(0.into())
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
        Ok(5.into())
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
        Ok(0.into())
    );
}

#[test]
fn count_xs_unless_there_are_continues() {
    struct CountXsUnlessThereAreContinues;
    impl Visit for CountXsUnlessThereAreContinues {
        type Output = Counter;
        type Error = &'static str;
        fn visit_simple_identifier(
            &mut self,
            n: WithRange<&SimpleIdentifier>,
        ) -> visit::Result<Self> {
            Ok(((n.0 .0 == "x") as i32).into())
        }
        fn visit_continue(&mut self) -> visit::Result<Self> {
            Err("found continue!")
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
        Err("found continue!")
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
    impl Visit for CollectAllNumberLiterals {
        type Output = HashSet<String>;
        type Error = ();

        fn visit_literal_expression(
            &mut self,
            e: &WithRange<LiteralExpression>,
        ) -> visit::Result<Self> {
            match e.0 {
                LiteralExpression::Number(x) => Ok([x.to_string()].into_iter().collect()),
                _ => leaf(()),
            }
        }
        fn visit_poetic_number_literal(&mut self, p: &PoeticNumberLiteral) -> visit::Result<Self> {
            Ok([p.compute_value().to_string()].into_iter().collect())
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
        give back 1 + 2
        "
            )
            .unwrap()
        ),
        Ok(["5", "6", "34224", "13", "1", "2"]
            .into_iter()
            .map(Into::into)
            .collect())
    );
}

#[test]
fn collect_all_number_literals_stateful() {
    #[derive(Default)]
    struct CollectAllNumberLiterals {
        pub literals: HashSet<String>,
    }
    impl Visit for CollectAllNumberLiterals {
        type Output = ();
        type Error = ();

        fn visit_literal_expression(
            &mut self,
            e: &WithRange<LiteralExpression>,
        ) -> visit::Result<Self> {
            if let LiteralExpression::Number(x) = e.0 {
                self.literals.insert(x.to_string());
            }
            Ok(())
        }
        fn visit_poetic_number_literal(&mut self, p: &PoeticNumberLiteral) -> visit::Result<Self> {
            self.literals.insert(p.compute_value().to_string());
            Ok(())
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
    give back 1 + 2
    ",
            )
            .unwrap(),
        )
        .unwrap();
        c.literals
    };
    assert_eq!(
        literals,
        ["5", "6", "34224", "13", "1", "2"]
            .into_iter()
            .map(Into::into)
            .collect()
    );
}
