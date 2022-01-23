use super::*;
use crate::{analysis::visit::VisitProgram, frontend::parser::parse};

fn pass(code: &str) -> Vec<Diag> {
    MissedPronounPass::new()
        .visit_program(&parse(code).unwrap())
        .unwrap()
        .build()
}

#[test]
fn find_nothing() {
    assert_eq!(pass(""), []);
    assert_eq!(pass("x is 5"), []);
    assert_eq!(
        pass(
            "\
    let x be 5
    shout it
    "
        ),
        []
    );
}

#[test]
fn find_missed_pronouns() {
    assert_eq!(
        pass(
            "\
    let x be 5
    shout x
    "
        ),
        [Diag {
            issue: "Using identifier `x` more than once in a row sounds kinda bad".into(),
            suggestions: vec!["Consider using a pronoun such as `it`".into()],
            line: 2
        }]
    );
    assert_eq!(
        pass(
            "\
    let x be 5
    shout it
    shout x
    "
        ),
        [Diag {
            issue: "Using identifier `x` more than once in a row sounds kinda bad".into(),
            suggestions: vec!["Consider using a pronoun such as `it`".into()],
            line: 3
        }]
    );
    assert_eq!(
        pass(
            "\
    let x be 5
    let y be 5
    shout x
    "
        ),
        []
    );
    assert_eq!(
        pass(
            "\
    let x be 5
    shout x with x
    shout x
    "
        ),
        [
            Diag {
                issue: "Using identifier `x` more than once in a row sounds kinda bad".into(),
                suggestions: vec!["Consider using a pronoun such as `it`".into()],
                line: 2
            },
            Diag {
                issue: "Using identifier `x` more than once in a row sounds kinda bad".into(),
                suggestions: vec!["Consider using a pronoun such as `it`".into()],
                line: 2
            },
            Diag {
                issue: "Using identifier `x` more than once in a row sounds kinda bad".into(),
                suggestions: vec!["Consider using a pronoun such as `it`".into()],
                line: 3
            }
        ]
    );
    assert_eq!(
        pass(
            "\
    let my words be true
    shout my words
    "
        ),
        [Diag {
            issue: "Using identifier `my words` more than once in a row sounds kinda bad".into(),
            suggestions: vec!["Consider using a pronoun such as `it`".into()],
            line: 2
        }]
    );
    assert_eq!(
        pass(
            "\
    polly wants a cracker
    give a cracker back


    let x be 5


    if true is false
    shout true

    polly taking 7
    polly taking 8 (functions can't be called via pronouns, so we shouldn't flag this)

    shout x (don't flag, since we used 'polly' in between even though it's a function)
    "
        ),
        [Diag {
            issue: "Using identifier `a cracker` more than once in a row sounds kinda bad".into(),
            suggestions: vec!["Consider using a pronoun such as `it`".into()],
            line: 2
        }]
    );
}
