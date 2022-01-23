mod util;

use util::*;

#[test]
fn empty_if() {
    assert_eq!(
        run(
            Code(
                r#"\
if false
else
say "pass"
"#
            ),
            Input("")
        ),
        "\
pass
"
    );
}

#[test]
fn simple_conditionals() {
    assert_eq!(
        run(
            Code(
                r#"\
if true
say "hello"
say "world"
else
say "goodbye"
say "everyone"
(end block)

if false
say "hello"
say "world"
else
say "goodbye"
say "everyone"
(end block)

if true
say "hello"
say "world"
(end block)

if true
if false
say "1"
else
say "2"
else
say "3"
"#
            ),
            Input("")
        ),
        "\
hello
world
goodbye
everyone
hello
world
2
"
    );
}

#[test]
fn truthiness_test() {
    assert_eq!(
        run(
            Code(
                r#"\
if ""
say "this should not print since empty strings are false"

if "hello"
say "this should print since non-empty strings are true"

if 0
say "this should not print since 0 is false"

if 1
say "this should print since non-zero numbers are true"

if -3
say "this should print since non-zero numbers, including negatives, are true"

if mysterious
say "this should not print since mysterious is false"

if null
say "this should not print since null is false"

F takes X
give back X

if F
say "this should print since function references are objects which are truthy"
                
"#
            ),
            Input("")
        ),
        "\
this should print since non-empty strings are true
this should print since non-zero numbers are true
this should print since non-zero numbers, including negatives, are true
this should print since function references are objects which are truthy
"
    );
}
