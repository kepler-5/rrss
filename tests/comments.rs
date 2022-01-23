mod util;

use util::*;

#[test]
fn complex_comments() {
    assert_eq!(
        run(
            Code(
                r#"\
say "pass 1" (trailing comment)

(leading comment) say "pass 2"

say (inline comment) "pass 3"

(
multiline
comments
are
also
supported
) say "pass 4"

(((((((( multiple opening brackets are just part of the comment) say "pass 5"
"#
            ),
            Input("")
        ),
        "\
pass 1
pass 2
pass 3
pass 4
pass 5
"
    );
}

#[test]
fn simple_comments() {
    assert_eq!(
        run(
            Code(
                r#"\
(comment on it's own)

say "the words" (comment at the end of a line)

say (comment between items) "you want to say"

(comment before items) say "whatever you want"
"#
            ),
            Input("")
        ),
        "\
the words
you want to say
whatever you want
"
    );
}
