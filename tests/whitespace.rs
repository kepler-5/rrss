mod util;

use util::*;

#[test]
fn apostrophes_ignored() {
    assert_eq!(
        run(
            Code(
                r#"\
('s is valid in poetic assignments with the apostrophe ignored)

my soul is the Devil's to take
shout my soul

my soul is the Devil'sto take
shout my soul

my soul is the Devil 's to take
shout my soul
"#
            ),
            Input("")
        ),
        "\
3624
384
35124
"
    );
}

#[test]
fn leading_blank_lines() {
    assert_eq!(
        run(
            Code(
                r#"\


(this file tests that parsers can handle BLANK leading lines)
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
fn leading_empty_lines() {
    assert_eq!(
        run(
            Code(
                r#"\


(this file tests that parsers can handle EMPTY leading lines)
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
fn leading_whitespace() {
    assert_eq!(
        run(
            Code(
                r#"\
Rockstar is a big bad monster truck
While Rockstar is greater than nothing
    Shout Rockstar
    Put Rockstar without 500 into Rockstar

                
"#
            ),
            Input("")
        ),
        "\
13375
12875
12375
11875
11375
10875
10375
9875
9375
8875
8375
7875
7375
6875
6375
5875
5375
4875
4375
3875
3375
2875
2375
1875
1375
875
375
"
    );
}

#[test]
fn no_newline_at_eof() {
    assert_eq!(
        run(
            Code(
                r#"\
(test whether parser can cope with a file that does not end with a new line)
say "pass""#
            ),
            Input("")
        ),
        "\
pass
"
    );
}

#[test]
fn trailing_blank_lines() {
    assert_eq!(
        run(
            Code(
                r#"\
say "pass"
(test we can cope with blank lines at the end of a file)

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
fn trailing_empty_lines() {
    assert_eq!(
        run(
            Code(
                r#"\
say "pass"
(test we can cope with EMPTY lines - i.e. multiple newlines - at the end of a file)
                
"#
            ),
            Input("")
        ),
        "\
pass
"
    );
}
