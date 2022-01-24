mod util;

use util::*;

#[test]
fn arrays() {
    assert_eq!(
        run(
            Code(
                r#"\
rock first with 0, 1, 2
rock second with 3, 4, 5

if first ain't second
say "arrays of the same length but different contents are not equal"

ArrayCopy takes source
rock dest
let i be 0
let len be source + 0
while i is less than len
let dest at i be source at i
build i up

return dest

let First Copy be ArrayCopy taking first
if First Copy is first
say "element-wise-copied arrays are equal"

let Second Copy be second
if Second Copy is second
say "assignment-copied arrays are equal"

rock First Nested with first, second
rock Second Nested with first, second
if First Nested is Second Nested
say "nested arrays with the same contents are equal"

rock Third Nested with first, second
rock Fourth Nested with second, first
if Third Nested ain't Fourth Nested
say "nested arrays with different contents are not equal"
"#
            ),
            Input("")
        ),
        "\
arrays of the same length but different contents are not equal
element-wise-copied arrays are equal
assignment-copied arrays are equal
nested arrays with the same contents are equal
nested arrays with different contents are not equal
"
    );
}

#[test]
fn booleans() {
    assert_eq!(
        run(
            Code(
                r#"\
say true is true
say true is false
say false is false
say false is true
"#
            ),
            Input("")
        ),
        "\
true
false
true
false
"
    );
}

#[test]
fn equality_comparison() {
    assert_eq!(
        run(
            Code(
                r#"\
say "Checking for mysterious equalities"
say mysterious is mysterious
say mysterious is true
say mysterious is "hello world"
say mysterious is null
say mysterious is 5
say ""
say "Checking for true v false equalities"
say true is true
say true is false
say false is false
say false is true
say ""
say "Checking for true/number"
say true is 5
say 5 is true
say 0 is true
say true is 0
say ""
say "Checking for false/number"
say false is 5
say 5 is false
say 0 is false
say false is 0
say ""
say "Checking for nulls"
say 5 is null
say 0 is null
say true is null
say false is null
say null is null
say ""
say "Checking boolean strings"
say true is "true"
say true is "RIGht"
say true is "lies"
say true is "hello"
say "true" is true
say "lies" is true
Say ""
Say "Checking false things"
say false is "false"
say false is "WROng"
say false is "true"
say false is "hello"
say "false" is false
say "true" is false
Say ""
say "Checking numbers"
say 5 is 5
say 0.26 is 0.26
say 4 is 3
say ""
say "Checking number/string"
say "5" is 5
say 5 is "05.0"
say 5 is "3"
say 5 is "hello"
say ""
say "Checking strings"
say "hello" is "hello"
say "hello" is "world"
say ""
say "Checking negation"
say true is not false
say true isn't false
say true is not true
say true isn't true
say ""

say "Aliases for is"
say "simple variable"
value is 1
say value is 1
value's 2
say value's 2
values are 3
say values are 3
values're 3
say values're 3
value was 5
say value was 5
values were 6
say values were 6

say "common variable"
the value is 1
say the value is 1
the value's 2
say the value's 2
the values are 3
say the values are 3
the values're 4
say the values're 4
the value was 5
say the value was 5
the values were 6
say the values were 6

say "proper variable"
Proper Value is 1
say Proper VALUE IS 1
PROPER VALUE'S 2
say Proper Value's 2
Proper Values are 3
say Proper Values are 3
PROPER VALUES'RE 4
say Proper Values're 4
Proper Value was 5
say Proper Value was 5
Proper Values were 6
say Proper Values were 6

say "pronoun"
Pronoun Value is 0
it is 1
say it is 1
it's 2
say it's 2
they are 3
say they are 3
they're 4
say they're 4
it was 5
say it was 5
they were 6
say they were 6

say ""

say "Aliases for isn't"
value is 1
say value isn't   2
say value isnt    2
say value ain't   2
say value aint    2
say value aren't  2
say value arent   2
say value wasn't  2
say value wasnt   2
say value weren't 2
say value werent  2
"#
            ),
            Input("")
        ),
        "\
Checking for mysterious equalities
true
false
false
true
false

Checking for true v false equalities
true
false
true
false

Checking for true/number
true
true
false
false

Checking for false/number
false
false
true
true

Checking for nulls
false
true
false
true
true

Checking boolean strings
true
true
true
true
true
true

Checking false things
false
false
false
false
false
false

Checking numbers
true
true
false

Checking number/string
true
true
false
false

Checking strings
true
false

Checking negation
true
true
false
false

Aliases for is
simple variable
true
true
true
true
true
true
common variable
true
true
true
true
true
true
proper variable
true
true
true
true
true
true
pronoun
true
true
true
true
true
true

Aliases for isn't
true
true
true
true
true
true
true
true
true
true
"
    );
}

#[test]
fn mysterious() {
    assert_eq!(
        run(
            Code(
                r#"\
say mysterious is mysterious
say mysterious is true
say mysterious is "hello world"
say mysterious is null
say mysterious is 5
"#
            ),
            Input("")
        ),
        "\
true
false
false
true
false
"
    );
}

#[test]
fn negation() {
    assert_eq!(
        run(
            Code(
                r#"\
say true is not false
say true isn't false
say true ain't false
say true aint false
say true isn't not true
say true ain't not true
say not true is not true
say ""
say true is not true
say true isn't true
say true ain't true
say true aint true
say true isn't not false
say true ain't not false
say not true is not false
"#
            ),
            Input("")
        ),
        "\
true
true
true
true
true
true
true

false
false
false
false
false
false
false
"
    );
}

#[test]
fn nothing() {
    assert_eq!(
        run(
            Code(
                r#"\
Say 0 is nothing
Say false is nothing
Say nobody is nothing
Say null is nothing
Say "" is nothing

Say 1 is nothing
Say true is nothing
Say "hello" is nothing
"#
            ),
            Input("")
        ),
        "\
true
true
true
true
true
false
false
false
"
    );
}

#[test]
fn null() {
    assert_eq!(
        run(
            Code(
                r#"\
say true is null
say false is null
say null is null
say 0 is null
say 1 is null
"#
            ),
            Input("")
        ),
        "\
false
true
true
true
false
"
    );
}

#[test]
fn numbers() {
    assert_eq!(
        run(
            Code(
                r#"\
say true is 5
say 5 is true
say 0 is false
say false is 0
say 0 is null
say 5 is 5
say 5 is 5.0
say 0.26 is 0.26
say "5" is 5
say 5 is "05.0"
say ""
say true is 0
say 0 is true
say 1 is false
say false is 1
say 1 is null
say 5 is 4
"#
            ),
            Input("")
        ),
        "\
true
true
true
true
true
true
true
true
true
true

false
false
false
false
false
false
"
    );
}

#[test]
fn strings() {
    assert_eq!(
        run(
            Code(
                r#"\
say "true" is true
say "yes" is true
say "OK" is true
say ""
say true is "true"
say true is "RIGht"
say "yes" is true
say "OK" is true
say false is "false"
say false is "WROng"
say "lies" is false
say ""
say true is "hello"
say "true" is true
say ""
say 5 is "3"
say 5 is "hello"
say ""
say true is "lies"
say "lies" is true
say false is "true"
say false is "hello"
say "false" is false
say "true" is false
say ""
say "hello" is "hello"
say "hello" is "world"
"#
            ),
            Input("")
        ),
        "\
true
true
true

true
true
true
true
false
false
false

true
true

false
false

true
true
false
false
false
false

true
false
"
    );
}
