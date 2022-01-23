mod util;

use util::*;

#[test]
fn literal_aliases() {
    assert_eq!(
        run(
            Code(
                r#"\
(all aliases for literals work)
say mysterious
say "aliases for null"
say null
say nothing
say nowhere
say nobody
say gone

say "aliases for empty string"
say empty

say "aliases for true"
say true
say right
say yes
say ok

say "aliases for false"
say false
say wrong
say no
say lies
"#
            ),
            Input("")
        ),
        "\
mysterious
aliases for null
null
null
null
null
null
aliases for empty string

aliases for true
true
true
true
true
aliases for false
false
false
false
false
"
    );
}

#[test]
fn literal_strings() {
    assert_eq!(
        run(
            Code(
                r#"\
Shout "What is not denied is permitted!"
Shout "This ain't not allowed!"
"#
            ),
            Input("")
        ),
        "\
What is not denied is permitted!
This ain't not allowed!
"
    );
}

#[test]
fn poetic_literals() {
    assert_eq!(
        run(
            Code(
                r#"\
(poetic assignments to booleans, null, mysterious, number literals, and string literals all work)
my heart is true
say my heart

your word is lies
say your word

Tommy is mysterious
say Tommy

the money is gone
say the money

the total is 100
say the total

the password is "open sesame"
say the password

the facts are right
say the facts

the answers are nowhere
say the answers

say "case sensitivity"

variable is 1
say variable
Variable Is 2
say variable
variable iS 3
say variable
VARIABLE IS 4
say variable

variables are 1
say variables
Variables Are 2
say variables
variables aRe 3
say variables
VARIABLES ARE 4
say variables
"#
            ),
            Input("")
        ),
        "\
true
false
mysterious
null
100
open sesame
true
null
case sensitivity
1
2
3
4
1
2
3
4
"
    );
}

#[test]
fn poetic_numbers() {
    assert_eq!(
        run(
            Code(
                r#"\
Tommy was a lovestruck ladykiller
say Tommy

Sweet Lucy was a dancer
say Sweet Lucy

A killer is on the loose
whisper a killer

My dreams were ice. A life unfulfilled; wakin' everybody up, taking booze and pills
shout My dreams

Sweet Lucy's a danger to society
say Sweet Lucy

my number is a 57 + true 43
say my number

The streets are lonely
say the streets
"#
            ),
            Input("")
        ),
        "\
100
16
235
3.1415926535
1627
14
6
"
    );
}
