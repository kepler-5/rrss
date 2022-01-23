mod util;

use util::*;

#[test]
fn add_operator() {
    assert_eq!(
        run(
            Code(
                r#"\
say 5 with 8
put 5 into my left
put 8 into my right
say my left plus my right
say 5 plus 8 plus 10.5
say 5 plus nothing

(string concatenation)
say "hello" plus " " plus "world"

(string + number)
say "hello" plus " " plus 0.85
say "hello" plus " " plus 5
say "hello" plus " " plus .11
say 5.5 plus " " plus "hello"

(string + boolean)
say "your words are " plus lies
say "your words are " plus True
say true plus ", you are."

(string + null)
say null plus " and void"
say "void and " plus nothing
"#
            ),
            Input("")
        ),
        "\
13
13
23.5
5
hello world
hello 0.85
hello 5
hello 0.11
5.5 hello
your words are false
your words are true
true, you are.
null and void
void and null
"
    );
}

#[test]
fn and_test() {
    assert_eq!(
        run(
            Code(
                r#"\
TrueFunc takes Ignored
Give back True
(end function)
SayHiAndReturn takes V
say "Hi"
Give back V
(end function)
say true and true
say true and false
say false and true
say false and false
say TrueFunc taking nothing and TrueFunc taking nothing
say false and 1 over 0 is 0
say true and SayHiAndReturn taking true
say false and SayHiAndReturn taking true
"#
            ),
            Input("")
        ),
        "\
true
false
false
false
true
false
Hi
true
false
"
    );
}

#[test]
fn booleans() {
    assert_eq!(
        run(
            Code(
                r#"\
X is true
Y is false
say X           (true)
say X and Y     (false)
say X or Y      (true)
say not X       (false)
say not X and not Y (false)
say not X or not Y (true)
say not X and not Y and not X or not Y (true)


                
"#
            ),
            Input("")
        ),
        "\
true
false
true
false
false
true
true
"
    );
}

#[test]
fn division_operator() {
    assert_eq!(
        run(
            Code(
                r#"\
say 5 over 2
say 2 over 2
say 4 over 2 over 2
say nothing over 2               
"#
            ),
            Input("")
        ),
        "\
2.5
1
1
0
"
    );
}

#[test]
fn increment_and_decrement() {
    assert_eq!(
        run(
            Code(
                r#"\
say "integers"
(increment and decrement works on integers)
put 5 into my world
put my world into my temp
build my world up
say my world
say my temp

say "multiples"
(multi increment and decrements, with optional commas)
put 5 into my world
build my world up, up, up up up, up
say my world
knock my world down down, down down down down
say my world

say "decimals"
(increment and decrement works on decimals)
put 2.25 into Other Variable
knock Other Variable down
say Other Variable

say "booleans"
(increment and decrement flip the value of booleans)
my test is true
knock my test down
say my test
knock my test down
say my test
build my test up
say my test
build my test up
say my test
build my test up up up up
say my test
build my test up up up
say my test
knock my test down down down down
say my test
knock my test down down down
say my test               
"#
            ),
            Input("")
        ),
        "\
integers
6
5
multiples
11
5
decimals
1.25
booleans
false
true
false
true
true
false
false
true
"
    );
}

#[test]
fn list_expressions_arithmetic() {
    assert_eq!(
        run(
            Code(
                r#"\
Let X be 1
Let Y be X with 2, 3, 4
Shout Y

Tommy says we gotta hold on
Let Tommy be with " to", " what", " we've", " got"
Shout Tommy

My life is always cold
Now is nothing
Build it up, up
Let my life be over now, now, now, now
Shout my life

The wolf is hungry, out on the street
Fear is the mind killer
Fury is the demon child
Hate is the only truth
Let the wolf be without fear, fury, and hate
Shout the wolf

Let Factorial be 6 times 5, 4, 3, 2, 1
Shout Factorial

Let chant be "Hey! " times 2, 2, 2
Shout chant

The songs are long forgotten,
Longing is exquisite,
Beauty is a memory,
Temptation is a lie
Let the songs be of beauty, and temptation, and longing
Whisper the songs 

(49 * 9 * 16 * 13 = 91728)

                         
"#
            ),
            Input("")
        ),
        "\
10
we gotta hold on to what we've got
4
62190
720
Hey! Hey! Hey! Hey! Hey! Hey! Hey! Hey! 
91728
"
    );
}

#[test]
fn multiplication_operator() {
    assert_eq!(
        run(
            Code(
                r#"\
say 5 of 5
say 2 of 4
say 4 of 3 of 2 of 1
say -3 of 2
say nothing times 5
say "hello" times 2
say "hello" times "world"  
"#
            ),
            Input("")
        ),
        "\
25
8
24
-6
0
hellohello
mysterious
"
    );
}

#[test]
fn not_test() {
    assert_eq!(
        run(
            Code(
                r#"\
say not True
say not 5
say not "hello"
say not 0
say not mysterious
say not null
(verify 5 is not 4 isn't the same as 5 is [not 4])
say 5 ain't 4
say false is not 4
say not not not true
"#
            ),
            Input("")
        ),
        "\
false
false
false
true
true
true
true
true
false
"
    );
}

#[test]
fn or_not_test() {
    assert_eq!(
        run(
            Code(
                r#"\
SayAndReturn takes B
say "Hi"
give back B
(end function)
say true or true
say true or false
say false or true
say false or false
say true or SayAndReturn taking false
say false or SayAndReturn taking false
say false or SayAndReturn taking true

say true nor true
say true nor false
say false nor true
say false nor false
say true nor SayAndReturn taking false
say false nor SayAndReturn taking false
say false nor SayAndReturn taking true
"#
            ),
            Input("")
        ),
        "\
true
true
true
false
true
Hi
false
Hi
true
false
false
false
true
false
Hi
true
Hi
false
"
    );
}

#[test]
fn ordering_comparison() {
    assert_eq!(
        run(
            Code(
                r#"\
say 5 is lower than 6
say 6 is lower than 5
say 5 is as low as 6
say 6 is as low as 5
say 5 is as low as 5
say 6 is higher than 5
say 5 is higher than 6
say 6 is as high as 5
say 5 is as high as 6
say 6 is as high as 6
say ""
say -3 is lower than nothing
say 3 is lower than nothing
say nothing is lower than nothing
say nothing is as low as null
say ""
say "a" is lower than "b"
say "b" is lower than "a"
say "a" is as low as "b"
say "b" is as low as "a"
say "a" is as low as "a"
say "b" is higher than "a"
say "a" is higher than "b"
say "b" is as high as "a"
say "a" is as high as "b"
say "a" is as high as "a"
say "abcd" is lower than "abce"
say ""
say "-123" is lower than 123
say 123 is higher than "-123"
say ""

say "Aliases"
ten is 10

say "is"
say ten is higher   than 9
say ten is greater  than 9
say ten is bigger   than 9
say ten is stronger than 9
say ten is lower    than 11
say ten is less     than 11
say ten is smaller  than 11
say ten is weaker   than 11

say "'s"
say ten's higher   than 9
say ten's greater  than 9
say ten's bigger   than 9
say ten's stronger than 9
say ten's lower    than 11
say ten's less     than 11
say ten's smaller  than 11
say ten's weaker   than 11

say "are"
say ten are higher   than 9
say ten are greater  than 9
say ten are bigger   than 9
say ten are stronger than 9
say ten are lower    than 11
say ten are less     than 11
say ten are smaller  than 11
say ten are weaker   than 11

say "'re"
say ten're higher   than 9
say ten're greater  than 9
say ten're bigger   than 9
say ten're stronger than 9
say ten're lower    than 11
say ten're less     than 11
say ten're smaller  than 11
say ten're weaker   than 11

say "was"
say ten was higher   than 9
say ten was greater  than 9
say ten was bigger   than 9
say ten was stronger than 9
say ten was lower    than 11
say ten was less     than 11
say ten was smaller  than 11
say ten was weaker   than 11

say "were"
say ten were higher   than 9
say ten were greater  than 9
say ten were bigger   than 9
say ten were stronger than 9
say ten were lower    than 11
say ten were less     than 11
say ten were smaller  than 11
say ten were weaker   than 11
"#
            ),
            Input("")
        ),
        "\
true
false
true
false
true
true
false
true
false
true

true
false
false
true

true
false
true
false
true
true
false
true
false
true
true

true
true

Aliases
is
true
true
true
true
true
true
true
true
's
true
true
true
true
true
true
true
true
are
true
true
true
true
true
true
true
true
're
true
true
true
true
true
true
true
true
was
true
true
true
true
true
true
true
true
were
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
fn subtract_operator() {
    assert_eq!(
        run(
            Code(
                r#"\
say .5 without 0.25
say 0 minus 3
say nothing minus 3
put 5 without 2 without 1 into X
say X
"#
            ),
            Input("")
        ),
        "\
0.25
-3
-3
2
"
    );
}
