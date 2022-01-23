mod util;

use util::*;

#[test]
fn pop() {
    assert_eq!(
        run(
            Code(
                r#"\
Shout "assignment:"
Rock ints with 1, 2, 3
Let the first be roll ints
Let the second be roll ints
Shout the first (outputs 1)
Shout the second (outputs 2)
Shout roll ints (outputs 3)

Shout "roll into variables:"
Rock the list with 4, 5, 6
Roll the list into foo
Roll the list into bar
Shout foo (outputs 4)
Shout bar (outputs 5)
Shout the list (outputs 1)
Roll the list
Shout the list (outputs 0)
Shout roll the list (outputs mysterious)
"#
            ),
            Input("")
        ),
        "\
assignment:
1
2
3
roll into variables:
4
5
1
0
mysterious
"
    );
}

#[test]
fn push() {
    assert_eq!(
        run(
            Code(
                r#"\
Say "empty arrays:"
Rock steady
Shout steady

say "in-place array coercion:"
Jazz is 123
Rock jazz
Shout jazz  (jazz is now a one-element array, so should output 1)
Shout jazz at 0 (should output 12345)
Shout jazz at 1 (should output mysterious)

say "push list expressions:"
Rock ints 7, 8, 9
Shout ints
Shout ints at 0
Shout ints at 1
Shout ints at 2
Shout ints at 3

say "hurricane:"
You are my destiny
Rock you like a hurricane
Shout you 
Shout you at 0
Shout you at 1
"#
            ),
            Input("")
        ),
        "\
empty arrays:
0
in-place array coercion:
1
123
mysterious
push list expressions:
3
7
8
9
mysterious
hurricane:
2
27
19
"
    );
}

#[test]
fn queues() {
    assert_eq!(
        run(
            Code(
                r#"\
rock queue with 1
rock queue with 2
rock queue with 3
shout roll queue
shout roll queue
shout roll queue
shout roll queue
shout queue

rock the list with "first"
rock the list with "second", "third"
rock the list with "fourth"
while the list ain't nothing
shout roll the list
"#
            ),
            Input("")
        ),
        "\
1
2
3
mysterious
0
first
second
third
fourth
"
    );
}

#[test]
fn string_queues() {
    assert_eq!(
        run(
            Code(
                r#"\
My dream is diamond nightmares
Your love is meaningless to me
Rock my dream like a razorblade smile
Rock my dream with your love, your love
Shout my dream at 0
Shout my dream at 1
Shout my dream at 2
Shout my dream at 3
Shout my dream at 4

The night is empty
Until my dream is nothing
Roll my dream into the dark
Cast the dark into the fire
Let the night be the night with the fire

Shout the night
                
"#
            ),
            Input("")
        ),
        "\
70
105
122
122
mysterious
Fizz
"
    );
}
