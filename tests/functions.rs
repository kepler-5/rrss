mod util;

use util::*;

#[test]
fn aliases_for_takes() {
    assert_eq!(
        run(
            Code(
                r#"\
Polly wants a cracker
Cheese is delicious
Put a cracker with cheese in your mouth
Give it back

Shout Polly taking 100

Mama wants a word
A kiss says more
Put a word with a kiss into a letter
Send it back

A child says never
Put mama taking a child into the wild
Shout the wild
"#
            ),
            Input("")
        ),
        "\
109
nevermore
"
    );
}

#[test]
fn array_arguments() {
    assert_eq!(
        run(
            Code(
                r#"\
the function takes array
shout "The parameters that were passed:"
shout array at 0
shout array at 1
shout array

let param at 0 be 3
let param at 1 be 4
shout "The parameters to be passed:"
shout param at 0
shout param at 1
shout param
the function taking param
"#
            ),
            Input("")
        ),
        "\
The parameters to be passed:
3
4
2
The parameters that were passed:
3
4
2
"
    );
}

#[test]
fn function_calls() {
    assert_eq!(
        run(
            Code(
                r#"\
Say "functions"
Adder takes X, and Y
say X plus Y
(end function)

Adder taking 1, 2

Subtractor takes X, Y
say X minus Y
(end function)

Subtractor taking 2, 1

Multiplier takes X & Y
say X times Y
(end function)

Multiplier taking 2 & 3

Divisor takes X'n'Y
say X over Y
(end function)

Divisor taking 4'n'2

ThreeMultiplier takes X, Y, and Z
say X times Y times Z
(end function)

ThreeMultiplier taking 2, 3, and 4
"#
            ),
            Input("")
        ),
        "\
functions
3
1
6
2
24
"
    );
}

#[test]
fn nested_function_scopes() {
    assert_eq!(
        run(
            Code(
                r#"\
OuterFunction takes X and Y
SameNameFunction takes X
Give back X with "NESTED"

Put SameNameFunction taking X into ResultX
Put SameNameFunction taking Y into ResultY
Put ResultX with ResultY into ResultXY
Give back ResultXY

SameNameFunction takes X
Give back X with "GLOBAL"

Shout OuterFunction taking "foo", "bar" (should print "fooNESTEDbarNESTED")
Shout SameNameFunction taking "foo" (should print "fooGLOBAL")
"#
            ),
            Input("")
        ),
        "\
fooNESTEDbarNESTED
fooGLOBAL
"
    );
}

#[test]
fn recursion() {
    assert_eq!(
        run(
            Code(
                r#"\
Decrement takes X
If X is nothing
Give back X
Else
Put X minus 1 into NewX
Give back Decrement taking NewX

Say Decrement taking 5
"#
            ),
            Input("")
        ),
        "\
0
"
    );
}

#[test]
fn simple_functions() {
    assert_eq!(
        run(
            Code(
                r#"\
Echo takes X
say X
(end function)
Echo taking true
Echo taking "hello world"
put 5 into Temp
Echo taking Temp

AddAndPrint takes X, and Y
put X plus Y into Value
say Value
Give back Value
(end function)
say AddAndPrint taking 3, 4

AddOrSub takes X, and B
if B
Give Back X plus 1
(end if)
say "else"
Give Back X minus 1
(end function)
say AddOrSub taking 4, true
say AddOrSub taking 4, false
"#
            ),
            Input("")
        ),
        "\
true
hello world
5
7
7
5
else
3
"
    );
}
