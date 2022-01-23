mod util;

use util::*;

#[test]
fn common_variables() {
    assert_eq!(
        run(
            Code(
                r#"\
a     variable is 1
an    variable is 2
the   variable is 3
my    variable is 4
your  variable is 5
our   variable is 6

say a     variable
say an    variable
say the   variable
say my    variable
say your  variable
say our   variable

a     variables are 11
an    variables are 12
the   variables are 13
my    variables are 14
your  variables are 15
our   variables are 16

say a     variables
say an    variables
say the   variables
say my    variables
say your  variables
say our   variables

say "expressions"
say a variable with the variable
say my variable with your variable

say "case insensitivity"
THE TIME is 1
say THE TIME
say THE time
say The Time
say tHe tImE
say the Time
say the time

THE time is 2
say THE TIME
say THE time
say The Time
say tHe tImE
say the Time
say the time

The Time is 3
say THE TIME
say THE time
say The Time
say tHe tImE
say the Time
say the time

tHe tImE is 4
say THE TIME
say THE time
say The Time
say tHe tImE
say the Time
say the time

the Time is 5
say THE TIME
say THE time
say The Time
say tHe tImE
say the Time
say the time

the time is 6
say THE TIME
say THE time
say The Time
say tHe tImE
say the Time
say the time

the times are 7
say the times
the times aRE 8
say the times
"#
            ),
            Input("")
        ),
        "\
1
2
3
4
5
6
11
12
13
14
15
16
expressions
4
9
case insensitivity
1
1
1
1
1
1
2
2
2
2
2
2
3
3
3
3
3
3
4
4
4
4
4
4
5
5
5
5
5
5
6
6
6
6
6
6
7
8
"
    );
}

#[test]
fn global_variables() {
    assert_eq!(
        run(
            Code(
                r#"\
put 5 into Global

MyFunc takes X
Give back X plus Global
(end func)

say MyFunc taking 2

MyFuncTwo takes X
Give back X plus GlobalTwo
(end func)

put 6 into GlobalTwo
say MyFuncTwo taking 2

(variables defined globally can be changed from functions)

SetGlobal takes X
put X into Global
(end func)

put 1 into Global
say Global
SetGlobal taking 2
say Global
"#
            ),
            Input("")
        ),
        "\
7
8
1
2
"
    );
}

#[test]
fn poetic_strings() {
    assert_eq!(
        run(
            Code(
                r#"\
my girlfriend says hello to Rockstar!
shout my girlfriend

Albert Einstein says he's got a new theory to share
whisper Albert Einstein

the darkness says Mysterious
whisper the darkness

my world says this. Contains a period. Or three.
shout my world

Somebody say something!
shout somebody

They said I'd never amount to anything. Well I showed them!
shout they
"#
            ),
            Input("")
        ),
        "\
hello to Rockstar!
he's got a new theory to share
Mysterious
this. Contains a period. Or three.
something!
I'd never amount to anything. Well I showed them!
"
    );
}

#[test]
fn pronouns() {
    assert_eq!(
        run(
            Code(
                r#"\
my heart is true
say my heart

it is false
say my heart
say it

put 1 into Epic Music
put 2 into my car
put Epic Music plus my car into it
say Epic Music
say my car

Thomas is true
put "yes" into him
say Thomas

put 5 into Da Way Da World Turns
say it
"#
            ),
            Input("")
        ),
        "\
true
false
false
1
3
yes
5
"
    );
}

#[test]
fn proper_variables() {
    assert_eq!(
        run(
            Code(
                r#"\
Dr Feelgood is 1
say Dr Feelgood

JOHNNY B GOODE is 2
say Johnny B Goode

say "case sensitivity"
(Proper variables are case-insensitive apart from
    the first letter of each word, which must be a capital letter.)
Tom Sawyer is 3
say Tom Sawyer
say TOM SAWYER
say ToM SaWyEr
"#
            ),
            Input("")
        ),
        "\
1
2
case sensitivity
3
3
3
"
    );
}

#[test]
fn simple_pronouns() {
    assert_eq!(
        run(
            Code(
                r#"\
The message is "pass"
say it
say it plus "!"

Gina is 25
Say her
Shout her
Whisper her
Scream her

Test Case is "message"
say it plus " it"
say he plus " he"
say she plus " she"
say him plus " him"
say her plus " her"
say they plus " they"
say them plus " them"
say ze plus " ze"
say hir plus " hir"
say zie plus " zie"
say zir plus " zir"
say xe plus " xe"
say xem plus " xem"
say ve plus " ve"
say ver plus " ver"
"#
            ),
            Input("")
        ),
        "\
pass
pass!
25
25
25
25
message it
message he
message she
message him
message her
message they
message them
message ze
message hir
message zie
message zir
message xe
message xem
message ve
message ver
"
    );
}

#[test]
fn simple_variables() {
    assert_eq!(
        run(
            Code(
                r#"\
Variable is 1
Say variable

Value is 1
say value with value

X is 1
Y is 2
Say x plus y
Alpha says r
Beta says o
Gamma says c
Delta says k
Say alpha with beta with gamma with delta

Let themes be 10 (variable name beginning with pronoun 'the')
Let italics be true (variable name beginning with pronoun 'it')
Shout themes
Shout italics

say "case insensitivity"
TIME is 1
say TIME
say time
say tIMe
say TIMe

time is 2
say TIME
say time
say tIMe
say TIMe

tIMe is 3
say TIME
say time
say tIMe
say TIMe

TIMe is 4
say TIME
say time
say tIMe
say TIMe
"#
            ),
            Input("")
        ),
        "\
1
2
3
rock
10
true
case insensitivity
1
1
1
1
2
2
2
2
3
3
3
3
4
4
4
4
"
    );
}

#[test]
fn umlauts() {
    assert_eq!(
        run(
            Code(
                r#"\
(umlauts should work as parts of common variable names)
The mētäl is true
Shout the mētäl

(proper variable names should allow umlauts too)
Ÿëstërdäÿ is ours
Say it

(umlauts should be allowed in a literal string)
Anna says she lïkes her ümlaüts
Whisper Anna
"#
            ),
            Input("")
        ),
        "\
true
4
she lïkes her ümlaüts
"
    );
}

#[test]
fn write_global() {
    assert_eq!(
        run(
            Code(
                r#"\
global is 0

CheckGlobal takes x
say global
(end of function)

SetGlobal takes x
let global be x
put CheckGlobal taking 0 into dummy
(end of function)

CheckGlobalWithSameName takes global
say global
(end of function)

put SetGlobal taking 1 into dummy
say global

put CheckGlobalWithSameName taking 5 into dummy
say global
"#
            ),
            Input("")
        ),
        "\
1
1
5
1
"
    );
}
