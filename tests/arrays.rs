mod util;

use util::*;

#[test]
fn array_functions() {
    assert_eq!(
        run(
            Code(
                r#"\
Shout "function that does nothing"

The void takes something
Give back nothing

Rock a list with nothing
Shout a list
Shout a list at 0
Let me be the void taking nothing
Shout a list
Shout a list at 0

Whisper empty

Shout "function that multiplies elements in an array"

Multiplier takes the array
The product is 1
Until the array is nothing
Roll the array into the variable
Shout the variable
Let the product be the product of the variable

Give back the product

Rock the list with 3,4,5,6,7,8,9,10
Let the answer be multiplier taking the list
Shout the answer
Shout the list

Whisper empty

Say "function that returns an array"

Rock collection
Shout collection
Shout collection at 0

Append takes the array and the arg
Rock the array with the arg
Give back the array

Let expansion be append taking collection, "hello"
Shout collection
Shout collection at 0
Shout expansion
Shout expansion at 0
"#
            ),
            Input("")
        ),
        "\
function that does nothing
1
null
1
null

function that multiplies elements in an array
3
4
5
6
7
8
9
10
1814400
8

function that returns an array
0
mysterious
0
mysterious
1
hello
"
    );
}

#[test]
fn arrayalike() {
    assert_eq!(
        run(
            Code(
                r#"\
Let my array at 10000000 be "large"
Shout my array
Shout my array at 9999999
Shout my array at 10000000

Let my string be "abc"
Shout my string at 0
Shout my string at 1
Shout my string at 2
Shout my string at 3
Shout my string                
"#
            ),
            Input("")
        ),
        "\
10000001
mysterious
large
a
b
c
mysterious
abc
"
    );
}

#[test]
fn arrays() {
    assert_eq!(
        run(
            Code(
                r#"\
Let my array at 0 be "foo"
Let my array at 1 be "bar"
Let my array at 2 be "baz"
Let my array at "key" be "value"
Shout my array at 0
Shout my array at 1
Shout my array at 2
Shout my array at "key"
Shout my array              
"#
            ),
            Input("")
        ),
        "\
foo
bar
baz
value
3
"
    );
}

#[test]
fn hash() {
    assert_eq!(
        run(
            Code(
                r#"\
Let my hash at "key" be "value"
Shout my hash at "key"
Let my hash at "key" be "hello"
Shout my hash at "key"
Shout my hash
Let my hash at 0 be "zero"
Shout my hash

Midnight says abc
Daylight says def
Let your hash at midnight be daylight
Shout your hash at midnight
Shout daylight
Shout your hash
Moonlight is sweet sorrow
Let your hash at moonlight be nothing
Shout your hash
                            
"#
            ),
            Input("")
        ),
        "\
value
hello
0
1
def
def
0
57
"
    );
}

#[test]
fn join() {
    assert_eq!(
        run(
            Code(
                r#"\
Let the array at 0 be "a"
Let the array at 1 be "b"
Let the array at 2 be "c"
Join the array into ResultA
Shout ResultA

Join the array into ResultB with "-"
Shout ResultB

Join the array
Shout the array

Split "abcde" into tokens
Join tokens with ";"
Shout tokens                          
"#
            ),
            Input("")
        ),
        "\
abc
a-b-c
abc
a;b;c;d;e
"
    );
}

#[test]
fn split() {
    assert_eq!(
        run(
            Code(
                r#"\
Say "IN-PLACE"
Let the string be "abc"
Split the string
Say the string at 0
Say the string at 1
Say the string at 2
Say the string at 3

Say "LITERAL"
Split "wxyz" into digits
Say digits at 0
Say digits at 1
Say digits at 2
Say digits at 3                        
"#
            ),
            Input("")
        ),
        "\
IN-PLACE
a
b
c
mysterious
LITERAL
w
x
y
z
"
    );
}

#[test]
fn split_delimiters() {
    assert_eq!(
        run(
            Code(
                r#"\
Say "IN-PLACE WITH DELIMITER"
Let the string be "a,b,c"
Split the string with ","
Say the string
Say the string at 0
Say the string at 1
Say the string at 2

Say "OUTPUT WITH DELIMITER"
Let the line be "livin' on a prayer"
Split the line into words with " "
Say words at 0
Say words at 1
Say words at 2
Say words at 3                       
"#
            ),
            Input("")
        ),
        "\
IN-PLACE WITH DELIMITER
3
a
b
c
OUTPUT WITH DELIMITER
livin'
on
a
prayer
"
    );
}
