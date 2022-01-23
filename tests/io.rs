mod util;

use util::*;

#[test]
fn hello_number() {
    assert_eq!(
        run(
            Code(
                r#"\
say -123.45
say -1
say 0
say 1
say 123.45
"#
            ),
            Input("")
        ),
        "\
-123.45
-1
0
1
123.45
"
    );
}

#[test]
fn hello_world() {
    assert_eq!(
        run(
            Code(
                r#"\
Say "hello world"
Say "Hello World"
SAY "HELLO WORLD"
"#
            ),
            Input("")
        ),
        "\
hello world
Hello World
HELLO WORLD
"
    );
}

#[test]
fn input_test() {
    assert_eq!(
        run(
            Code(
                r#"\
("listen to" stores a line of input into a variable and "listen" reads a line of input without storing it)

listen to my heartbeat
whisper my heartbeat
listen
listen
listen to the wind
shout the wind
"#
            ),
            Input(
                "\
first line
skip
skip
second line
extra
"
            )
        ),
        "\
first line
second line
"
    );
}

#[test]
fn input_test_2() {
    assert_eq!(
        run(
            Code(
                r#"\
(nothing breaks when extra lines of input are provided)

listen to my heartbeat
whisper my heartbeat
"#
            ),
            Input(
                "\
first line
second line
third line
many many lines
"
            )
        ),
        "\
first line
"
    );
}
