mod util;

use util::*;

#[test]
fn indented_else() {
    assert_eq!(
        run(
            Code(
                r#"\
My dream is a thought
My hope is a dream
While my hope is less than my dream
    Knock my dream down
    Shout my dream
    If my dream is my hope
    Shout "No"
    
    Else
    Shout "Yes"
                
"#
            ),
            Input("")
        ),
        "\
16
Yes
15
No
"
    );
}

#[test]
fn nested_loops() {
    assert_eq!(
        run(
            Code(
                r#"\
Say "begin"
X is 10
While X is greater than nothing
Y is 0
While Y is less than 3
Build Y up
Say Y

Knock X down
Say X

Say "end"
"#
            ),
            Input("")
        ),
        "\
begin
1
2
3
9
1
2
3
8
1
2
3
7
1
2
3
6
1
2
3
5
1
2
3
4
1
2
3
3
1
2
3
2
1
2
3
1
1
2
3
0
end
"
    );
}

#[test]
fn simple_loops() {
    assert_eq!(
        run(
            Code(
                r#"\
put 10 into I
while I
say I
knock I down
(end loop)
until I
say I
build I up
(end loop)
"#
            ),
            Input("")
        ),
        "\
10
9
8
7
6
5
4
3
2
1
0
"
    );
}
