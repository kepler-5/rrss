mod util;

use util::*;

#[test]
fn compound_assignments() {
    assert_eq!(
        run(
            Code(
                r#"\
Let X be 1
Let X be with 1
Shout X (2)

Let Y be 2
Let Y be without 1
Shout Y (1)


Y is a variable
Let it be 2
Shout it

Ozzy is always creeping up on you
My heart is alone in the darkness
Let it be with Ozzy
Shout it

Your spirit is flying free
The moon is up 
Let your spirit be over the moon
Shout it
"#
            ),
            Input("")
        ),
        "\
2
1
2
73461
32
"
    );
}

#[test]
fn lets() {
    assert_eq!(
        run(
            Code(
                r#"\
Let X be 1
Let Y be 2
Let Z be X plus Y

Shout X
Shout Y
Shout Z

Your salvation is pure
Let my sword be your salvation
Shout my sword

My variable is 10
Bad Boy Billy is 5
Let the answer be my variable without Bad Boy Billy
Shout the answer

Let simple be 6
Shout simple

Let the variable be 7
Shout the variable

Let Proper Variable be 2 plus 2 plus 2 plus 2
Shout Proper Variable

Let the price be nothing
Shout the price

Variable is 0
Let it be 1
Shout it


The brave are without number
The fallen are always remembered
Let the heroes be the brave without the fallen
Shout the heroes (76 - 60 = 16)
                
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
7
8
null
1
16
"
    );
}
