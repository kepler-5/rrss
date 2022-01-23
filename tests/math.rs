mod util;

use util::*;

#[test]
fn operator_aliases() {
    assert_eq!(
        run(
            Code(
                r#"\   
say "addition"
say 2 plus 2
say 2 with 2

say "subtraction"
say 5 minus 3
say 5 without 3

say "multiplication"
say 5 times 5
say 5 of 5

say "division"
say 4 over 2

                
"#
            ),
            Input("")
        ),
        "\
addition
4
4
subtraction
2
2
multiplication
25
25
division
2
"
    );
}

#[test]
fn operator_precedence() {
    assert_eq!(
        run(
            Code(
                r#"\
say 1 + 1 * 2 (= 3)
say 1 * 2 + 1 (= 5)
say 1 / 2 * 3 + 4
say 5-2*3
say 2*3 - 5
                
"#
            ),
            Input("")
        ),
        "\
3
3
5.5
-1
1
"
    );
}

#[test]
fn operators() {
    assert_eq!(
        run(
            Code(
                r#"\
say "addition"
say 1 + 1
say 5 + 10
say 2.5 + 2.5
say 1 + 0.2 + 0.03
say "subtraction"
say 2 - 1
say 0 - 1
say 1 - 0.5
say "multiplication"
say 1 * 0
say 1 * 1
say 1 * 2
say 2 * 2     
"#
            ),
            Input("")
        ),
        "\
addition
2
15
5
1.23
subtraction
1
-1
0.5
multiplication
0
1
2
4
"
    );
}

#[test]
fn rounding() {
    assert_eq!(
        run(
            Code(
                r#"\
The radio is 1.1
Turn down the radio
Shout the radio

The radio is 1.1
Turn the radio down
Shout the radio

The radio is 1.1
Turn up the radio
Shout the radio

The radio is 1.1
Turn the radio up
Shout the radio

The radio is 1.1 
Turn the radio around
Shout the radio

The radio is 1.1 
Turn around the radio
Shout the radio


The radio is 1.1 
Turn the radio round
Shout the radio

The radio is 1.1 
Turn round the radio
Shout the radio    
"#
            ),
            Input("")
        ),
        "\
1
1
2
2
1
1
1
1
"
    );
}

#[test]
fn rounding_pronouns() {
    assert_eq!(
        run(
            Code(
                r#"\
The radio is 1.1
Turn it down
Shout the radio

The radio is 1.1
Turn it up
Shout the radio

The radio is 1.1 
Turn it around
Shout the radio

The radio is 1.1 
Turn it round
Shout the radio 
"#
            ),
            Input("")
        ),
        "\
1
2
1
1
"
    );
}
