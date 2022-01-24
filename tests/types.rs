mod util;

use util::*;

#[test]
fn parsing() {
    assert_eq!(
        run(
            Code(
                r#"\
Say "parsing ints with cast"
Let X be "123"
Let Y be "456"
Let Z be X with Y
Shout Z (123456)
Cast X
Cast Y
Let Z be X with Y
Shout Z (579)

Say "creating chars with cast"
Let X be 65
Cast X
Shout X (A)
"#
            ),
            Input("")
        ),
        "\
parsing ints with cast
123456
579
creating chars with cast
A
"
    );
}
