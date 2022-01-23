mod util;

use util::*;

#[test]
fn constants() {
    assert_eq!(
        run(
            Code(
                r#"\
my heart is true
say my heart

my love is false
say my love
"#
            ),
            Input("")
        ),
        "\
true
false
"
    );
}
