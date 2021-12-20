use super::*;
use crate::frontend::parser::parse;

#[test]
fn diags() {
    let diag = |code| parse(code).unwrap_err().into_diag().to_string();

    assert_eq!(
        diag("x"),
        "Parse error (line 1): Expected `is`, `'s`, `'re`, `says`, or `say`"
    );

    assert_eq!(
        diag("x and"),
        "Parse error (line 1): Expected `is`, `'s`, `'re`, `says`, or `say`, found `and`"
    );

    assert_eq!(
        diag(
            "\
        x is 5
        
        x"
        ),
        "Parse error (line 3): Expected `is`, `'s`, `'re`, `says`, or `say`"
    );

    assert_eq!(
        diag(
            "
            
        x"
        ),
        "Parse error (line 3): Expected `is`, `'s`, `'re`, `says`, or `say`"
    );
}
