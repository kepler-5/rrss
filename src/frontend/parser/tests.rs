use std::sync::Arc;

use crate::frontend::source_range::SourceRange;

use super::*;

use inner::inner;

fn bogus_range() -> SourceRange {
    ((0, 0), (0, 0)).into()
}

fn line_range(start: u32, end: u32) -> SourceRange {
    ((1, start), (1, end)).into()
}

#[test]
fn parse_literal_expression() {
    let parse = |text| Parser::for_source_code(text).parse_literal_expression();
    assert_eq!(
        parse("null"),
        Some(WithRange(LiteralExpression::Null, line_range(0, 4)))
    );
    assert_eq!(
        parse("true"),
        Some(WithRange(
            LiteralExpression::Boolean(true),
            line_range(0, 4)
        ))
    );
    assert_eq!(
        parse("false"),
        Some(WithRange(
            LiteralExpression::Boolean(false),
            line_range(0, 5)
        ))
    );
    assert_eq!(
        parse("empty"),
        Some(WithRange(
            LiteralExpression::String(String::new()),
            line_range(0, 5)
        ))
    );
    assert_eq!(
        parse("\"\""),
        Some(WithRange(
            LiteralExpression::String("".to_owned()),
            line_range(0, 2)
        ))
    );
    assert_eq!(
        parse("5"),
        Some(WithRange(LiteralExpression::Number(5.0), line_range(0, 1)))
    );

    assert_eq!(parse(""), None);
    assert_eq!(parse("foo"), None);
}

#[test]
fn parse_unary_expression() {
    let parse = |text| Parser::for_source_code(text).parse_unary_expression();

    // I don't think the spec provides for unary minus. I'm allowing it since it makes parsing numbers easier
    assert_eq!(
        parse("-1"),
        Ok(UnaryExpression {
            operator: UnaryOperator::Minus,
            operand: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(1, 2)))
        }
        .into())
    );
    assert_eq!(
        parse("--1"),
        Ok(UnaryExpression {
            operator: UnaryOperator::Minus,
            operand: boxed_expr(UnaryExpression {
                operator: UnaryOperator::Minus,
                operand: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(2, 3)))
            })
        }
        .into())
    );

    assert_eq!(
        parse("not cool"),
        Ok(UnaryExpression {
            operator: UnaryOperator::Not,
            operand: boxed_expr(WithRange(SimpleIdentifier("cool".into()), line_range(4, 8)))
        }
        .into())
    );
    assert_eq!(
        parse("not not cool"),
        Ok(UnaryExpression {
            operator: UnaryOperator::Not,
            operand: boxed_expr(UnaryExpression {
                operator: UnaryOperator::Not,
                operand: boxed_expr(WithRange(
                    SimpleIdentifier("cool".into()),
                    line_range(8, 12)
                ))
            })
        }
        .into())
    );
    assert_eq!(
        parse("not -1"),
        Ok(UnaryExpression {
            operator: UnaryOperator::Not,
            operand: boxed_expr(UnaryExpression {
                operator: UnaryOperator::Minus,
                operand: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(5, 6)))
            })
        }
        .into())
    );
}

#[test]
fn parse_binary_expression() {
    let parse = |text| Parser::for_source_code(text).parse_expression();
    assert_eq!(
        parse("1 + 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5)))
        }
        .into())
    );
    assert_eq!(
        parse("1 with 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(7, 8)))
        }
        .into())
    );
    assert_eq!(
        parse("1 - 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Minus,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5)))
        }
        .into())
    );
    assert_eq!(
        parse("1 without 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Minus,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(
                LiteralExpression::Number(1.0),
                line_range(10, 11)
            ))
        }
        .into())
    );
    assert_eq!(
        parse("1 - -1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Minus,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(UnaryExpression {
                operator: UnaryOperator::Minus,
                operand: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(5, 6)))
            })
        }
        .into())
    );
    assert_eq!(
        parse("1 * 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Multiply,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5)))
        }
        .into())
    );
    assert_eq!(
        parse("1 / 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Divide,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5)))
        }
        .into())
    );

    // associativity
    assert_eq!(
        parse("1 + 1 + 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
                rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5)))
            }),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(8, 9)))
        }
        .into())
    );
    assert_eq!(
        parse("1 - 1 - 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Minus,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Minus,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
                rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5)))
            }),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(8, 9)))
        }
        .into())
    );
    assert_eq!(
        parse("1 * 1 + 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Multiply,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
                rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5)))
            }),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(8, 9)))
        }
        .into())
    );
    assert_eq!(
        parse("1 / 1 + 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Divide,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
                rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5)))
            }),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(8, 9)))
        }
        .into())
    );
    assert_eq!(
        parse("1 + 1 * 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Multiply,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5))),
                rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(8, 9)))
            }),
        }
        .into())
    );
    assert_eq!(
        parse("1 + 1 / 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Divide,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5))),
                rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(8, 9)))
            }),
        }
        .into())
    );
}

#[test]
fn parse_comparison_expression() {
    let parse = |text| Parser::for_source_code(text).parse_expression();

    assert_eq!(
        parse("1 < 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Less,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5)))
        }
        .into())
    );
    assert_eq!(
        parse("1 <= 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::LessEq,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(5, 6)))
        }
        .into())
    );
    assert_eq!(
        parse("1 > 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Greater,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5)))
        }
        .into())
    );
    assert_eq!(
        parse("1 >= 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::GreaterEq,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(5, 6)))
        }
        .into())
    );
    assert_eq!(
        parse("1 > 1 < 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Less,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Greater,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
                rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5)))
            }),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(8, 9))),
        }
        .into())
    );
    assert_eq!(
        parse("1 > 1 + 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Greater,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5))),
                rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(8, 9)))
            }),
        }
        .into())
    );
}

#[test]
fn parse_fancy_comparison_expression() {
    let parse = |text| Parser::for_source_code(text).parse_expression();

    assert_eq!(
        parse("1 is 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Eq,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(5, 6)))
        }
        .into())
    );
    assert_eq!(
        parse("1 is not 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::NotEq,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(9, 10)))
        }
        .into())
    );
    assert_eq!(
        parse("Tommy's 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Eq,
            lhs: boxed_expr(WithRange(
                SimpleIdentifier("Tommy".into()),
                line_range(0, 5)
            )),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(8, 9)))
        }
        .into())
    );
    assert_eq!(
        parse("1 ain't 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::NotEq,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(8, 9)))
        }
        .into())
    );
    assert_eq!(
        parse("1 + 1 is not 1 * 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::NotEq,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
                rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(4, 5)))
            }),
            rhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Multiply,
                lhs: boxed_expr(WithRange(
                    LiteralExpression::Number(1.0),
                    line_range(13, 14)
                )),
                rhs: boxed_expr(WithRange(
                    LiteralExpression::Number(1.0),
                    line_range(17, 18)
                ))
            })
        }
        .into())
    );

    assert_eq!(
        parse("1 is bigger than 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Greater,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(
                LiteralExpression::Number(1.0),
                line_range(17, 18)
            ))
        }
        .into())
    );
    assert_eq!(
        parse("1 is as big as 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::GreaterEq,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(
                LiteralExpression::Number(1.0),
                line_range(15, 16)
            ))
        }
        .into())
    );
    assert_eq!(
        parse("1 is smaller than 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Less,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(
                LiteralExpression::Number(1.0),
                line_range(18, 19)
            ))
        }
        .into())
    );
    assert_eq!(
        parse("1 is as small as 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::LessEq,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(
                LiteralExpression::Number(1.0),
                line_range(17, 18)
            ))
        }
        .into())
    );
    assert_eq!(
        parse("1 is 1 is 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Eq,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Eq,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
                rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(5, 6)))
            }),
            rhs: boxed_expr(WithRange(
                LiteralExpression::Number(1.0),
                line_range(10, 11)
            ))
        }
        .into())
    );
    assert_eq!(
        parse("1 is as small as 1 is bigger than 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Greater,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::LessEq,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
                rhs: boxed_expr(WithRange(
                    LiteralExpression::Number(1.0),
                    line_range(17, 18)
                ))
            }),
            rhs: boxed_expr(WithRange(
                LiteralExpression::Number(1.0),
                line_range(34, 35)
            ))
        }
        .into())
    );
}

#[test]
fn parse_logical_expression() {
    let parse = |text| Parser::for_source_code(text).parse_expression();

    assert_eq!(
        parse("1 and 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::And,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(6, 7)))
        }
        .into())
    );
    assert_eq!(
        parse("1 or 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Or,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(5, 6)))
        }
        .into())
    );
    assert_eq!(
        parse("1 nor 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Nor,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(6, 7)))
        }
        .into())
    );
    assert_eq!(
        parse("1 and 1 or 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Or,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::And,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
                rhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(6, 7)))
            }),
            rhs: boxed_expr(WithRange(
                LiteralExpression::Number(1.0),
                line_range(11, 12)
            )),
        }
        .into())
    );
    assert_eq!(
        parse("1 and 1 < 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::And,
            lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(0, 1))),
            rhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Less,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(6, 7))),
                rhs: boxed_expr(WithRange(
                    LiteralExpression::Number(1.0),
                    line_range(10, 11)
                ))
            }),
        }
        .into())
    );
}

#[test]
fn parse_identifier() {
    let parse = |text| Parser::for_source_code(text).parse_identifier();
    assert_eq!(parse(""), Ok(None));
    assert_eq!(parse("1"), Ok(None));

    assert_eq!(
        parse("my heart"),
        Ok(Some(
            WithRange(
                CommonIdentifier("my".into(), "heart".into()),
                line_range(0, 8)
            )
            .into()
        ))
    );
    assert_eq!(
        parse("my  heart"),
        Ok(Some(
            WithRange(
                CommonIdentifier("my".into(), "heart".into()),
                line_range(0, 9)
            )
            .into()
        ))
    );
    assert_eq!(
        parse("your heart"),
        Ok(Some(
            WithRange(
                CommonIdentifier("your".into(), "heart".into()),
                line_range(0, 10)
            )
            .into()
        ))
    );
    assert_eq!(
        parse("My heart"),
        Ok(Some(
            WithRange(
                CommonIdentifier("My".into(), "heart".into()),
                line_range(0, 8)
            )
            .into()
        ))
    );
    assert_eq!(
        parse("Your heart"),
        Ok(Some(
            WithRange(
                CommonIdentifier("Your".into(), "heart".into()),
                line_range(0, 10)
            )
            .into()
        ))
    );

    assert_eq!(
        parse("Billie Jean"),
        Ok(Some(
            WithRange(
                ProperIdentifier(vec!["Billie".into(), "Jean".into()]),
                line_range(0, 11)
            )
            .into()
        ))
    );
    assert_eq!(
        // parse("Distance In KM"), // this example is from the official spec, but I think it's broken: 'in' is a language keyword
        parse("Distance Out KM"),
        Ok(Some(
            // ProperIdentifier(vec!["Distance".into(), "In".into(), "KM".into()]).into()
            WithRange(
                ProperIdentifier(vec!["Distance".into(), "Out".into(), "KM".into()]),
                line_range(0, 15)
            )
            .into()
        ))
    );
    assert_eq!(
        parse("Tom Sawyer"),
        Ok(Some(
            WithRange(
                ProperIdentifier(vec!["Tom".into(), "Sawyer".into()]),
                line_range(0, 10)
            )
            .into()
        ))
    );
    assert_eq!(
        parse("TOM SAWYER"),
        Ok(Some(
            WithRange(
                ProperIdentifier(vec!["TOM".into(), "SAWYER".into()]),
                line_range(0, 10)
            )
            .into()
        ))
    );
    assert_eq!(
        parse("TOm SAWyer"),
        Ok(Some(
            WithRange(
                ProperIdentifier(vec!["TOm".into(), "SAWyer".into()]),
                line_range(0, 10)
            )
            .into()
        ))
    );
}
#[test]

fn parse_identifier_errors() {
    let parse = |text| Parser::for_source_code(text).parse_identifier();
    assert_eq!(
        parse("DOCTOR feelgood"),
        Ok(Some(
            WithRange(SimpleIdentifier("DOCTOR".into()), line_range(0, 6)).into()
        )) // TODO ParseError here?
    );

    assert_eq!(
        parse("my"),
        Err(ParseError::new(
            ParseErrorCode::MissingIDAfterCommonPrefix("my".into()),
            1.into()
        ))
    );
    assert_eq!(
        parse("your"),
        Err(ParseError::new(
            ParseErrorCode::MissingIDAfterCommonPrefix("your".into()),
            1.into()
        ))
    );

    assert_eq!(
        parse("my Heart"),
        Err(ParseError::new(
            ParseErrorCode::UppercaseAfterCommonPrefix("my".into(), "Heart".into()).into(),
            1.into()
        ))
    );
    assert_eq!(
        parse("your Heart"),
        Err(ParseError::new(
            ParseErrorCode::UppercaseAfterCommonPrefix("your".into(), "Heart".into()).into(),
            1.into()
        ))
    );
    assert_eq!(
        parse("My Heart"),
        Err(ParseError::new(
            ParseErrorCode::UppercaseAfterCommonPrefix("My".into(), "Heart".into()).into(),
            1.into()
        ))
    );
    assert_eq!(
        parse("Your Heart"),
        Err(ParseError::new(
            ParseErrorCode::UppercaseAfterCommonPrefix("Your".into(), "Heart".into()).into(),
            1.into()
        ))
    );
    assert_eq!(
        parse("your heArt"),
        Err(ParseError::new(
            ParseErrorCode::UppercaseAfterCommonPrefix("your".into(), "heArt".into()).into(),
            1.into()
        ))
    );
}

#[test]
fn parse_pronoun() {
    for p in [
        "it", "he", "she", "him", "her", "they", "them", "ze", "hir", "zie", "zir", "xe", "xem",
        "ve", "ver",
    ] {
        assert_eq!(
            Parser::for_source_code(p).parse_pronoun(),
            Some(WithRange(Identifier::Pronoun, line_range(0, p.len() as u32)).into())
        );
    }
}

#[test]
fn parse_expression() {
    let parse = |text| Parser::for_source_code(text).parse_expression();

    assert_eq!(
        parse("my heart * your heart"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Multiply,
            lhs: boxed_expr(WithRange(
                CommonIdentifier("my".into(), "heart".into()),
                line_range(0, 8)
            )),
            rhs: boxed_expr(WithRange(
                CommonIdentifier("your".into(), "heart".into()),
                line_range(11, 21)
            ))
        }
        .into())
    );
}

#[test]
fn parse_assignment() {
    let parse = |text| {
        Parser::for_source_code(text)
            .parse_statement()
            .map(|s| s.unwrap())
    };
    assert_eq!(
        parse("Put x plus y into result"),
        Ok(Assignment {
            dest: WithRange(SimpleIdentifier("result".into()), line_range(18, 24)).into(),
            value: BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(WithRange(SimpleIdentifier("x".into()), line_range(4, 5))),
                rhs: boxed_expr(WithRange(SimpleIdentifier("y".into()), line_range(11, 12)))
            }
            .into(),
            operator: None
        }
        .into())
    );

    assert_eq!(
        parse("Put 123 into X"),
        Ok(Assignment {
            dest: WithRange(SimpleIdentifier("X".into()), line_range(13, 14)).into(),
            value: WithRange(LiteralExpression::Number(123.0), line_range(4, 7)).into(),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Put \"Hello San Francisco\" into the message"),
        Ok(Assignment {
            dest: WithRange(
                CommonIdentifier("the".into(), "message".into()),
                line_range(31, 42)
            )
            .into(),
            value: WithRange(
                LiteralExpression::String("Hello San Francisco".into()),
                line_range(4, 25) // includes quotes
            )
            .into(),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Let my balance be 1000000"),
        Ok(Assignment {
            dest: WithRange(
                CommonIdentifier("my".into(), "balance".into()),
                line_range(4, 14)
            )
            .into(),
            value: WithRange(LiteralExpression::Number(1000000.0), line_range(18, 25)).into(),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Let the survivors be the brave without the fallen"),
        Ok(Assignment {
            dest: WithRange(
                CommonIdentifier("the".into(), "survivors".into()),
                line_range(4, 17)
            )
            .into(),
            value: BinaryExpression {
                operator: BinaryOperator::Minus,
                lhs: boxed_expr(WithRange(
                    CommonIdentifier("the".into(), "brave".into()),
                    line_range(21, 30)
                )),
                rhs: boxed_expr(WithRange(
                    CommonIdentifier("the".into(), "fallen".into()),
                    line_range(39, 49)
                ))
            }
            .into(),
            operator: None
        }
        .into())
    );

    assert_eq!(
        parse("Let X be with 10"),
        Ok(Assignment {
            dest: WithRange(SimpleIdentifier("X".into()), line_range(4, 5)).into(),
            value: WithRange(LiteralExpression::Number(10.0), line_range(14, 16)).into(),
            operator: Some(BinaryOperator::Plus),
        }
        .into())
    );
    assert_eq!(
        parse("Let the children be without fear"),
        Ok(Assignment {
            dest: WithRange(
                CommonIdentifier("the".into(), "children".into()),
                line_range(4, 16)
            )
            .into(),
            value: WithRange(SimpleIdentifier("fear".into()), line_range(28, 32)).into(),
            operator: Some(BinaryOperator::Minus),
        }
        .into())
    );
    assert_eq!(
        parse("Let my heart be over the moon"),
        Ok(Assignment {
            dest: WithRange(
                CommonIdentifier("my".into(), "heart".into()),
                line_range(4, 12)
            )
            .into(),
            value: WithRange(
                CommonIdentifier("the".into(), "moon".into()),
                line_range(21, 29)
            )
            .into(),
            operator: Some(BinaryOperator::Divide),
        }
        .into())
    );

    assert_eq!(
        parse("Put the whole of your heart into my hands"),
        Ok(Assignment {
            dest: WithRange(
                CommonIdentifier("my".into(), "hands".into()),
                line_range(33, 41)
            )
            .into(),
            value: BinaryExpression {
                operator: BinaryOperator::Multiply,
                lhs: boxed_expr(WithRange(
                    CommonIdentifier("the".into(), "whole".into()),
                    line_range(4, 13)
                )),
                rhs: boxed_expr(WithRange(
                    CommonIdentifier("your".into(), "heart".into()),
                    line_range(17, 27)
                ))
            }
            .into(),
            operator: None
        }
        .into())
    );

    assert_eq!(
        parse("Let X be 1 with 2, 3, 4"),
        Ok(Assignment {
            dest: WithRange(SimpleIdentifier("X".into()), line_range(4, 5)).into(),
            value: BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(9, 10))),
                rhs: boxed_expr(ExpressionList {
                    first: WithRange(LiteralExpression::Number(2.0), line_range(16, 17)).into(),
                    rest: vec![
                        WithRange(LiteralExpression::Number(3.0), line_range(19, 20)).into(),
                        WithRange(LiteralExpression::Number(4.0), line_range(22, 23)).into()
                    ]
                })
            }
            .into(),
            operator: None,
        }
        .into())
    );
    assert_eq!(
        parse("Let X be 1 with 2, 3, and 4"),
        Ok(Assignment {
            dest: WithRange(SimpleIdentifier("X".into()), line_range(4, 5)).into(),
            value: BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(WithRange(LiteralExpression::Number(1.0), line_range(9, 10))),
                rhs: boxed_expr(ExpressionList {
                    first: WithRange(LiteralExpression::Number(2.0), line_range(16, 17)).into(),
                    rest: vec![
                        WithRange(LiteralExpression::Number(3.0), line_range(19, 20)).into(),
                        WithRange(LiteralExpression::Number(4.0), line_range(26, 27)).into()
                    ]
                })
            }
            .into(),
            operator: None,
        }
        .into())
    );
    assert_eq!(
        parse("Let X be with 2, 3, 4"),
        Ok(Assignment {
            dest: WithRange(SimpleIdentifier("X".into()), line_range(4, 5)).into(),
            value: ExpressionList {
                first: WithRange(LiteralExpression::Number(2.0), line_range(14, 15)).into(),
                rest: vec![
                    WithRange(LiteralExpression::Number(3.0), line_range(17, 18)).into(),
                    WithRange(LiteralExpression::Number(4.0), line_range(20, 21)).into()
                ]
            }
            .into(),
            operator: Some(BinaryOperator::Plus),
        }
        .into())
    );

    assert_eq!(
        parse("Let my array at 255 be \"some value\""),
        Ok(Assignment {
            dest: ArraySubscript {
                array: boxed_expr(WithRange(
                    CommonIdentifier("my".into(), "array".into()),
                    line_range(4, 12)
                )),
                subscript: boxed_expr(WithRange(
                    LiteralExpression::Number(255.0),
                    line_range(16, 19)
                ))
            }
            .into(),
            value: WithRange(
                LiteralExpression::String("some value".into()),
                line_range(23, 35)
            )
            .into(),
            operator: None,
        }
        .into())
    );
    assert_eq!(
        parse("Let my array at 255 be my array at 255"),
        Ok(Assignment {
            dest: ArraySubscript {
                array: boxed_expr(WithRange(
                    CommonIdentifier("my".into(), "array".into()),
                    line_range(4, 12)
                )),
                subscript: boxed_expr(WithRange(
                    LiteralExpression::Number(255.0),
                    line_range(16, 19)
                ))
            }
            .into(),
            value: ArraySubscript {
                array: boxed_expr(WithRange(
                    CommonIdentifier("my".into(), "array".into()),
                    line_range(23, 31)
                )),
                subscript: boxed_expr(WithRange(
                    LiteralExpression::Number(255.0),
                    line_range(35, 38)
                ))
            }
            .into(),
            operator: None,
        }
        .into())
    );
}

#[test]
#[ignore = "this doesn't currently pass"]
fn roll_in_assignments() {
    let parse = |text| {
        Parser::for_source_code(text)
            .parse_statement()
            .map(|s| s.unwrap())
    };
    assert!(parse("let the first be roll ints").is_ok());
}

#[test]
fn array_subscript_left_associativity() {
    let parse = |text| Parser::for_source_code(text).parse_expression();
    assert_eq!(
        parse("x at y at z"),
        Ok(ArraySubscript {
            array: boxed_expr(ArraySubscript {
                array: boxed_expr(WithRange(SimpleIdentifier("x".into()), line_range(0, 1))),
                subscript: boxed_expr(WithRange(SimpleIdentifier("y".into()), line_range(5, 6))),
            }),
            subscript: boxed_expr(WithRange(SimpleIdentifier("z".into()), line_range(10, 11))),
        }
        .into())
    )
}

#[test]
fn parse_assignment_errors() {
    let parse = |text| {
        Parser::for_source_code(text)
            .parse_statement()
            .map(|s| s.unwrap())
    };
    assert_eq!(
        parse("Let 5 be 6"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedIdentifier,
            loc: Token::new(TokenType::Number(5.0), "5", line_range(4, 5)).into()
        })
    );
    assert_eq!(
        parse("Put 6 into 5"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedIdentifier,
            loc: Token::new(TokenType::Number(5.0), "5", line_range(11, 12)).into()
        })
    );
    assert_eq!(
        parse("Let five bee 6"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedToken(TokenType::Be),
            loc: Token::new(TokenType::Word, "bee", line_range(9, 12)).into()
        })
    );
    assert_eq!(
        parse("Put five intoo six"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedToken(TokenType::Into),
            loc: Token::new(TokenType::Word, "intoo", line_range(9, 14)).into()
        })
    );

    assert_eq!(
        parse("Put 1, 2, 3 into six"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedToken(TokenType::Into),
            loc: Token::new(TokenType::Comma, ",", line_range(5, 6)).into()
        })
    );
}

#[test]
fn parse_poetic_assignment() {
    let parse = |text| {
        Parser::for_source_code(text)
            .parse_statement()
            .map(|s| s.unwrap())
    };
    assert_eq!(
        parse("Variable is 1"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("Variable".into()), line_range(0, 8)).into(),
            rhs: WithRange(LiteralExpression::Number(1.0), line_range(12, 13)).into(),
        }
        .into())
    );
    assert_eq!(
        parse("Variable at 1 is 1"),
        Ok(PoeticNumberAssignment {
            dest: ArraySubscript {
                array: boxed_expr(WithRange(
                    SimpleIdentifier("Variable".into()),
                    line_range(0, 8)
                )),
                subscript: boxed_expr(WithRange(
                    LiteralExpression::Number(1.0),
                    line_range(12, 13)
                )),
            }
            .into(),
            rhs: WithRange(LiteralExpression::Number(1.0), line_range(17, 18)).into(),
        }
        .into())
    );
    assert_eq!(
        parse("Tommy is a rockstar"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("Tommy".into()), line_range(0, 5)).into(),
            rhs: PoeticNumberLiteral {
                elems: vec![
                    PoeticNumberLiteralElem::Word("a".into()),
                    PoeticNumberLiteralElem::Word("rockstar".into()),
                ]
            }
            .into(),
        }
        .into())
    );
    assert_eq!(
        parse("Tommy is a rockstar's rockstar"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("Tommy".into()), line_range(0, 5)).into(),
            rhs: PoeticNumberLiteral {
                elems: vec![
                    PoeticNumberLiteralElem::Word("a".into()),
                    PoeticNumberLiteralElem::Word("rockstar".into()),
                    PoeticNumberLiteralElem::WordSuffix("'s".into()),
                    PoeticNumberLiteralElem::Word("rockstar".into()),
                ]
            }
            .into(),
        }
        .into())
    );
    assert_eq!(
        parse("Sweet Lucy was a dancer"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(
                ProperIdentifier(vec!["Sweet".into(), "Lucy".into()]),
                line_range(0, 10)
            )
            .into(),
            rhs: PoeticNumberLiteral {
                elems: vec![
                    PoeticNumberLiteralElem::Word("a".into()),
                    PoeticNumberLiteralElem::Word("dancer".into()),
                ]
            }
            .into(),
        }
        .into())
    );
    assert_eq!(
        parse("Tommy was without"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("Tommy".into()), line_range(0, 5)).into(),
            rhs: PoeticNumberLiteral {
                elems: vec![PoeticNumberLiteralElem::Word("without".into())]
            }
            .into(),
        }
        .into())
    );
    assert_eq!(
        parse("Tommy was hunky-dory"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("Tommy".into()), line_range(0, 5)).into(),
            rhs: PoeticNumberLiteral {
                elems: vec![
                    PoeticNumberLiteralElem::Word("hunky".into()),
                    PoeticNumberLiteralElem::WordSuffix("-dory".into())
                ]
            }
            .into(),
        }
        .into())
    );
    // the spec isn't crystal clear, but I'm allowing whitespace around hypens because it simplifies the implementation
    assert_eq!(
        parse("Tommy was hunky - dory"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("Tommy".into()), line_range(0, 5)).into(),
            rhs: PoeticNumberLiteral {
                elems: vec![
                    PoeticNumberLiteralElem::Word("hunky".into()),
                    PoeticNumberLiteralElem::WordSuffix("-dory".into()) // the suffix still has no whitespace
                ]
            }
            .into(),
        }
        .into())
    );
    assert_eq!(
        parse("Tommy was hunky-dory-dory"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("Tommy".into()), line_range(0, 5)).into(),
            rhs: PoeticNumberLiteral {
                elems: vec![
                    PoeticNumberLiteralElem::Word("hunky".into()),
                    PoeticNumberLiteralElem::WordSuffix("-dory".into()),
                    PoeticNumberLiteralElem::WordSuffix("-dory".into())
                ]
            }
            .into(),
        }
        .into())
    );
    assert_eq!(
        parse("Tommy was a mommy's-boy"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("Tommy".into()), line_range(0, 5)).into(),
            rhs: PoeticNumberLiteral {
                elems: vec![
                    PoeticNumberLiteralElem::Word("a".into()),
                    PoeticNumberLiteralElem::Word("mommy".into()),
                    PoeticNumberLiteralElem::WordSuffix("'s".into()),
                    PoeticNumberLiteralElem::WordSuffix("-boy".into()),
                ]
            }
            .into(),
        }
        .into())
    );
    assert_eq!(
        parse("Tommy was Obi-Wan's pal"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("Tommy".into()), line_range(0, 5)).into(),
            rhs: PoeticNumberLiteral {
                elems: vec![
                    PoeticNumberLiteralElem::Word("Obi".into()),
                    PoeticNumberLiteralElem::WordSuffix("-Wan".into()),
                    PoeticNumberLiteralElem::WordSuffix("'s".into()),
                    PoeticNumberLiteralElem::Word("pal".into()),
                ]
            }
            .into(),
        }
        .into())
    );
    assert_eq!(
        parse("X is 2"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("X".into()), line_range(0, 1)).into(),
            rhs: WithRange(LiteralExpression::Number(2.0), line_range(5, 6)).into(),
        }
        .into())
    );
    assert_eq!(
        parse("Y is 3"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("Y".into()), line_range(0, 1)).into(),
            rhs: WithRange(LiteralExpression::Number(3.0), line_range(5, 6)).into(),
        }
        .into())
    );
    assert_eq!(
        parse("noise is silence"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("noise".into()), line_range(0, 5)).into(),
            rhs: WithRange(LiteralExpression::String(String::new()), line_range(9, 16)).into(),
        }
        .into())
    );

    assert_eq!(
        parse("Variable's 1"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("Variable".into()), line_range(0, 8)).into(),
            rhs: WithRange(LiteralExpression::Number(1.0), line_range(11, 12)).into(),
        }
        .into())
    );
    assert_eq!(
        parse("Variables are 1"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("Variables".into()), line_range(0, 9)).into(),
            rhs: WithRange(LiteralExpression::Number(1.0), line_range(14, 15)).into(),
        }
        .into())
    );
    assert_eq!(
        parse("We're 1"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(SimpleIdentifier("We".into()), line_range(0, 2)).into(),
            rhs: WithRange(LiteralExpression::Number(1.0), line_range(6, 7)).into(),
        }
        .into())
    );
    assert_eq!(
        parse("My world is nothing without your love"),
        Ok(PoeticNumberAssignment {
            dest: WithRange(
                CommonIdentifier("My".into(), "world".into()),
                line_range(0, 8)
            )
            .into(),
            rhs: BinaryExpression {
                operator: BinaryOperator::Minus,
                lhs: boxed_expr(WithRange(LiteralExpression::Null, line_range(12, 19))),
                rhs: boxed_expr(WithRange(
                    CommonIdentifier("your".into(), "love".into()),
                    line_range(28, 37)
                ))
            }
            .into(),
        }
        .into())
    );
}

#[test]
fn parse_poetic_string_assignment() {
    let parse = |text| {
        Parser::for_source_code(text)
            .parse_statement()
            .map(|s| s.unwrap())
    };

    assert_eq!(
        parse("Peter says Hello San Francisco!\n"),
        Ok(PoeticStringAssignment {
            dest: WithRange(SimpleIdentifier("Peter".into()), line_range(0, 5)).into(),
            rhs: "Hello San Francisco!".into(),
        }
        .into())
    );
    assert_eq!(
        parse("Peter says Hello San Francisco!"),
        Ok(PoeticStringAssignment {
            dest: WithRange(SimpleIdentifier("Peter".into()), line_range(0, 5)).into(),
            rhs: "Hello San Francisco!".into(),
        }
        .into())
    );
    assert_eq!(
        parse("Peter says    Hello    San    Francisco!    \n"),
        Ok(PoeticStringAssignment {
            dest: WithRange(SimpleIdentifier("Peter".into()), line_range(0, 5)).into(),
            rhs: "   Hello    San    Francisco!    ".into(),
        }
        .into())
    );
    assert_eq!(
        parse("Peter says    Hello    San    Francisco!    "),
        Ok(PoeticStringAssignment {
            dest: WithRange(SimpleIdentifier("Peter".into()), line_range(0, 5)).into(),
            rhs: "   Hello    San    Francisco!    ".into(),
        }
        .into())
    );

    assert_eq!(
        parse("Peter say Hello San Francisco!\n"),
        Ok(PoeticStringAssignment {
            dest: WithRange(SimpleIdentifier("Peter".into()), line_range(0, 5)).into(),
            rhs: "Hello San Francisco!".into(),
        }
        .into())
    );
    assert_eq!(
        parse("Peter say Hello San Francisco!"),
        Ok(PoeticStringAssignment {
            dest: WithRange(SimpleIdentifier("Peter".into()), line_range(0, 5)).into(),
            rhs: "Hello San Francisco!".into(),
        }
        .into())
    );
    assert_eq!(
        parse("Peter say    Hello    San    Francisco!    \n"),
        Ok(PoeticStringAssignment {
            dest: WithRange(SimpleIdentifier("Peter".into()), line_range(0, 5)).into(),
            rhs: "   Hello    San    Francisco!    ".into(),
        }
        .into())
    );
    assert_eq!(
        parse("Peter say    Hello    San    Francisco!    "),
        Ok(PoeticStringAssignment {
            dest: WithRange(SimpleIdentifier("Peter".into()), line_range(0, 5)).into(),
            rhs: "   Hello    San    Francisco!    ".into(),
        }
        .into())
    );

    // I think this is technically allowed?
    assert_eq!(
        parse("Peter says "),
        Ok(PoeticStringAssignment {
            dest: WithRange(SimpleIdentifier("Peter".into()), line_range(0, 5)).into(),
            rhs: "".into(),
        }
        .into())
    );

    assert_eq!(
        parse("My parents said we'd never make it\n"),
        Ok(PoeticStringAssignment {
            dest: WithRange(
                CommonIdentifier("My".into(), "parents".into()),
                line_range(0, 10)
            )
            .into(),
            rhs: "we'd never make it".into(),
        }
        .into())
    );
}

#[test]
fn parse_poetic_assignment_errors() {
    let parse = |text| {
        Parser::for_source_code(text)
            .parse_statement()
            .map(|s| s.unwrap())
    };

    assert_eq!(
        parse("My world. is nothing without your love"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedOneOfTokens(vec![
                TokenType::Is,
                TokenType::ApostropheS,
                TokenType::ApostropheRE,
                TokenType::Says,
                TokenType::Say,
            ]),
            loc: Token::new(TokenType::Dot, ".", line_range(8, 9)).into()
        })
    );

    assert_eq!(
        parse("My world whisper nothing without your love"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedOneOfTokens(vec![
                TokenType::Is,
                TokenType::ApostropheS,
                TokenType::ApostropheRE,
                TokenType::Says,
                TokenType::Say,
            ]),
            loc: Token::new(TokenType::SayAlias, "whisper", line_range(9, 16)).into()
        })
    );

    assert_eq!(
        parse("My world is without-"),
        Err(ParseError {
            code: ParseErrorCode::UnexpectedEndOfTokens,
            loc: 1.into()
        })
    );
    assert_eq!(
        parse("My world is without--"),
        Err(ParseError {
            code: ParseErrorCode::UnexpectedToken,
            loc: Token::new(TokenType::Minus, "-", line_range(20, 21)).into()
        })
    );

    assert_eq!(
        parse("My world said"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedSpaceAfterSays(Token::new(
                TokenType::Says,
                "said",
                line_range(9, 13)
            )),
            loc: 1.into()
        })
    );
    assert_eq!(
        parse("My world said\n"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedSpaceAfterSays(Token::new(
                TokenType::Says,
                "said",
                line_range(9, 13)
            )),
            loc: Token::new(TokenType::Newline, "\n", line_range(13, 14)).into()
        })
    );
}

#[test]
fn poetic_number_literal_compute_value() {
    let val = |text| {
        inner!(
            inner!(
                inner!(
                    Parser::for_source_code(text).parse_statement_starting_with_word().unwrap(),
                    if Statement::PoeticAssignment),
                if PoeticAssignment::Number
            ).rhs,
            if PoeticNumberAssignmentRHS::PoeticNumberLiteral
        )
        .compute_value()
    };

    assert_eq!(val("Tommy was a big bad brother"), 1337.0);
    assert_eq!(val("Tommy was a big bad brother."), 1337.0);
    assert_eq!(val("Tommy is a rockstar!"), 18.0);
    assert_eq!(val("Tommy is a rockstar's rockstar!"), 108.0);
    assert_eq!(val("Tommy was a lovestruck ladykiller"), 100.0);
    assert_eq!(val("Tommy was a dancer"), 16.0);
    assert_eq!(val("Tommy is on the loose"), 235.0);
    assert_eq!(
        val("Tommy were ice. A life unfulfilled; wakin' everybody up, taking booze and pills"),
        3.1415926535
    );
    assert_eq!(
        val("Tommy were ice... A. life .. ...unfulfilled;....;;;''''' wakin' .everybody. .up, ..taking booze ....and pills......"),
        3.1415926535
    );
    assert_eq!(val("Tommy was without"), 7.0);
    assert_eq!(val("Tommy was hunky-dory"), 0.0);
    assert_eq!(val("Tommy was hunky-dory-dory"), 5.0);
    assert_eq!(val("Tommy was hunky-dory-dory-dor"), 9.0);
    assert_eq!(val("Tommy was a mommy's-boy"), 11.0);
    assert_eq!(val("Tommy was Obi-Wan's pal"), 93.0);
}

fn range_on_line(line: u32, range: (u32, u32)) -> SourceRange {
    ((line, range.0), (line, range.1)).into()
}

#[test]
fn parse_block() {
    let parse = |text| Parser::for_source_code(text).parse_block();
    assert_eq!(parse("\n"), Ok(Block::Empty((1, 0).into())));

    assert_eq!(
        parse(
            "\
Variable is 1
Tommy is a rockstar"
        ),
        Ok(Block::NonEmpty(vec![
            PoeticNumberAssignment {
                dest: WithRange(
                    SimpleIdentifier("Variable".into()),
                    range_on_line(1, (0, 8))
                )
                .into(),
                rhs: WithRange(LiteralExpression::Number(1.0), range_on_line(1, (12, 13))).into()
            }
            .into(),
            PoeticNumberAssignment {
                dest: WithRange(SimpleIdentifier("Tommy".into()), range_on_line(2, (0, 5))).into(),
                rhs: PoeticNumberLiteral {
                    elems: vec![
                        PoeticNumberLiteralElem::Word("a".into()),
                        PoeticNumberLiteralElem::Word("rockstar".into()),
                    ]
                }
                .into(),
            }
            .into()
        ]))
    );
    assert_eq!(
        parse(
            "\
Variable is 1
Tommy is a rockstar
            "
        ),
        Ok(Block::NonEmpty(vec![
            PoeticNumberAssignment {
                dest: WithRange(
                    SimpleIdentifier("Variable".into()),
                    range_on_line(1, (0, 8))
                )
                .into(),
                rhs: WithRange(LiteralExpression::Number(1.0), range_on_line(1, (12, 13))).into()
            }
            .into(),
            PoeticNumberAssignment {
                dest: WithRange(SimpleIdentifier("Tommy".into()), range_on_line(2, (0, 5))).into(),
                rhs: PoeticNumberLiteral {
                    elems: vec![
                        PoeticNumberLiteralElem::Word("a".into()),
                        PoeticNumberLiteralElem::Word("rockstar".into()),
                    ]
                }
                .into(),
            }
            .into()
        ]))
    );

    assert_eq!(
        parse(
            "\
Variable is 1
Tommy is a rockstar
            
            "
        ),
        Ok(Block::NonEmpty(vec![
            PoeticNumberAssignment {
                dest: WithRange(
                    SimpleIdentifier("Variable".into()),
                    range_on_line(1, (0, 8))
                )
                .into(),
                rhs: WithRange(LiteralExpression::Number(1.0), range_on_line(1, (12, 13))).into()
            }
            .into(),
            PoeticNumberAssignment {
                dest: WithRange(SimpleIdentifier("Tommy".into()), range_on_line(2, (0, 5))).into(),
                rhs: PoeticNumberLiteral {
                    elems: vec![
                        PoeticNumberLiteralElem::Word("a".into()),
                        PoeticNumberLiteralElem::Word("rockstar".into()),
                    ]
                }
                .into(),
            }
            .into()
        ]))
    );
}

#[test]
fn parse_if_statement() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(
        parse(
            "\
if x is 5,
x is 6
        "
        ),
        Ok(Some(
            If {
                condition: BinaryExpression {
                    operator: BinaryOperator::Eq,
                    lhs: boxed_expr(WithRange(
                        SimpleIdentifier("x".into()),
                        range_on_line(1, (3, 4))
                    )),
                    rhs: boxed_expr(WithRange(
                        LiteralExpression::Number(5.0),
                        range_on_line(1, (8, 9))
                    ))
                }
                .into(),
                then_block: Block::NonEmpty(vec![PoeticNumberAssignment {
                    dest: WithRange(SimpleIdentifier("x".into()), range_on_line(2, (0, 1))).into(),
                    rhs: WithRange(LiteralExpression::Number(6.0), range_on_line(2, (5, 6))).into()
                }
                .into()]),
                else_block: None,
            }
            .into()
        ))
    );
    assert_eq!(
        parse(
            "\
if x is 5,
x is 6
else
x is 7
        "
        ),
        Ok(Some(
            If {
                condition: BinaryExpression {
                    operator: BinaryOperator::Eq,
                    lhs: boxed_expr(WithRange(
                        SimpleIdentifier("x".into()),
                        range_on_line(1, (3, 4))
                    )),
                    rhs: boxed_expr(WithRange(
                        LiteralExpression::Number(5.0),
                        range_on_line(1, (8, 9))
                    ))
                }
                .into(),
                then_block: Block::NonEmpty(vec![PoeticNumberAssignment {
                    dest: WithRange(SimpleIdentifier("x".into()), range_on_line(2, (0, 1))).into(),
                    rhs: WithRange(LiteralExpression::Number(6.0), range_on_line(2, (5, 6))).into()
                }
                .into()]),
                else_block: Some(Block::NonEmpty(vec![PoeticNumberAssignment {
                    dest: WithRange(SimpleIdentifier("x".into()), range_on_line(4, (0, 1))).into(),
                    rhs: WithRange(LiteralExpression::Number(7.0), range_on_line(4, (5, 6))).into()
                }
                .into()])),
            }
            .into()
        ))
    );

    assert_eq!(
        parse(
            "\
if x is 5
else
x is 7"
        ),
        Ok(Some(
            If {
                condition: BinaryExpression {
                    operator: BinaryOperator::Eq,
                    lhs: boxed_expr(WithRange(
                        SimpleIdentifier("x".into()),
                        range_on_line(1, (3, 4))
                    )),
                    rhs: boxed_expr(WithRange(
                        LiteralExpression::Number(5.0),
                        range_on_line(1, (8, 9))
                    ))
                }
                .into(),
                then_block: Block::Empty((2, 0).into()),
                else_block: Some(Block::NonEmpty(vec![PoeticNumberAssignment {
                    dest: WithRange(SimpleIdentifier("x".into()), range_on_line(3, (0, 1))).into(),
                    rhs: WithRange(LiteralExpression::Number(7.0), range_on_line(3, (5, 6))).into()
                }
                .into()])),
            }
            .into()
        ))
    );

    assert_eq!(
        parse(
            "\
if x is 5
else"
        ),
        Ok(Some(
            If {
                condition: BinaryExpression {
                    operator: BinaryOperator::Eq,
                    lhs: boxed_expr(WithRange(
                        SimpleIdentifier("x".into()),
                        range_on_line(1, (3, 4))
                    )),
                    rhs: boxed_expr(WithRange(
                        LiteralExpression::Number(5.0),
                        range_on_line(1, (8, 9))
                    ))
                }
                .into(),
                then_block: Block::Empty((2, 0).into()),
                else_block: Some(Block::Empty((2, 4).into())),
            }
            .into()
        ))
    );
}

#[test]
fn parse_loop() {
    // kind of testing parse_block in here too
    let parse = |text| Parser::for_source_code(text).parse_block();

    assert_eq!(
        parse(
            "\
while x is 5,
x is 6

until x is 5,
x is 6
        "
        ),
        Ok(Block::NonEmpty(vec![
            While {
                condition: BinaryExpression {
                    operator: BinaryOperator::Eq,
                    lhs: boxed_expr(WithRange(
                        SimpleIdentifier("x".into()),
                        range_on_line(1, (6, 7))
                    )),
                    rhs: boxed_expr(WithRange(
                        LiteralExpression::Number(5.0),
                        range_on_line(1, (11, 12))
                    ))
                }
                .into(),
                block: Block::NonEmpty(vec![PoeticNumberAssignment {
                    dest: WithRange(SimpleIdentifier("x".into()), range_on_line(2, (0, 1))).into(),
                    rhs: WithRange(LiteralExpression::Number(6.0), range_on_line(2, (5, 6))).into()
                }
                .into()])
            }
            .into(),
            Until {
                condition: BinaryExpression {
                    operator: BinaryOperator::Eq,
                    lhs: boxed_expr(WithRange(
                        SimpleIdentifier("x".into()),
                        range_on_line(4, (6, 7))
                    )),
                    rhs: boxed_expr(WithRange(
                        LiteralExpression::Number(5.0),
                        range_on_line(4, (11, 12))
                    ))
                }
                .into(),
                block: Block::NonEmpty(vec![PoeticNumberAssignment {
                    dest: WithRange(SimpleIdentifier("x".into()), range_on_line(5, (0, 1))).into(),
                    rhs: WithRange(LiteralExpression::Number(6.0), range_on_line(5, (5, 6))).into()
                }
                .into()])
            }
            .into()
        ]))
    );
    assert_eq!(
        parse(
            "\
while x is 5,
x is 6
(end loop)
until x is 5,
x is 6
        "
        ),
        Ok(Block::NonEmpty(vec![
            While {
                condition: BinaryExpression {
                    operator: BinaryOperator::Eq,
                    lhs: boxed_expr(WithRange(
                        SimpleIdentifier("x".into()),
                        range_on_line(1, (6, 7))
                    )),
                    rhs: boxed_expr(WithRange(
                        LiteralExpression::Number(5.0),
                        range_on_line(1, (11, 12))
                    ))
                }
                .into(),
                block: Block::NonEmpty(vec![PoeticNumberAssignment {
                    dest: WithRange(SimpleIdentifier("x".into()), range_on_line(2, (0, 1))).into(),
                    rhs: WithRange(LiteralExpression::Number(6.0), range_on_line(2, (5, 6))).into()
                }
                .into()])
            }
            .into(),
            Until {
                condition: BinaryExpression {
                    operator: BinaryOperator::Eq,
                    lhs: boxed_expr(WithRange(
                        SimpleIdentifier("x".into()),
                        range_on_line(4, (6, 7))
                    )),
                    rhs: boxed_expr(WithRange(
                        LiteralExpression::Number(5.0),
                        range_on_line(4, (11, 12))
                    ))
                }
                .into(),
                block: Block::NonEmpty(vec![PoeticNumberAssignment {
                    dest: WithRange(SimpleIdentifier("x".into()), range_on_line(5, (0, 1))).into(),
                    rhs: WithRange(LiteralExpression::Number(6.0), range_on_line(5, (5, 6))).into()
                }
                .into()])
            }
            .into()
        ]))
    );

    assert_eq!(
        parse(
            "\
while x is 5,
while y is 6,
y is 7

x is 6
        "
        ),
        Ok(Block::NonEmpty(vec![While {
            condition: BinaryExpression {
                operator: BinaryOperator::Eq,
                lhs: boxed_expr(WithRange(
                    SimpleIdentifier("x".into()),
                    range_on_line(1, (6, 7))
                )),
                rhs: boxed_expr(WithRange(
                    LiteralExpression::Number(5.0),
                    range_on_line(1, (11, 12))
                ))
            }
            .into(),
            block: Block::NonEmpty(vec![
                While {
                    condition: BinaryExpression {
                        operator: BinaryOperator::Eq,
                        lhs: boxed_expr(WithRange(
                            SimpleIdentifier("y".into()),
                            range_on_line(2, (6, 7))
                        )),
                        rhs: boxed_expr(WithRange(
                            LiteralExpression::Number(6.0),
                            range_on_line(2, (11, 12))
                        ))
                    }
                    .into(),
                    block: Block::NonEmpty(vec![PoeticNumberAssignment {
                        dest: WithRange(SimpleIdentifier("y".into()), range_on_line(3, (0, 1)))
                            .into(),
                        rhs: WithRange(LiteralExpression::Number(7.0), range_on_line(3, (5, 6)))
                            .into()
                    }
                    .into()])
                }
                .into(),
                PoeticNumberAssignment {
                    dest: WithRange(SimpleIdentifier("x".into()), range_on_line(5, (0, 1))).into(),
                    rhs: WithRange(LiteralExpression::Number(6.0), range_on_line(5, (5, 6))).into()
                }
                .into()
            ])
        }
        .into()]))
    );
}

#[test]
fn parse_build_knock() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(
        parse("build it up"),
        Ok(Some(
            Inc {
                dest: WithRange(Identifier::Pronoun, line_range(6, 8)),
                amount: 1
            }
            .into()
        ))
    );
    assert_eq!(
        parse("build it up, up, up"),
        Ok(Some(
            Inc {
                dest: WithRange(Identifier::Pronoun, line_range(6, 8)),
                amount: 3
            }
            .into()
        ))
    );
    assert_eq!(
        parse("knock it down"),
        Ok(Some(
            Dec {
                dest: WithRange(Identifier::Pronoun, line_range(6, 8)),
                amount: 1
            }
            .into()
        ))
    );
    assert_eq!(
        parse("knock it down down down, down"),
        Ok(Some(
            Dec {
                dest: WithRange(Identifier::Pronoun, line_range(6, 8)),
                amount: 4
            }
            .into()
        ))
    );
}

#[test]
fn parse_io() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(
        parse("say it"),
        Ok(Some(
            Output {
                value: WithRange(Identifier::Pronoun, line_range(4, 6)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("shout it"),
        Ok(Some(
            Output {
                value: WithRange(Identifier::Pronoun, line_range(6, 8)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("shout it at 255"),
        Ok(Some(
            Output {
                value: ArraySubscript {
                    array: boxed_expr(WithRange(Identifier::Pronoun, line_range(6, 8))),
                    subscript: boxed_expr(WithRange(
                        LiteralExpression::Number(255.0),
                        line_range(12, 15)
                    ))
                }
                .into()
            }
            .into()
        ))
    );

    assert_eq!(
        parse("listen to it"),
        Ok(Some(
            Input {
                dest: InputDest::Some(WithRange(Identifier::Pronoun, line_range(10, 12)).into())
            }
            .into()
        ))
    );
    assert_eq!(
        parse("listen to it at night"),
        Ok(Some(
            Input {
                dest: InputDest::Some(
                    ArraySubscript {
                        array: boxed_expr(WithRange(Identifier::Pronoun, line_range(10, 12))),
                        subscript: boxed_expr(WithRange(
                            SimpleIdentifier("night".into()),
                            line_range(16, 21)
                        )),
                    }
                    .into()
                )
            }
            .into()
        ))
    );
    assert_eq!(
        parse("listen"),
        Ok(Some(
            Input {
                dest: InputDest::None((1, 6).into())
            }
            .into()
        ))
    );
}

#[test]
fn parse_mutation() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(
        parse("cut it"),
        Ok(Some(
            Mutation {
                operator: MutationOperator::Cut,
                operand: WithRange(Identifier::Pronoun, line_range(4, 6)).into(),
                dest: None,
                param: None,
            }
            .into()
        ))
    );
    assert_eq!(
        parse("cut it into pieces"),
        Ok(Some(
            Mutation {
                operator: MutationOperator::Cut,
                operand: WithRange(Identifier::Pronoun, line_range(4, 6)).into(),
                dest: Some(WithRange(SimpleIdentifier("pieces".into()), line_range(12, 18)).into()),
                param: None,
            }
            .into()
        ))
    );
    assert_eq!(
        parse("cut it with my knife"),
        Ok(Some(
            Mutation {
                operator: MutationOperator::Cut,
                operand: WithRange(Identifier::Pronoun, line_range(4, 6)).into(),
                dest: None,
                param: Some(
                    WithRange(
                        CommonIdentifier("my".into(), "knife".into()),
                        line_range(12, 20)
                    )
                    .into()
                ),
            }
            .into()
        ))
    );
    assert_eq!(
        parse("cut it into pieces with my knife"),
        Ok(Some(
            Mutation {
                operator: MutationOperator::Cut,
                operand: WithRange(Identifier::Pronoun, line_range(4, 6)).into(),
                dest: Some(WithRange(SimpleIdentifier("pieces".into()), line_range(12, 18)).into()),
                param: Some(
                    WithRange(
                        CommonIdentifier("my".into(), "knife".into()),
                        line_range(24, 32)
                    )
                    .into()
                ),
            }
            .into()
        ))
    );
    assert_eq!(
        parse("cut it into pieces with my knife with my knife"),
        Ok(Some(
            Mutation {
                operator: MutationOperator::Cut,
                operand: WithRange(Identifier::Pronoun, line_range(4, 6)).into(),
                dest: Some(WithRange(SimpleIdentifier("pieces".into()), line_range(12, 18)).into()),
                param: Some(
                    BinaryExpression {
                        operator: BinaryOperator::Plus,
                        lhs: boxed_expr(WithRange(
                            CommonIdentifier("my".into(), "knife".into()),
                            line_range(24, 32)
                        )),
                        rhs: boxed_expr(WithRange(
                            CommonIdentifier("my".into(), "knife".into()),
                            line_range(38, 46)
                        )),
                    }
                    .into()
                ),
            }
            .into()
        ))
    );
    assert_eq!(
        parse("cut \"2, 2\" with my knife"),
        Err(ParseError::new(
            ParseErrorCode::MutationOperandMustBeIdentifier(
                WithRange(LiteralExpression::String("2, 2".into()), line_range(4, 10)).into()
            ),
            Token::new(TokenType::With, "with", line_range(11, 15)).into()
        ))
    );

    assert_eq!(
        parse("join it"),
        Ok(Some(
            Mutation {
                operator: MutationOperator::Join,
                operand: WithRange(Identifier::Pronoun, line_range(5, 7)).into(),
                dest: None,
                param: None,
            }
            .into()
        ))
    );
    assert_eq!(
        parse("cast it into the fire"),
        Ok(Some(
            Mutation {
                operator: MutationOperator::Cast,
                operand: WithRange(Identifier::Pronoun, line_range(5, 7)).into(),
                dest: Some(
                    WithRange(
                        CommonIdentifier("the".into(), "fire".into()),
                        line_range(13, 21)
                    )
                    .into()
                ),
                param: None,
            }
            .into()
        ))
    );

    assert_eq!(
        parse("cast it into the fire at Mount Doom"),
        Ok(Some(
            Mutation {
                operator: MutationOperator::Cast,
                operand: WithRange(Identifier::Pronoun, line_range(5, 7)).into(),
                dest: Some(
                    ArraySubscript {
                        array: boxed_expr(WithRange(
                            CommonIdentifier("the".into(), "fire".into()),
                            line_range(13, 21)
                        )),
                        subscript: boxed_expr(WithRange(
                            ProperIdentifier(vec!["Mount".into(), "Doom".into()]),
                            line_range(25, 35)
                        ))
                    }
                    .into()
                ),
                param: None,
            }
            .into()
        ))
    );
}

#[test]
fn parse_rounding() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(
        parse("Turn it up."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Up,
                operand: WithRange(Identifier::Pronoun, line_range(5, 7)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn up it."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Up,
                operand: WithRange(Identifier::Pronoun, line_range(8, 10)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn it down."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Down,
                operand: WithRange(Identifier::Pronoun, line_range(5, 7)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn down it."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Down,
                operand: WithRange(Identifier::Pronoun, line_range(10, 12)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn it around."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Nearest,
                operand: WithRange(Identifier::Pronoun, line_range(5, 7)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn around it."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Nearest,
                operand: WithRange(Identifier::Pronoun, line_range(12, 14)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn it round."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Nearest,
                operand: WithRange(Identifier::Pronoun, line_range(5, 7)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn round it."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Nearest,
                operand: WithRange(Identifier::Pronoun, line_range(11, 13)).into()
            }
            .into()
        ))
    );
}

#[test]
fn parse_break_continue() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(parse("break"), Ok(Some(Break(line_range(0, 5)).into())));
    assert_eq!(
        parse("break it down"),
        Ok(Some(Break(line_range(0, 13)).into()))
    );
    assert_eq!(
        parse(" BREAK IT DOWN "),
        Ok(Some(Break(line_range(1, 14)).into()))
    );
    assert_eq!(
        parse("continue "),
        Ok(Some(Continue(line_range(0, 8)).into()))
    );
    assert_eq!(
        parse("take it to the top"),
        Ok(Some(Continue(line_range(0, 18)).into()))
    );
    assert_eq!(
        parse(" TAKE IT TO THE TOP "),
        Ok(Some(Continue(line_range(1, 19)).into()))
    );

    assert_eq!(
        parse("break it"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedToken(TokenType::Down),
            loc: 1.into()
        })
    );
    assert_eq!(
        parse("take it to"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedText("the".into()),
            loc: 1.into()
        })
    );
}

#[test]
fn parse_array_push_pop() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(
        parse("rock ints"),
        Ok(Some(
            ArrayPush {
                array: WithRange(SimpleIdentifier("ints".into()), line_range(5, 9)).into(),
                value: None
            }
            .into()
        ))
    );
    assert_eq!(
        parse("rock ints with 1"),
        Ok(Some(
            ArrayPush {
                array: WithRange(SimpleIdentifier("ints".into()), line_range(5, 9)).into(),
                value: Some(WithRange(LiteralExpression::Number(1.0), line_range(15, 16)).into())
            }
            .into()
        ))
    );
    assert_eq!(
        parse("rock ints with 1, 2, 3"),
        Ok(Some(
            ArrayPush {
                array: WithRange(SimpleIdentifier("ints".into()), line_range(5, 9)).into(),
                value: Some(
                    ExpressionList {
                        first: WithRange(LiteralExpression::Number(1.0), line_range(15, 16)).into(),
                        rest: vec![
                            WithRange(LiteralExpression::Number(2.0), line_range(18, 19)).into(),
                            WithRange(LiteralExpression::Number(3.0), line_range(21, 22)).into()
                        ]
                    }
                    .into()
                )
            }
            .into()
        ))
    );
    assert_eq!(
        parse("rock ints with 1, 2 with 3, 4, 5"),
        Ok(Some(
            ArrayPush {
                array: WithRange(SimpleIdentifier("ints".into()), line_range(5, 9)).into(),
                value: Some(
                    ExpressionList {
                        first: WithRange(LiteralExpression::Number(1.0), line_range(15, 16)).into(),
                        rest: vec![
                            BinaryExpression {
                                operator: BinaryOperator::Plus,
                                lhs: boxed_expr(WithRange(
                                    LiteralExpression::Number(2.0),
                                    line_range(18, 19)
                                )),
                                rhs: boxed_expr(WithRange(
                                    LiteralExpression::Number(3.0),
                                    line_range(25, 26)
                                ))
                            }
                            .into(),
                            WithRange(LiteralExpression::Number(4.0), line_range(28, 29)).into(),
                            WithRange(LiteralExpression::Number(5.0), line_range(31, 32)).into(),
                        ]
                    }
                    .into()
                )
            }
            .into()
        ))
    );
    assert_eq!(
        parse("rock you like a hurricane"),
        Ok(Some(
            ArrayPush {
                array: WithRange(SimpleIdentifier("you".into()), line_range(5, 8)).into(),
                value: Some(
                    PoeticNumberLiteral {
                        elems: vec![
                            PoeticNumberLiteralElem::Word("a".into()),
                            PoeticNumberLiteralElem::Word("hurricane".into())
                        ]
                    }
                    .into()
                )
            }
            .into()
        ))
    );
    assert_eq!(
        parse("rock you like nothing"),
        Ok(Some(
            ArrayPush {
                array: WithRange(SimpleIdentifier("you".into()), line_range(5, 8)).into(),
                value: Some(
                    PoeticNumberLiteral {
                        elems: vec![PoeticNumberLiteralElem::Word("nothing".into())]
                    }
                    .into()
                )
            }
            .into()
        ))
    );
    assert_eq!(
        parse("rock ints at night with 1"),
        Ok(Some(
            ArrayPush {
                array: ArraySubscript {
                    array: boxed_expr(WithRange(SimpleIdentifier("ints".into()), line_range(5, 9))),
                    subscript: boxed_expr(WithRange(
                        SimpleIdentifier("night".into()),
                        line_range(13, 18)
                    ))
                }
                .into(),
                value: Some(WithRange(LiteralExpression::Number(1.0), line_range(24, 25)).into())
            }
            .into()
        ))
    );

    assert_eq!(
        parse("roll ints"),
        Ok(Some(
            ArrayPop {
                array: WithRange(SimpleIdentifier("ints".into()), line_range(5, 9)).into(),
                dest: None
            }
            .into()
        ))
    );
    assert_eq!(
        parse("roll ints into it"),
        Ok(Some(
            ArrayPop {
                array: WithRange(SimpleIdentifier("ints".into()), line_range(5, 9)).into(),
                dest: Some(WithRange(Identifier::Pronoun, line_range(15, 17)).into())
            }
            .into()
        ))
    );
    assert_eq!(
        parse("roll ints at night"),
        Ok(Some(
            ArrayPop {
                array: ArraySubscript {
                    array: boxed_expr(WithRange(SimpleIdentifier("ints".into()), line_range(5, 9))),
                    subscript: boxed_expr(WithRange(
                        SimpleIdentifier("night".into()),
                        line_range(13, 18)
                    ))
                }
                .into(),
                dest: None
            }
            .into()
        ))
    );
}
#[test]
fn parse_return() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(
        parse("return it"),
        Ok(Some(
            Return {
                value: WithRange(Identifier::Pronoun, line_range(7, 9)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("give it"),
        Ok(Some(
            Return {
                value: WithRange(Identifier::Pronoun, line_range(5, 7)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("give it back"),
        Ok(Some(
            Return {
                value: WithRange(Identifier::Pronoun, line_range(5, 7)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("give back it"),
        Ok(Some(
            Return {
                value: WithRange(Identifier::Pronoun, line_range(10, 12)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("give back it back"),
        Ok(Some(
            Return {
                value: WithRange(Identifier::Pronoun, line_range(10, 12)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("return it back"),
        Ok(Some(
            Return {
                value: WithRange(Identifier::Pronoun, line_range(7, 9)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("send it back"),
        Ok(Some(
            Return {
                value: WithRange(Identifier::Pronoun, line_range(5, 7)).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("send back it"),
        Err(ParseError::new(
            ParseErrorCode::ExpectedPrimaryExpression,
            Token::new(TokenType::Back, "back", line_range(5, 9)).into()
        ))
    );
    assert_eq!(
        parse("give back X with Y"),
        Ok(Some(
            Return {
                value: BinaryExpression {
                    operator: BinaryOperator::Plus,
                    lhs: boxed_expr(WithRange(SimpleIdentifier("X".into()), line_range(10, 11))),
                    rhs: boxed_expr(WithRange(SimpleIdentifier("Y".into()), line_range(17, 18)))
                }
                .into()
            }
            .into()
        ))
    );
}

#[test]
fn parse_function() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(
        parse(
            "\
Echo takes X
say X
            "
        ),
        Ok(Some(
            Function {
                name: WithRange(SimpleIdentifier("Echo".into()), range_on_line(1, (0, 4))).into(),
                data: Arc::new(FunctionData {
                    params: vec![WithRange(
                        SimpleIdentifier("X".into()),
                        range_on_line(1, (11, 12))
                    )
                    .into()],
                    body: Block::NonEmpty(vec![Output {
                        value: WithRange(SimpleIdentifier("X".into()), range_on_line(2, (4, 5)))
                            .into()
                    }
                    .into()])
                })
            }
            .into()
        ))
    );
    assert_eq!(
        parse(
            "\
my heart takes X
say X
            "
        ),
        Ok(Some(
            Function {
                name: WithRange(
                    CommonIdentifier("my".into(), "heart".into()),
                    range_on_line(1, (0, 8))
                )
                .into(),
                data: Arc::new(FunctionData {
                    params: vec![WithRange(
                        SimpleIdentifier("X".into()),
                        range_on_line(1, (15, 16))
                    )
                    .into()],
                    body: Block::NonEmpty(vec![Output {
                        value: WithRange(SimpleIdentifier("X".into()), range_on_line(2, (4, 5)))
                            .into()
                    }
                    .into()])
                })
            }
            .into()
        ))
    );
    assert_eq!(
        parse(
            "\
Tom Sawyer wants my body
say X
            "
        ),
        Ok(Some(
            Function {
                name: WithRange(
                    ProperIdentifier(vec!["Tom".into(), "Sawyer".into()]),
                    range_on_line(1, (0, 10))
                )
                .into(),
                data: Arc::new(FunctionData {
                    params: vec![WithRange(
                        CommonIdentifier("my".into(), "body".into()),
                        range_on_line(1, (17, 24))
                    )
                    .into()],
                    body: Block::NonEmpty(vec![Output {
                        value: WithRange(SimpleIdentifier("X".into()), range_on_line(2, (4, 5)))
                            .into()
                    }
                    .into()])
                })
            }
            .into()
        ))
    );
    assert_eq!(
        parse(
            "\
AddOrSub takes X, and B
if B
Give Back X plus 1
(end if)
say \"else\"
Give Back X minus 1
(end function)
            "
        ),
        Ok(Some(
            Function {
                name: WithRange(
                    SimpleIdentifier("AddOrSub".into()),
                    range_on_line(1, (0, 8))
                )
                .into(),
                data: Arc::new(FunctionData {
                    params: vec![
                        WithRange(SimpleIdentifier("X".into()), range_on_line(1, (15, 16))).into(),
                        WithRange(SimpleIdentifier("B".into()), range_on_line(1, (22, 23))).into()
                    ],
                    body: Block::NonEmpty(vec![
                        If {
                            condition: WithRange(
                                SimpleIdentifier("B".into()),
                                range_on_line(2, (3, 4))
                            )
                            .into(),
                            then_block: Block::NonEmpty(vec![Return {
                                value: BinaryExpression {
                                    operator: BinaryOperator::Plus,
                                    lhs: boxed_expr(WithRange(
                                        SimpleIdentifier("X".into()),
                                        range_on_line(3, (10, 11))
                                    )),
                                    rhs: boxed_expr(WithRange(
                                        LiteralExpression::Number(1.0),
                                        range_on_line(3, (17, 18))
                                    ))
                                }
                                .into()
                            }
                            .into()]),
                            else_block: None,
                        }
                        .into(),
                        Output {
                            value: WithRange(
                                LiteralExpression::String("else".into()),
                                range_on_line(5, (4, 10))
                            )
                            .into()
                        }
                        .into(),
                        Return {
                            value: BinaryExpression {
                                operator: BinaryOperator::Minus,
                                lhs: boxed_expr(WithRange(
                                    SimpleIdentifier("X".into()),
                                    range_on_line(6, (10, 11))
                                )),
                                rhs: boxed_expr(WithRange(
                                    LiteralExpression::Number(1.0),
                                    range_on_line(6, (18, 19))
                                ))
                            }
                            .into()
                        }
                        .into()
                    ])
                })
            }
            .into()
        ))
    );
}

#[test]
fn parse_function_call_expr() {
    let parse = |text| Parser::for_source_code(text).parse_expression();

    assert_eq!(
        parse("F taking X"),
        Ok(FunctionCall {
            name: WithRange(SimpleIdentifier("F".into()), line_range(0, 1)).into(),
            args: vec![WithRange(SimpleIdentifier("X".into()), line_range(9, 10)).into()]
        }
        .into())
    );

    assert_eq!(
        parse("my heart taking X, Y, and Z, and 12"),
        Ok(FunctionCall {
            name: WithRange(
                CommonIdentifier("my".into(), "heart".into()),
                line_range(0, 8)
            )
            .into(),
            args: vec![
                WithRange(SimpleIdentifier("X".into()), line_range(16, 17)).into(),
                WithRange(SimpleIdentifier("Y".into()), line_range(19, 20)).into(),
                WithRange(SimpleIdentifier("Z".into()), line_range(26, 27)).into(),
                WithRange(LiteralExpression::Number(12.0), line_range(33, 35)).into(),
            ]
        }
        .into())
    );
}

#[test]
fn parse_function_call_statement() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(
        parse("F taking X"),
        Ok(Some(
            FunctionCall {
                name: WithRange(SimpleIdentifier("F".into()), line_range(0, 1)).into(),
                args: vec![WithRange(SimpleIdentifier("X".into()), line_range(9, 10)).into()]
            }
            .into()
        ))
    );

    assert_eq!(
        parse("my heart taking X, Y, and Z, and 12"),
        Ok(Some(
            FunctionCall {
                name: WithRange(
                    CommonIdentifier("my".into(), "heart".into()),
                    line_range(0, 8)
                )
                .into(),
                args: vec![
                    WithRange(SimpleIdentifier("X".into()), line_range(16, 17)).into(),
                    WithRange(SimpleIdentifier("Y".into()), line_range(19, 20)).into(),
                    WithRange(SimpleIdentifier("Z".into()), line_range(26, 27)).into(),
                    WithRange(LiteralExpression::Number(12.0), line_range(33, 35)).into(),
                ]
            }
            .into()
        ))
    );
}

#[test]
fn parse() {
    let parse = super::parse;

    assert_eq!(
        parse(
            "\
let X be Y

let Y be Z


let Z be Z
    "
        ),
        Ok(Program {
            code: vec![
                Block::NonEmpty(vec![Assignment {
                    dest: WithRange(SimpleIdentifier("X".into()), range_on_line(1, (4, 5))).into(),
                    value: WithRange(SimpleIdentifier("Y".into()), range_on_line(1, (9, 10)))
                        .into(),
                    operator: None
                }
                .into()])
                .into(),
                Block::NonEmpty(vec![Assignment {
                    dest: WithRange(SimpleIdentifier("Y".into()), range_on_line(3, (4, 5))).into(),
                    value: WithRange(SimpleIdentifier("Z".into()), range_on_line(3, (9, 10)))
                        .into(),
                    operator: None
                }
                .into()])
                .into(),
                Block::NonEmpty(vec![Assignment {
                    dest: WithRange(SimpleIdentifier("Z".into()), range_on_line(6, (4, 5))).into(),
                    value: WithRange(SimpleIdentifier("Z".into()), range_on_line(6, (9, 10)))
                        .into(),
                    operator: None
                }
                .into()])
                .into()
            ]
        })
    );
}

#[test]
fn parse_error_to_string() {
    let bogus_loc = || {
        ParseErrorLocation::from(Token::new(
            TokenType::ApostropheNApostrophe,
            "bloop",
            line_range(0, 5),
        ))
    };
    let bogus_tok = || Token::new(TokenType::Word, "worrrd", line_range(0, 6));

    macro_rules! check {
        ($err_code:expr, $without_token:literal, $with_token:literal $(,)?) => {
            assert_eq!(
                ParseError::new($err_code, 1.into()).to_string(),
                $without_token
            );
            assert_eq!(
                ParseError::new($err_code, bogus_loc()).to_string(),
                $with_token
            );
        };
    }

    check!(
        ParseErrorCode::Generic("Something bad".into()),
        "Parse error (line 1): Something bad",
        "Parse error (line 1): Something bad at `bloop`"
    );

    check!(
        ParseErrorCode::ExpectedSpaceAfterSays(bogus_tok()),
        "Parse error (line 1): Expected space after `worrrd`",
        "Parse error (line 1): Expected space after `worrrd`, found `bloop`"
    );

    check!(
        ParseErrorCode::MissingIDAfterCommonPrefix("my".into()),
        "Parse error (line 1): Missing identifier after `my`",
        "Parse error (line 1): Missing identifier after `my`, found `bloop`"
    );

    check!(
        ParseErrorCode::UppercaseAfterCommonPrefix("my".into(), "heArt".into()),
        "Parse error (line 1): Unexpected uppercase after common variable prefix; `my heArt` is an invalid common variable name",
        "Parse error (line 1): Unexpected uppercase after common variable prefix; `my heArt` is an invalid common variable name"
    );

    check!(
        ParseErrorCode::MutationOperandMustBeIdentifier(
            WithRange(LiteralExpression::Number(0.0), bogus_range()).into()
        ),
        "Parse error (line 1): Mutation operand with no `into` destination must be identifier; found literal",
        "Parse error (line 1): Mutation operand with no `into` destination must be identifier; found literal"
    );
    check!(
        ParseErrorCode::MutationOperandMustBeIdentifier(
            ArraySubscript {
                array: boxed_expr(WithRange(Identifier::Pronoun, bogus_range())),
                subscript: boxed_expr(WithRange(Identifier::Pronoun, bogus_range()))
            }
            .into()
        ),
        "Parse error (line 1): Mutation operand with no `into` destination must be identifier; found array subscript expression",
        "Parse error (line 1): Mutation operand with no `into` destination must be identifier; found array subscript expression"
    );
    check!(
        ParseErrorCode::MutationOperandMustBeIdentifier(
            FunctionCall {
                name: WithRange(SimpleIdentifier("foo".into()), bogus_range()).into(),
                args: vec![]
            }
            .into()
        ),
        "Parse error (line 1): Mutation operand with no `into` destination must be identifier; found function call",
        "Parse error (line 1): Mutation operand with no `into` destination must be identifier; found function call"
    );

    check!(
        ParseErrorCode::ExpectedPrimaryExpression,
        "Parse error (line 1): Expected primary expression",
        "Parse error (line 1): Expected primary expression, found `bloop`"
    );

    check!(
        ParseErrorCode::ExpectedIdentifier,
        "Parse error (line 1): Expected identifier",
        "Parse error (line 1): Expected identifier, found `bloop`"
    );

    check!(
        ParseErrorCode::ExpectedText("hello, world".into()),
        "Parse error (line 1): Expected `hello, world`",
        "Parse error (line 1): Expected `hello, world`, found `bloop`"
    );

    check!(
        ParseErrorCode::ExpectedToken(TokenType::And),
        "Parse error (line 1): Expected `and`",
        "Parse error (line 1): Expected `and`, found `bloop`"
    );
    check!(
        ParseErrorCode::ExpectedToken(TokenType::ApostropheS),
        "Parse error (line 1): Expected `'s`",
        "Parse error (line 1): Expected `'s`, found `bloop`"
    );

    check!(
        ParseErrorCode::ExpectedOneOfTokens(vec![TokenType::And]),
        "Parse error (line 1): Expected `and`",
        "Parse error (line 1): Expected `and`, found `bloop`"
    );
    check!(
        ParseErrorCode::ExpectedOneOfTokens(vec![TokenType::And, TokenType::Big]),
        "Parse error (line 1): Expected `and` or `big`",
        "Parse error (line 1): Expected `and` or `big`, found `bloop`"
    );
    check!(
        ParseErrorCode::ExpectedOneOfTokens(vec![
            TokenType::And,
            TokenType::Big,
            TokenType::Mysterious,
        ]),
        "Parse error (line 1): Expected `and`, `big`, or `mysterious`",
        "Parse error (line 1): Expected `and`, `big`, or `mysterious`, found `bloop`"
    );
    check!(
        ParseErrorCode::ExpectedOneOfTokens(vec![
            TokenType::And,
            TokenType::Big,
            TokenType::Mysterious,
            TokenType::Says,
        ]),
        "Parse error (line 1): Expected `and`, `big`, `mysterious`, or `says`",
        "Parse error (line 1): Expected `and`, `big`, `mysterious`, or `says`, found `bloop`"
    );

    check!(
        ParseErrorCode::ExpectedPoeticNumberLiteral,
        "Parse error (line 1): Expected poetic number literal",
        "Parse error (line 1): Expected poetic number literal, found `bloop`"
    );

    // UnexpectedToken always has a token
    assert_eq!(
        ParseError::new(ParseErrorCode::UnexpectedToken, bogus_loc()).to_string(),
        "Parse error (line 1): Unexpected token `bloop`"
    );

    check!(
        ParseErrorCode::UnexpectedEndOfTokens,
        "Parse error (line 1): Unexpected end of tokens",
        "Parse error (line 1): Unexpected end of tokens"
    );
}

#[test]
fn some_diags() {
    let diag = |code| super::parse(code).unwrap_err().to_string();

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
