use super::*;

#[test]
fn parse_literal_expression() {
    let parse = |text| Parser::for_source_code(text).parse_literal_expression();
    assert_eq!(parse("null"), Some(LiteralExpression::Null));
    assert_eq!(parse("true"), Some(LiteralExpression::Boolean(true)));
    assert_eq!(parse("false"), Some(LiteralExpression::Boolean(false)));
    assert_eq!(
        parse("empty"),
        Some(LiteralExpression::String(String::new()))
    );
    assert_eq!(
        parse("\"\""),
        Some(LiteralExpression::String("".to_owned()))
    );
    assert_eq!(parse("5"), Some(LiteralExpression::Number(5.0)));

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
            operand: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("--1"),
        Ok(UnaryExpression {
            operator: UnaryOperator::Minus,
            operand: boxed_expr(UnaryExpression {
                operator: UnaryOperator::Minus,
                operand: boxed_expr(LiteralExpression::Number(1.0))
            })
        }
        .into())
    );

    assert_eq!(
        parse("not cool"),
        Ok(UnaryExpression {
            operator: UnaryOperator::Not,
            operand: boxed_expr(SimpleIdentifier("cool".into()))
        }
        .into())
    );
    assert_eq!(
        parse("not not cool"),
        Ok(UnaryExpression {
            operator: UnaryOperator::Not,
            operand: boxed_expr(UnaryExpression {
                operator: UnaryOperator::Not,
                operand: boxed_expr(SimpleIdentifier("cool".into()))
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
                operand: boxed_expr(LiteralExpression::Number(1.0))
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
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 with 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 - 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Minus,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 without 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Minus,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 - -1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Minus,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(UnaryExpression {
                operator: UnaryOperator::Minus,
                operand: boxed_expr(LiteralExpression::Number(1.0))
            })
        }
        .into())
    );
    assert_eq!(
        parse("1 * 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Multiply,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 / 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Divide,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
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
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            }),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 - 1 - 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Minus,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Minus,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            }),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 * 1 + 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Multiply,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            }),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 / 1 + 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Divide,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            }),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 + 1 * 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Multiply,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            }),
        }
        .into())
    );
    assert_eq!(
        parse("1 + 1 / 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Divide,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
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
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 <= 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::LessEq,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 > 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Greater,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 >= 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::GreaterEq,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 > 1 < 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Less,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Greater,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            }),
            rhs: boxed_expr(LiteralExpression::Number(1.0)),
        }
        .into())
    );
    assert_eq!(
        parse("1 > 1 + 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Greater,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
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
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 is not 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::NotEq,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("Tommy's 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Eq,
            lhs: boxed_expr(SimpleIdentifier("Tommy".into())),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 ain't 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::NotEq,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 + 1 is not 1 * 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::NotEq,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            }),
            rhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Multiply,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            })
        }
        .into())
    );

    assert_eq!(
        parse("1 is bigger than 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Greater,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 is as big as 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::GreaterEq,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 is smaller than 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Less,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 is as small as 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::LessEq,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 is 1 is 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Eq,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Eq,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            }),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 is as small as 1 is bigger than 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Greater,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::LessEq,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            }),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
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
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 or 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Or,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 nor 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Nor,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }
        .into())
    );
    assert_eq!(
        parse("1 and 1 or 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::Or,
            lhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::And,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            }),
            rhs: boxed_expr(LiteralExpression::Number(1.0)),
        }
        .into())
    );
    assert_eq!(
        parse("1 and 1 < 1"),
        Ok(BinaryExpression {
            operator: BinaryOperator::And,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Less,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
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
        Ok(Some(CommonIdentifier("my".into(), "heart".into()).into()))
    );
    assert_eq!(
        parse("your heart"),
        Ok(Some(CommonIdentifier("your".into(), "heart".into()).into()))
    );
    assert_eq!(
        parse("My heart"),
        Ok(Some(CommonIdentifier("My".into(), "heart".into()).into()))
    );
    assert_eq!(
        parse("Your heart"),
        Ok(Some(CommonIdentifier("Your".into(), "heart".into()).into()))
    );

    assert_eq!(
        parse("Billie Jean"),
        Ok(Some(
            ProperIdentifier(vec!["Billie".into(), "Jean".into()]).into()
        ))
    );
    assert_eq!(
        // parse("Distance In KM"), // this example is from the official spec, but I think it's broken: 'in' is a language keyword
        parse("Distance Out KM"),
        Ok(Some(
            // ProperIdentifier(vec!["Distance".into(), "In".into(), "KM".into()]).into()
            ProperIdentifier(vec!["Distance".into(), "Out".into(), "KM".into()]).into()
        ))
    );
    assert_eq!(
        parse("Tom Sawyer"),
        Ok(Some(
            ProperIdentifier(vec!["Tom".into(), "Sawyer".into()]).into()
        ))
    );
    assert_eq!(
        parse("TOM SAWYER"),
        Ok(Some(
            ProperIdentifier(vec!["TOM".into(), "SAWYER".into()]).into()
        ))
    );
    assert_eq!(
        parse("TOm SAWyer"),
        Ok(Some(
            ProperIdentifier(vec!["TOm".into(), "SAWyer".into()]).into()
        ))
    );
}
#[test]

fn parse_identifier_errors() {
    let parse = |text| Parser::for_source_code(text).parse_identifier();
    assert_eq!(
        parse("DOCTOR feelgood"),
        Ok(Some(SimpleIdentifier("DOCTOR".into()).into())) // TODO ParseError here?
    );

    assert_eq!(
        parse("my"),
        Err(ParseError::new(
            ParseErrorCode::MissingIDAfterCommonPrefix("my".into()),
            None
        ))
    );
    assert_eq!(
        parse("your"),
        Err(ParseError::new(
            ParseErrorCode::MissingIDAfterCommonPrefix("your".into()),
            None
        ))
    );

    assert_eq!(
        parse("my Heart"),
        Err(ParseError::new(
            ParseErrorCode::UppercaseAfterCommonPrefix("my".into(), "Heart".into()).into(),
            None
        ))
    );
    assert_eq!(
        parse("your Heart"),
        Err(ParseError::new(
            ParseErrorCode::UppercaseAfterCommonPrefix("your".into(), "Heart".into()).into(),
            None
        ))
    );
    assert_eq!(
        parse("My Heart"),
        Err(ParseError::new(
            ParseErrorCode::UppercaseAfterCommonPrefix("My".into(), "Heart".into()).into(),
            None
        ))
    );
    assert_eq!(
        parse("Your Heart"),
        Err(ParseError::new(
            ParseErrorCode::UppercaseAfterCommonPrefix("Your".into(), "Heart".into()).into(),
            None
        ))
    );
    assert_eq!(
        parse("your heArt"),
        Err(ParseError::new(
            ParseErrorCode::UppercaseAfterCommonPrefix("your".into(), "heArt".into()).into(),
            None
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
            Some(Identifier::Pronoun.into())
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
            lhs: boxed_expr(PrimaryExpression::from(CommonIdentifier(
                "my".into(),
                "heart".into()
            ))),
            rhs: boxed_expr(PrimaryExpression::from(CommonIdentifier(
                "your".into(),
                "heart".into()
            )))
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
            dest: SimpleIdentifier("result".into()).into(),
            value: BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(SimpleIdentifier("x".into())),
                rhs: boxed_expr(SimpleIdentifier("y".into()))
            }
            .into(),
            operator: None
        }
        .into())
    );

    assert_eq!(
        parse("Put 123 into X"),
        Ok(Assignment {
            dest: SimpleIdentifier("X".into()).into(),
            value: LiteralExpression::Number(123.0).into(),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Put \"Hello San Francisco\" into the message"),
        Ok(Assignment {
            dest: CommonIdentifier("the".into(), "message".into()).into(),
            value: LiteralExpression::String("Hello San Francisco".into()).into(),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Let my balance be 1000000"),
        Ok(Assignment {
            dest: CommonIdentifier("my".into(), "balance".into()).into(),
            value: LiteralExpression::Number(1000000.0).into(),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Let the survivors be the brave without the fallen"),
        Ok(Assignment {
            dest: CommonIdentifier("the".into(), "survivors".into()).into(),
            value: BinaryExpression {
                operator: BinaryOperator::Minus,
                lhs: boxed_expr(CommonIdentifier("the".into(), "brave".into())),
                rhs: boxed_expr(CommonIdentifier("the".into(), "fallen".into()))
            }
            .into(),
            operator: None
        }
        .into())
    );

    assert_eq!(
        parse("Let X be with 10"),
        Ok(Assignment {
            dest: SimpleIdentifier("X".into()).into(),
            value: LiteralExpression::Number(10.0).into(),
            operator: Some(BinaryOperator::Plus),
        }
        .into())
    );
    assert_eq!(
        parse("Let the children be without fear"),
        Ok(Assignment {
            dest: CommonIdentifier("the".into(), "children".into()).into(),
            value: SimpleIdentifier("fear".into()).into(),
            operator: Some(BinaryOperator::Minus),
        }
        .into())
    );
    assert_eq!(
        parse("Let my heart be over the moon"),
        Ok(Assignment {
            dest: CommonIdentifier("my".into(), "heart".into()).into(),
            value: CommonIdentifier("the".into(), "moon".into()).into(),
            operator: Some(BinaryOperator::Divide),
        }
        .into())
    );

    assert_eq!(
        parse("Put the whole of your heart into my hands"),
        Ok(Assignment {
            dest: CommonIdentifier("my".into(), "hands".into()).into(),
            value: BinaryExpression {
                operator: BinaryOperator::Multiply,
                lhs: boxed_expr(CommonIdentifier("the".into(), "whole".into())),
                rhs: boxed_expr(CommonIdentifier("your".into(), "heart".into()))
            }
            .into(),
            operator: None
        }
        .into())
    );

    assert_eq!(
        parse("Let X be 1 with 2, 3, 4"),
        Ok(Assignment {
            dest: SimpleIdentifier("X".into()).into(),
            value: BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(ExpressionList {
                    first: LiteralExpression::Number(2.0).into(),
                    rest: vec![
                        LiteralExpression::Number(3.0).into(),
                        LiteralExpression::Number(4.0).into()
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
            dest: SimpleIdentifier("X".into()).into(),
            value: BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(ExpressionList {
                    first: LiteralExpression::Number(2.0).into(),
                    rest: vec![
                        LiteralExpression::Number(3.0).into(),
                        LiteralExpression::Number(4.0).into()
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
            dest: SimpleIdentifier("X".into()).into(),
            value: ExpressionList {
                first: LiteralExpression::Number(2.0).into(),
                rest: vec![
                    LiteralExpression::Number(3.0).into(),
                    LiteralExpression::Number(4.0).into()
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
                array: boxed_expr(CommonIdentifier("my".into(), "array".into())),
                subscript: boxed_expr(LiteralExpression::Number(255.0))
            }
            .into(),
            value: LiteralExpression::String("some value".into()).into(),
            operator: None,
        }
        .into())
    );
    assert_eq!(
        parse("Let my array at 255 be my array at 255"),
        Ok(Assignment {
            dest: ArraySubscript {
                array: boxed_expr(CommonIdentifier("my".into(), "array".into())),
                subscript: boxed_expr(LiteralExpression::Number(255.0))
            }
            .into(),
            value: ArraySubscript {
                array: boxed_expr(CommonIdentifier("my".into(), "array".into())),
                subscript: boxed_expr(LiteralExpression::Number(255.0))
            }
            .into(),
            operator: None,
        }
        .into())
    );
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
            token: Some(Token {
                id: TokenType::Number(5.0),
                spelling: "5"
            })
        })
    );
    assert_eq!(
        parse("Put 6 into 5"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedIdentifier,
            token: Some(Token {
                id: TokenType::Number(5.0),
                spelling: "5"
            })
        })
    );
    assert_eq!(
        parse("Let five bee 6"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedToken(TokenType::Be),
            token: Some(Token {
                id: TokenType::Word,
                spelling: "bee"
            })
        })
    );
    assert_eq!(
        parse("Put five intoo six"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedToken(TokenType::Into),
            token: Some(Token {
                id: TokenType::Word,
                spelling: "intoo"
            })
        })
    );

    assert_eq!(
        parse("Put 1, 2, 3 into six"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedToken(TokenType::Into),
            token: Some(Token {
                id: TokenType::Comma,
                spelling: ","
            })
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
            dest: SimpleIdentifier("Variable".into()).into(),
            rhs: LiteralExpression::Number(1.0).into(),
        }
        .into())
    );
    assert_eq!(
        parse("Variable at 1 is 1"),
        Ok(PoeticNumberAssignment {
            dest: ArraySubscript {
                array: boxed_expr(SimpleIdentifier("Variable".into())),
                subscript: boxed_expr(LiteralExpression::Number(1.0)),
            }
            .into(),
            rhs: LiteralExpression::Number(1.0).into(),
        }
        .into())
    );
    assert_eq!(
        parse("Tommy is a rockstar"),
        Ok(PoeticNumberAssignment {
            dest: SimpleIdentifier("Tommy".into()).into(),
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
            dest: SimpleIdentifier("Tommy".into()).into(),
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
            dest: ProperIdentifier(vec!["Sweet".into(), "Lucy".into()]).into(),
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
            dest: SimpleIdentifier("Tommy".into()).into(),
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
            dest: SimpleIdentifier("Tommy".into()).into(),
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
            dest: SimpleIdentifier("Tommy".into()).into(),
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
            dest: SimpleIdentifier("Tommy".into()).into(),
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
            dest: SimpleIdentifier("Tommy".into()).into(),
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
            dest: SimpleIdentifier("Tommy".into()).into(),
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
            dest: SimpleIdentifier("X".into()).into(),
            rhs: LiteralExpression::Number(2.0).into(),
        }
        .into())
    );
    assert_eq!(
        parse("Y is 3"),
        Ok(PoeticNumberAssignment {
            dest: SimpleIdentifier("Y".into()).into(),
            rhs: LiteralExpression::Number(3.0).into(),
        }
        .into())
    );
    assert_eq!(
        parse("noise is silence"),
        Ok(PoeticNumberAssignment {
            dest: SimpleIdentifier("noise".into()).into(),
            rhs: LiteralExpression::String(String::new()).into(),
        }
        .into())
    );

    assert_eq!(
        parse("Variable's 1"),
        Ok(PoeticNumberAssignment {
            dest: SimpleIdentifier("Variable".into()).into(),
            rhs: LiteralExpression::Number(1.0).into(),
        }
        .into())
    );
    assert_eq!(
        parse("Variables are 1"),
        Ok(PoeticNumberAssignment {
            dest: SimpleIdentifier("Variables".into()).into(),
            rhs: LiteralExpression::Number(1.0).into(),
        }
        .into())
    );
    assert_eq!(
        parse("We're 1"),
        Ok(PoeticNumberAssignment {
            dest: SimpleIdentifier("We".into()).into(),
            rhs: LiteralExpression::Number(1.0).into(),
        }
        .into())
    );
    assert_eq!(
        parse("My world is nothing without your love"),
        Ok(PoeticNumberAssignment {
            dest: CommonIdentifier("My".into(), "world".into()).into(),
            rhs: BinaryExpression {
                operator: BinaryOperator::Minus,
                lhs: boxed_expr(LiteralExpression::Null),
                rhs: boxed_expr(CommonIdentifier("your".into(), "love".into()))
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
            dest: SimpleIdentifier("Peter".into()).into(),
            rhs: "Hello San Francisco!".into(),
        }
        .into())
    );
    assert_eq!(
        parse("Peter says Hello San Francisco!"),
        Ok(PoeticStringAssignment {
            dest: SimpleIdentifier("Peter".into()).into(),
            rhs: "Hello San Francisco!".into(),
        }
        .into())
    );
    assert_eq!(
        parse("Peter says    Hello    San    Francisco!    \n"),
        Ok(PoeticStringAssignment {
            dest: SimpleIdentifier("Peter".into()).into(),
            rhs: "   Hello    San    Francisco!    ".into(),
        }
        .into())
    );
    assert_eq!(
        parse("Peter says    Hello    San    Francisco!    "),
        Ok(PoeticStringAssignment {
            dest: SimpleIdentifier("Peter".into()).into(),
            rhs: "   Hello    San    Francisco!    ".into(),
        }
        .into())
    );

    assert_eq!(
        parse("Peter say Hello San Francisco!\n"),
        Ok(PoeticStringAssignment {
            dest: SimpleIdentifier("Peter".into()).into(),
            rhs: "Hello San Francisco!".into(),
        }
        .into())
    );
    assert_eq!(
        parse("Peter say Hello San Francisco!"),
        Ok(PoeticStringAssignment {
            dest: SimpleIdentifier("Peter".into()).into(),
            rhs: "Hello San Francisco!".into(),
        }
        .into())
    );
    assert_eq!(
        parse("Peter say    Hello    San    Francisco!    \n"),
        Ok(PoeticStringAssignment {
            dest: SimpleIdentifier("Peter".into()).into(),
            rhs: "   Hello    San    Francisco!    ".into(),
        }
        .into())
    );
    assert_eq!(
        parse("Peter say    Hello    San    Francisco!    "),
        Ok(PoeticStringAssignment {
            dest: SimpleIdentifier("Peter".into()).into(),
            rhs: "   Hello    San    Francisco!    ".into(),
        }
        .into())
    );

    // I think this is technically allowed?
    assert_eq!(
        parse("Peter says "),
        Ok(PoeticStringAssignment {
            dest: SimpleIdentifier("Peter".into()).into(),
            rhs: "".into(),
        }
        .into())
    );

    assert_eq!(
        parse("My parents said we'd never make it\n"),
        Ok(PoeticStringAssignment {
            dest: CommonIdentifier("My".into(), "parents".into()).into(),
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
            token: Some(Token {
                id: TokenType::Dot,
                spelling: "."
            })
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
            token: Some(Token {
                id: TokenType::SayAlias,
                spelling: "whisper"
            })
        })
    );

    assert_eq!(
        parse("My world is without-"),
        Err(ParseError {
            code: ParseErrorCode::UnexpectedEndOfTokens,
            token: None
        })
    );
    assert_eq!(
        parse("My world is without--"),
        Err(ParseError {
            code: ParseErrorCode::UnexpectedToken,
            token: Some(Token {
                id: TokenType::Minus,
                spelling: "-"
            })
        })
    );

    assert_eq!(
        parse("My world said"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedSpaceAfterSays(Token::new(TokenType::Says, "said")),
            token: None
        })
    );
    assert_eq!(
        parse("My world said\n"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedSpaceAfterSays(Token::new(TokenType::Says, "said")),
            token: Some(Token::new(TokenType::Newline, "\n"))
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

#[test]
fn parse_block() {
    let parse = |text| Parser::for_source_code(text).parse_block();
    assert_eq!(parse("\n"), Ok(Block::empty()));

    assert_eq!(
        parse(
            "\
            Variable is 1
            Tommy is a rockstar"
        ),
        Ok(Block(vec![
            StatementWithLine(
                PoeticNumberAssignment {
                    dest: SimpleIdentifier("Variable".into()).into(),
                    rhs: LiteralExpression::Number(1.0).into()
                }
                .into(),
                1
            ),
            StatementWithLine(
                PoeticNumberAssignment {
                    dest: SimpleIdentifier("Tommy".into()).into(),
                    rhs: PoeticNumberLiteral {
                        elems: vec![
                            PoeticNumberLiteralElem::Word("a".into()),
                            PoeticNumberLiteralElem::Word("rockstar".into()),
                        ]
                    }
                    .into(),
                }
                .into(),
                2
            )
        ]))
    );
    assert_eq!(
        parse(
            "\
            Variable is 1
            Tommy is a rockstar
            "
        ),
        Ok(Block(vec![
            StatementWithLine(
                PoeticNumberAssignment {
                    dest: SimpleIdentifier("Variable".into()).into(),
                    rhs: LiteralExpression::Number(1.0).into()
                }
                .into(),
                1
            ),
            StatementWithLine(
                PoeticNumberAssignment {
                    dest: SimpleIdentifier("Tommy".into()).into(),
                    rhs: PoeticNumberLiteral {
                        elems: vec![
                            PoeticNumberLiteralElem::Word("a".into()),
                            PoeticNumberLiteralElem::Word("rockstar".into()),
                        ]
                    }
                    .into(),
                }
                .into(),
                2
            )
        ]))
    );

    assert_eq!(
        parse(
            "\
            Variable is 1
            Tommy is a rockstar
            
            "
        ),
        Ok(Block(vec![
            StatementWithLine(
                PoeticNumberAssignment {
                    dest: SimpleIdentifier("Variable".into()).into(),
                    rhs: LiteralExpression::Number(1.0).into()
                }
                .into(),
                1
            ),
            StatementWithLine(
                PoeticNumberAssignment {
                    dest: SimpleIdentifier("Tommy".into()).into(),
                    rhs: PoeticNumberLiteral {
                        elems: vec![
                            PoeticNumberLiteralElem::Word("a".into()),
                            PoeticNumberLiteralElem::Word("rockstar".into()),
                        ]
                    }
                    .into(),
                }
                .into(),
                2
            )
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
                    lhs: boxed_expr(SimpleIdentifier("x".into())),
                    rhs: boxed_expr(LiteralExpression::Number(5.0))
                }
                .into(),
                then_block: Block(vec![StatementWithLine(
                    PoeticNumberAssignment {
                        dest: SimpleIdentifier("x".into()).into(),
                        rhs: LiteralExpression::Number(6.0).into()
                    }
                    .into(),
                    2
                )]),
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
                    lhs: boxed_expr(SimpleIdentifier("x".into())),
                    rhs: boxed_expr(LiteralExpression::Number(5.0))
                }
                .into(),
                then_block: Block(vec![StatementWithLine(
                    PoeticNumberAssignment {
                        dest: SimpleIdentifier("x".into()).into(),
                        rhs: LiteralExpression::Number(6.0).into()
                    }
                    .into(),
                    2
                )]),
                else_block: Some(Block(vec![StatementWithLine(
                    PoeticNumberAssignment {
                        dest: SimpleIdentifier("x".into()).into(),
                        rhs: LiteralExpression::Number(7.0).into()
                    }
                    .into(),
                    4
                )])),
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
                    lhs: boxed_expr(SimpleIdentifier("x".into())),
                    rhs: boxed_expr(LiteralExpression::Number(5.0))
                }
                .into(),
                then_block: Block::empty(),
                else_block: Some(Block(vec![StatementWithLine(
                    PoeticNumberAssignment {
                        dest: SimpleIdentifier("x".into()).into(),
                        rhs: LiteralExpression::Number(7.0).into()
                    }
                    .into(),
                    3
                )])),
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
                    lhs: boxed_expr(SimpleIdentifier("x".into())),
                    rhs: boxed_expr(LiteralExpression::Number(5.0))
                }
                .into(),
                then_block: Block::empty(),
                else_block: Some(Block::empty()),
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
        Ok(Block(vec![
            StatementWithLine(
                While {
                    condition: BinaryExpression {
                        operator: BinaryOperator::Eq,
                        lhs: boxed_expr(SimpleIdentifier("x".into())),
                        rhs: boxed_expr(LiteralExpression::Number(5.0))
                    }
                    .into(),
                    block: Block(vec![StatementWithLine(
                        PoeticNumberAssignment {
                            dest: SimpleIdentifier("x".into()).into(),
                            rhs: LiteralExpression::Number(6.0).into()
                        }
                        .into(),
                        2
                    )])
                }
                .into(),
                1
            ),
            StatementWithLine(
                Until {
                    condition: BinaryExpression {
                        operator: BinaryOperator::Eq,
                        lhs: boxed_expr(SimpleIdentifier("x".into())),
                        rhs: boxed_expr(LiteralExpression::Number(5.0))
                    }
                    .into(),
                    block: Block(vec![StatementWithLine(
                        PoeticNumberAssignment {
                            dest: SimpleIdentifier("x".into()).into(),
                            rhs: LiteralExpression::Number(6.0).into()
                        }
                        .into(),
                        5
                    )])
                }
                .into(),
                4
            )
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
        Ok(Block(vec![
            StatementWithLine(
                While {
                    condition: BinaryExpression {
                        operator: BinaryOperator::Eq,
                        lhs: boxed_expr(SimpleIdentifier("x".into())),
                        rhs: boxed_expr(LiteralExpression::Number(5.0))
                    }
                    .into(),
                    block: Block(vec![StatementWithLine(
                        PoeticNumberAssignment {
                            dest: SimpleIdentifier("x".into()).into(),
                            rhs: LiteralExpression::Number(6.0).into()
                        }
                        .into(),
                        2
                    )])
                }
                .into(),
                1
            ),
            StatementWithLine(
                Until {
                    condition: BinaryExpression {
                        operator: BinaryOperator::Eq,
                        lhs: boxed_expr(SimpleIdentifier("x".into())),
                        rhs: boxed_expr(LiteralExpression::Number(5.0))
                    }
                    .into(),
                    block: Block(vec![StatementWithLine(
                        PoeticNumberAssignment {
                            dest: SimpleIdentifier("x".into()).into(),
                            rhs: LiteralExpression::Number(6.0).into()
                        }
                        .into(),
                        5
                    )])
                }
                .into(),
                4
            )
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
        Ok(Block(vec![StatementWithLine(
            While {
                condition: BinaryExpression {
                    operator: BinaryOperator::Eq,
                    lhs: boxed_expr(SimpleIdentifier("x".into())),
                    rhs: boxed_expr(LiteralExpression::Number(5.0))
                }
                .into(),
                block: Block(vec![
                    StatementWithLine(
                        While {
                            condition: BinaryExpression {
                                operator: BinaryOperator::Eq,
                                lhs: boxed_expr(SimpleIdentifier("y".into())),
                                rhs: boxed_expr(LiteralExpression::Number(6.0))
                            }
                            .into(),
                            block: Block(vec![StatementWithLine(
                                PoeticNumberAssignment {
                                    dest: SimpleIdentifier("y".into()).into(),
                                    rhs: LiteralExpression::Number(7.0).into()
                                }
                                .into(),
                                3
                            )])
                        }
                        .into(),
                        2
                    ),
                    StatementWithLine(
                        PoeticNumberAssignment {
                            dest: SimpleIdentifier("x".into()).into(),
                            rhs: LiteralExpression::Number(6.0).into()
                        }
                        .into(),
                        5
                    )
                ])
            }
            .into(),
            1
        ),]))
    );
}

#[test]
fn parse_build_knock() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(
        parse("build it up"),
        Ok(Some(
            Inc {
                dest: Identifier::Pronoun,
                amount: 1
            }
            .into()
        ))
    );
    assert_eq!(
        parse("build it up, up, up"),
        Ok(Some(
            Inc {
                dest: Identifier::Pronoun,
                amount: 3
            }
            .into()
        ))
    );
    assert_eq!(
        parse("knock it down"),
        Ok(Some(
            Dec {
                dest: Identifier::Pronoun,
                amount: 1
            }
            .into()
        ))
    );
    assert_eq!(
        parse("knock it down down down, down"),
        Ok(Some(
            Dec {
                dest: Identifier::Pronoun,
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
                value: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("shout it"),
        Ok(Some(
            Output {
                value: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("shout it at 255"),
        Ok(Some(
            Output {
                value: ArraySubscript {
                    array: boxed_expr(Identifier::Pronoun),
                    subscript: boxed_expr(LiteralExpression::Number(255.0))
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
                dest: Some(Identifier::Pronoun.into())
            }
            .into()
        ))
    );
    assert_eq!(
        parse("listen to it at night"),
        Ok(Some(
            Input {
                dest: Some(
                    ArraySubscript {
                        array: boxed_expr(Identifier::Pronoun),
                        subscript: boxed_expr(SimpleIdentifier("night".into())),
                    }
                    .into()
                )
            }
            .into()
        ))
    );
    assert_eq!(parse("listen"), Ok(Some(Input { dest: None }.into())));
}

#[test]
fn parse_mutation() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(
        parse("cut it"),
        Ok(Some(
            Mutation {
                operator: MutationOperator::Cut,
                operand: Identifier::Pronoun.into(),
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
                operand: Identifier::Pronoun.into(),
                dest: Some(SimpleIdentifier("pieces".into()).into()),
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
                operand: Identifier::Pronoun.into(),
                dest: None,
                param: Some(CommonIdentifier("my".into(), "knife".into()).into()),
            }
            .into()
        ))
    );
    assert_eq!(
        parse("cut it into pieces with my knife"),
        Ok(Some(
            Mutation {
                operator: MutationOperator::Cut,
                operand: Identifier::Pronoun.into(),
                dest: Some(SimpleIdentifier("pieces".into()).into()),
                param: Some(CommonIdentifier("my".into(), "knife".into()).into()),
            }
            .into()
        ))
    );
    assert_eq!(
        parse("cut it into pieces with my knife with my knife"),
        Ok(Some(
            Mutation {
                operator: MutationOperator::Cut,
                operand: Identifier::Pronoun.into(),
                dest: Some(SimpleIdentifier("pieces".into()).into()),
                param: Some(
                    BinaryExpression {
                        operator: BinaryOperator::Plus,
                        lhs: boxed_expr(CommonIdentifier("my".into(), "knife".into())),
                        rhs: boxed_expr(CommonIdentifier("my".into(), "knife".into())),
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
                LiteralExpression::String("2, 2".into()).into()
            ),
            Some(Token::new(TokenType::With, "with"))
        ))
    );

    assert_eq!(
        parse("join it"),
        Ok(Some(
            Mutation {
                operator: MutationOperator::Join,
                operand: Identifier::Pronoun.into(),
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
                operand: Identifier::Pronoun.into(),
                dest: Some(CommonIdentifier("the".into(), "fire".into()).into()),
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
                operand: Identifier::Pronoun.into(),
                dest: Some(
                    ArraySubscript {
                        array: boxed_expr(CommonIdentifier("the".into(), "fire".into())),
                        subscript: boxed_expr(ProperIdentifier(vec![
                            "Mount".into(),
                            "Doom".into()
                        ]))
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
                operand: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn up it."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Up,
                operand: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn it down."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Down,
                operand: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn down it."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Down,
                operand: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn it around."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Nearest,
                operand: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn around it."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Nearest,
                operand: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn it round."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Nearest,
                operand: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("Turn round it."),
        Ok(Some(
            Rounding {
                direction: RoundingDirection::Nearest,
                operand: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
}

#[test]
fn parse_break_continue() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(parse("break"), Ok(Some(Statement::Break)));
    assert_eq!(parse("break it down"), Ok(Some(Statement::Break)));
    assert_eq!(parse("BREAK IT DOWN"), Ok(Some(Statement::Break)));
    assert_eq!(parse("continue"), Ok(Some(Statement::Continue)));
    assert_eq!(parse("take it to the top"), Ok(Some(Statement::Continue)));
    assert_eq!(parse("TAKE IT TO THE TOP"), Ok(Some(Statement::Continue)));

    assert_eq!(
        parse("break it"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedToken(TokenType::Down),
            token: None
        })
    );
    assert_eq!(
        parse("take it to"),
        Err(ParseError {
            code: ParseErrorCode::ExpectedText("the".into()),
            token: None
        })
    );
}

#[test]
fn parse_array_push_pop() {
    let parse = |text| Parser::for_source_code(text).parse_statement();

    assert_eq!(
        parse("rock ints with 1"),
        Ok(Some(
            ArrayPush {
                array: SimpleIdentifier("ints".into()).into(),
                value: LiteralExpression::Number(1.0).into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("rock ints with 1, 2, 3"),
        Ok(Some(
            ArrayPush {
                array: SimpleIdentifier("ints".into()).into(),
                value: ExpressionList {
                    first: LiteralExpression::Number(1.0).into(),
                    rest: vec![
                        LiteralExpression::Number(2.0).into(),
                        LiteralExpression::Number(3.0).into()
                    ]
                }
                .into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("rock ints with 1, 2 with 3, 4, 5"),
        Ok(Some(
            ArrayPush {
                array: SimpleIdentifier("ints".into()).into(),
                value: ExpressionList {
                    first: LiteralExpression::Number(1.0).into(),
                    rest: vec![
                        BinaryExpression {
                            operator: BinaryOperator::Plus,
                            lhs: boxed_expr(LiteralExpression::Number(2.0)),
                            rhs: boxed_expr(LiteralExpression::Number(3.0))
                        }
                        .into(),
                        LiteralExpression::Number(4.0).into(),
                        LiteralExpression::Number(5.0).into(),
                    ]
                }
                .into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("rock you like a hurricane"),
        Ok(Some(
            ArrayPush {
                array: SimpleIdentifier("you".into()).into(),
                value: PoeticNumberLiteral {
                    elems: vec![
                        PoeticNumberLiteralElem::Word("a".into()),
                        PoeticNumberLiteralElem::Word("hurricane".into())
                    ]
                }
                .into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("rock you like nothing"),
        Ok(Some(
            ArrayPush {
                array: SimpleIdentifier("you".into()).into(),
                value: PoeticNumberLiteral {
                    elems: vec![PoeticNumberLiteralElem::Word("nothing".into())]
                }
                .into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("rock ints at night with 1"),
        Ok(Some(
            ArrayPush {
                array: ArraySubscript {
                    array: boxed_expr(SimpleIdentifier("ints".into())),
                    subscript: boxed_expr(SimpleIdentifier("night".into()))
                }
                .into(),
                value: LiteralExpression::Number(1.0).into()
            }
            .into()
        ))
    );

    assert_eq!(
        parse("roll ints"),
        Ok(Some(
            ArrayPop {
                array: SimpleIdentifier("ints".into()).into(),
                dest: None
            }
            .into()
        ))
    );
    assert_eq!(
        parse("roll ints into it"),
        Ok(Some(
            ArrayPop {
                array: SimpleIdentifier("ints".into()).into(),
                dest: Some(Identifier::Pronoun.into())
            }
            .into()
        ))
    );
    assert_eq!(
        parse("roll ints at night"),
        Ok(Some(
            ArrayPop {
                array: ArraySubscript {
                    array: boxed_expr(SimpleIdentifier("ints".into())),
                    subscript: boxed_expr(SimpleIdentifier("night".into()))
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
                value: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("give it"),
        Ok(Some(
            Return {
                value: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("give it back"),
        Ok(Some(
            Return {
                value: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("give back it"),
        Ok(Some(
            Return {
                value: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("give back it back"),
        Ok(Some(
            Return {
                value: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("return it back"),
        Ok(Some(
            Return {
                value: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("send it back"),
        Ok(Some(
            Return {
                value: Identifier::Pronoun.into()
            }
            .into()
        ))
    );
    assert_eq!(
        parse("send back it"),
        Err(ParseError::new(
            ParseErrorCode::ExpectedPrimaryExpression,
            Some(Token::new(TokenType::Back, "back"))
        ))
    );
    assert_eq!(
        parse("give back X with Y"),
        Ok(Some(
            Return {
                value: BinaryExpression {
                    operator: BinaryOperator::Plus,
                    lhs: boxed_expr(SimpleIdentifier("X".into())),
                    rhs: boxed_expr(SimpleIdentifier("Y".into()))
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
                name: SimpleIdentifier("Echo".into()).into(),
                params: vec![SimpleIdentifier("X".into()).into()],
                body: Block(vec![StatementWithLine(
                    Output {
                        value: SimpleIdentifier("X".into()).into()
                    }
                    .into(),
                    2
                )])
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
                name: CommonIdentifier("my".into(), "heart".into()).into(),
                params: vec![SimpleIdentifier("X".into()).into()],
                body: Block(vec![StatementWithLine(
                    Output {
                        value: SimpleIdentifier("X".into()).into()
                    }
                    .into(),
                    2
                )])
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
                name: ProperIdentifier(vec!["Tom".into(), "Sawyer".into()]).into(),
                params: vec![CommonIdentifier("my".into(), "body".into()).into()],
                body: Block(vec![StatementWithLine(
                    Output {
                        value: SimpleIdentifier("X".into()).into()
                    }
                    .into(),
                    2
                )])
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
                name: SimpleIdentifier("AddOrSub".into()).into(),
                params: vec![
                    SimpleIdentifier("X".into()).into(),
                    SimpleIdentifier("B".into()).into()
                ],
                body: Block(vec![
                    StatementWithLine(
                        If {
                            condition: SimpleIdentifier("B".into()).into(),
                            then_block: Block(vec![StatementWithLine(
                                Return {
                                    value: BinaryExpression {
                                        operator: BinaryOperator::Plus,
                                        lhs: boxed_expr(SimpleIdentifier("X".into())),
                                        rhs: boxed_expr(LiteralExpression::Number(1.0))
                                    }
                                    .into()
                                }
                                .into(),
                                3
                            )]),
                            else_block: None,
                        }
                        .into(),
                        2
                    ),
                    StatementWithLine(
                        Output {
                            value: LiteralExpression::String("else".into()).into()
                        }
                        .into(),
                        5
                    ),
                    StatementWithLine(
                        Return {
                            value: BinaryExpression {
                                operator: BinaryOperator::Minus,
                                lhs: boxed_expr(SimpleIdentifier("X".into())),
                                rhs: boxed_expr(LiteralExpression::Number(1.0))
                            }
                            .into()
                        }
                        .into(),
                        6
                    )
                ])
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
            name: SimpleIdentifier("F".into()).into(),
            args: vec![SimpleIdentifier("X".into()).into()]
        }
        .into())
    );

    assert_eq!(
        parse("my heart taking X, Y, and Z, and 12"),
        Ok(FunctionCall {
            name: CommonIdentifier("my".into(), "heart".into()).into(),
            args: vec![
                SimpleIdentifier("X".into()).into(),
                SimpleIdentifier("Y".into()).into(),
                SimpleIdentifier("Z".into()).into(),
                LiteralExpression::Number(12.0).into(),
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
                name: SimpleIdentifier("F".into()).into(),
                args: vec![SimpleIdentifier("X".into()).into()]
            }
            .into()
        ))
    );

    assert_eq!(
        parse("my heart taking X, Y, and Z, and 12"),
        Ok(Some(
            FunctionCall {
                name: CommonIdentifier("my".into(), "heart".into()).into(),
                args: vec![
                    SimpleIdentifier("X".into()).into(),
                    SimpleIdentifier("Y".into()).into(),
                    SimpleIdentifier("Z".into()).into(),
                    LiteralExpression::Number(12.0).into(),
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
                Block(vec![StatementWithLine(
                    Assignment {
                        dest: SimpleIdentifier("X".into()).into(),
                        value: SimpleIdentifier("Y".into()).into(),
                        operator: None
                    }
                    .into(),
                    1
                )])
                .into(),
                Block(vec![StatementWithLine(
                    Assignment {
                        dest: SimpleIdentifier("Y".into()).into(),
                        value: SimpleIdentifier("Z".into()).into(),
                        operator: None
                    }
                    .into(),
                    3
                )])
                .into(),
                Block(vec![StatementWithLine(
                    Assignment {
                        dest: SimpleIdentifier("Z".into()).into(),
                        value: SimpleIdentifier("Z".into()).into(),
                        operator: None
                    }
                    .into(),
                    6
                )])
                .into()
            ]
        })
    );
}
