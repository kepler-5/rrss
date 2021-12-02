use std::iter::Peekable;

use crate::{
    ast::{
        BinaryExpression, BinaryOperator, CommonIdentifier, Expression, LiteralExpression,
        PrimaryExpression, Program, SimpleIdentifier, UnaryExpression, UnaryOperator,
    },
    lexer::{CommentSkippingLexer, Lexer, Token},
};

#[derive(Debug, PartialEq)]
pub struct ParseError<'a> {
    pub token: Option<Token<'a>>,
    pub message: String,
}

impl<'a> ParseError<'a> {
    pub fn new<S: Into<String>>(token: Option<Token<'a>>, message: S) -> Self {
        Self {
            token,
            message: message.into(),
        }
    }

    pub fn saying<S: Into<String>>(message: S) -> Self {
        Self::new(None, message)
    }
}

pub struct Parser<'a> {
    lexer: Peekable<CommentSkippingLexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: CommentSkippingLexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }

    pub fn for_source_code(text: &'a str) -> Self {
        Self::new(Lexer::new(text).skip_comments())
    }

    pub fn parse(mut self) -> Result<Program, ParseError<'a>> {
        Ok(Program {
            code: vec![self.parse_expression()?],
        })
    }
}

fn get_unary_operator(token: Token) -> Option<UnaryOperator> {
    match token {
        Token::Minus => Some(UnaryOperator::Minus),

        _ => None,
    }
}

fn get_binary_operator(token: Token) -> Option<BinaryOperator> {
    match token {
        Token::Plus => Some(BinaryOperator::Plus),
        Token::Minus => Some(BinaryOperator::Minus),
        Token::Multiply => Some(BinaryOperator::Multiply),
        Token::Divide => Some(BinaryOperator::Divide),

        _ => None,
    }
}

fn boxed_expr(x: impl Into<Expression>) -> Box<Expression> {
    Box::new(x.into())
}

impl<'a> Parser<'a> {
    fn match_and_consume_if<F: FnOnce(&Token) -> bool>(&mut self, f: F) -> Option<Token> {
        if self.lexer.peek().filter(|tok| f(*tok)).is_some() {
            self.lexer.next()
        } else {
            None
        }
    }

    fn match_and_consume(&mut self, tokens: &[Token]) -> Option<Token> {
        self.match_and_consume_if(|token| tokens.contains(token))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError<'a>> {
        self.parse_term()
    }

    fn parse_term(&mut self) -> Result<Expression, ParseError<'a>> {
        self.parse_binary_expression(&[Token::Plus, Token::Minus], |p| p.parse_factor())
    }

    fn parse_factor(&mut self) -> Result<Expression, ParseError<'a>> {
        self.parse_binary_expression(&[Token::Multiply, Token::Divide], |p| {
            p.parse_unary_expression()
        })
    }

    fn parse_binary_expression<F>(
        &mut self,
        operators: &[Token],
        next: F,
    ) -> Result<Expression, ParseError<'a>>
    where
        F: Fn(&mut Parser<'a>) -> Result<Expression, ParseError<'a>>,
    {
        let mut expr = next(self)?;
        while let Some(operator) = self
            .match_and_consume(operators)
            .map(|token| get_binary_operator(token).unwrap())
        {
            let rhs = boxed_expr(next(self)?);
            expr = BinaryExpression {
                operator,
                lhs: boxed_expr(expr),
                rhs,
            }
            .into();
        }
        Ok(expr)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, ParseError<'a>> {
        if let Some(operator) = self
            .match_and_consume(&[Token::Minus])
            .map(|token| get_unary_operator(token).unwrap())
        {
            Ok(UnaryExpression {
                operator,
                operand: boxed_expr(self.parse_unary_expression()?),
            }
            .into())
        } else {
            self.parse_primary_expression().map(Into::into)
        }
    }

    fn parse_primary_expression(&mut self) -> Result<PrimaryExpression, ParseError<'a>> {
        self.parse_identifier()?
            .or_else(|| {
                self.parse_pronoun()
                    .or_else(|| self.parse_literal_expression().map(Into::into))
            })
            .ok_or_else(|| ParseError::saying("Expected primary expression"))
    }

    fn parse_pronoun(&mut self) -> Option<PrimaryExpression> {
        self.match_and_consume(&[Token::Pronoun])
            .map(|_| PrimaryExpression::Pronoun)
    }

    fn parse_literal_expression(&mut self) -> Option<LiteralExpression> {
        self.lexer.next().and_then(|token| match token {
            Token::Null => Some(LiteralExpression::Null),
            Token::Number(n) => Some(LiteralExpression::Number(n)),
            Token::StringLiteral(s) => Some(LiteralExpression::String(s.to_owned())),
            Token::True => Some(LiteralExpression::Boolean(true)),
            Token::False => Some(LiteralExpression::Boolean(false)),
            _ => None,
        })
    }

    fn match_and_extract_text<F: FnOnce(&Token) -> bool>(&mut self, f: F) -> Option<String> {
        self.match_and_consume_if(f).and_then(|tok| match tok {
            Token::Word(text) => Some(text.to_owned()),
            Token::CommonVariablePrefix(text) => Some(text.to_owned()),

            _ => None,
        })
    }

    fn parse_identifier(&mut self) -> Result<Option<PrimaryExpression>, ParseError<'a>> {
        if let Some(prefix) =
            self.match_and_extract_text(|tok| matches!(tok, Token::CommonVariablePrefix(_)))
        {
            let identifier = Some(
                self.match_and_extract_text(|tok| matches!(tok, Token::Word(_)))
                    .ok_or_else(|| {
                        ParseError::saying("Expected identifier after '".to_owned() + &prefix + "'")
                    })?,
            )
            .filter(|text| text.chars().all(|c| c.is_ascii_lowercase()))
            .ok_or_else(|| {
                ParseError::saying(
                    "Common variables must be all-lowercase (after '".to_owned() + &prefix + "')",
                )
            })?;
            Ok(Some(CommonIdentifier(prefix, identifier).into()))
        } else {
            Ok(None)
        }
    }
}

pub fn parse(text: &str) -> Result<Program, ParseError> {
    Parser::for_source_code(text).parse()
}

#[test]
fn parse_literal_expression() {
    let parse = |text| Parser::for_source_code(text).parse_literal_expression();
    assert_eq!(parse("null"), Some(LiteralExpression::Null));
    assert_eq!(parse("true"), Some(LiteralExpression::Boolean(true)));
    assert_eq!(parse("false"), Some(LiteralExpression::Boolean(false)));
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
}

#[test]
fn parse_binary_expression() {
    let parse = |text| Parser::for_source_code(text).parse_expression();
    assert_eq!(
        parse("1 + 1"),
        Ok(Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }))
    );
    assert_eq!(
        parse("1 - 1"),
        Ok(Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Minus,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }))
    );
    assert_eq!(
        parse("1 - -1"),
        Ok(Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Minus,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(UnaryExpression {
                operator: UnaryOperator::Minus,
                operand: boxed_expr(LiteralExpression::Number(1.0))
            })
        }))
    );
    assert_eq!(
        parse("1 * 1"),
        Ok(Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Multiply,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }))
    );
    assert_eq!(
        parse("1 / 1"),
        Ok(Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Divide,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }))
    );

    // associativity
    assert_eq!(
        parse("1 + 1 + 1"),
        Ok(Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(Expression::Binary(BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            })),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }))
    );
    assert_eq!(
        parse("1 - 1 - 1"),
        Ok(Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Minus,
            lhs: boxed_expr(Expression::Binary(BinaryExpression {
                operator: BinaryOperator::Minus,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            })),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }))
    );
    assert_eq!(
        parse("1 * 1 + 1"),
        Ok(Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(Expression::Binary(BinaryExpression {
                operator: BinaryOperator::Multiply,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            })),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }))
    );
    assert_eq!(
        parse("1 / 1 + 1"),
        Ok(Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(Expression::Binary(BinaryExpression {
                operator: BinaryOperator::Divide,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            })),
            rhs: boxed_expr(LiteralExpression::Number(1.0))
        }))
    );
    assert_eq!(
        parse("1 + 1 * 1"),
        Ok(Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(Expression::Binary(BinaryExpression {
                operator: BinaryOperator::Multiply,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            })),
        }))
    );
    assert_eq!(
        parse("1 + 1 / 1"),
        Ok(Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Plus,
            lhs: boxed_expr(LiteralExpression::Number(1.0)),
            rhs: boxed_expr(Expression::Binary(BinaryExpression {
                operator: BinaryOperator::Divide,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            })),
        }))
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
        parse("my"),
        Err(ParseError::saying("Expected identifier after 'my'"))
    );
    assert_eq!(
        parse("your"),
        Err(ParseError::saying("Expected identifier after 'your'"))
    );

    assert_eq!(
        parse("my Heart"),
        Err(ParseError::saying(
            "Common variables must be all-lowercase (after 'my')"
        ))
    );
    assert_eq!(
        parse("your Heart"),
        Err(ParseError::saying(
            "Common variables must be all-lowercase (after 'your')"
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
            Some(PrimaryExpression::Pronoun)
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
    )
}
