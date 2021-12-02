use std::{boxed, iter::Peekable};

use crate::{
    ast::{
        BinaryExpression, BinaryOperator, Expression, LiteralExpression, PrimaryExpression,
        Program, UnaryExpression, UnaryOperator,
    },
    lexer::{CommentSkippingLexer, Lexer, Token},
};

#[derive(Debug, PartialEq)]
pub struct ParseError<'a> {
    pub token: Option<Token<'a>>,
    pub message: &'a str,
}

impl<'a> ParseError<'a> {
    pub fn new(token: Option<Token<'a>>, message: &'a str) -> Self {
        Self { token, message }
    }

    pub fn saying(message: &'a str) -> Self {
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
    fn match_and_consume(&mut self, tokens: &[Token]) -> Option<Token> {
        if self
            .lexer
            .peek()
            .filter(|token| tokens.contains(token))
            .is_some()
        {
            self.lexer.next()
        } else {
            None
        }
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
        self.parse_literal_expression().map(Into::into)
    }

    fn parse_literal_expression(&mut self) -> Result<LiteralExpression, ParseError<'a>> {
        let error = || ParseError::saying("Expected literal");
        self.lexer
            .next()
            .ok_or_else(error)
            .and_then(|token| match token {
                Token::Null => Ok(LiteralExpression::Null),
                Token::Number(n) => Ok(LiteralExpression::Number(n)),
                Token::StringLiteral(s) => Ok(LiteralExpression::String(s.to_owned())),
                Token::True => Ok(LiteralExpression::Boolean(true)),
                Token::False => Ok(LiteralExpression::Boolean(false)),
                _ => Err(error()),
            })
    }
}

pub fn parse(text: &str) -> Result<Program, ParseError> {
    Parser::for_source_code(text).parse()
}

#[test]
fn parse_literal_expression() {
    let parse = |text| Parser::for_source_code(text).parse_literal_expression();
    assert_eq!(parse("null"), Ok(LiteralExpression::Null));
    assert_eq!(parse("true"), Ok(LiteralExpression::Boolean(true)));
    assert_eq!(parse("false"), Ok(LiteralExpression::Boolean(false)));
    assert_eq!(parse("\"\""), Ok(LiteralExpression::String("".to_owned())));
    assert_eq!(parse("5"), Ok(LiteralExpression::Number(5.0)));

    assert_eq!(parse(""), Err(ParseError::saying("Expected literal")));
    assert_eq!(parse("foo"), Err(ParseError::saying("Expected literal")));
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
