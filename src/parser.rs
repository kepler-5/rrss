use std::iter::Peekable;

use crate::{
    ast::{
        Assignment, BinaryExpression, BinaryOperator, CommonIdentifier, Expression, Identifier,
        LiteralExpression, PrimaryExpression, Program, ProperIdentifier, SimpleIdentifier,
        Statement, UnaryExpression, UnaryOperator,
    },
    lexer::{CommentSkippingLexer, Lexer, Token, TokenType},
};

#[derive(Clone, Debug, PartialEq)]
pub enum ParseErrorCode<'a> {
    Generic(String),
    MissingIDAfterCommonPrefix(String),
    UppercaseAfterCommonPrefix(String, String),
    ExpectedIdentifier,
    ExpectedToken(TokenType<'a>),
    UnexpectedToken,
    UnexpectedEndOfTokens,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParseError<'a> {
    pub code: ParseErrorCode<'a>,
    pub token: Option<Token<'a>>,
}

impl<'a> ParseError<'a> {
    pub fn new(code: ParseErrorCode<'a>, token: Option<Token<'a>>) -> Self {
        Self { code, token }
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
            code: vec![self.parse_statement()?],
        })
    }
}

fn get_unary_operator(token: TokenType) -> Option<UnaryOperator> {
    match token {
        TokenType::Minus => Some(UnaryOperator::Minus),

        _ => None,
    }
}

fn get_binary_operator(token: TokenType) -> Option<BinaryOperator> {
    match token {
        TokenType::Plus | TokenType::With => Some(BinaryOperator::Plus),
        TokenType::Minus => Some(BinaryOperator::Minus),
        TokenType::Multiply => Some(BinaryOperator::Multiply),
        TokenType::Divide => Some(BinaryOperator::Divide),

        _ => None,
    }
}

fn boxed_expr(x: impl Into<Expression>) -> Box<Expression> {
    Box::new(x.into())
}

fn take_first<T>(vec: Vec<T>) -> T {
    assert!(!vec.is_empty());
    vec.into_iter().next().unwrap()
}

impl<'a> Parser<'a> {
    fn current(&mut self) -> Option<Token<'a>> {
        self.lexer.peek().copied()
    }

    fn new_parse_error(&mut self, code: ParseErrorCode<'a>) -> ParseError<'a> {
        ParseError::new(code, self.current())
    }

    fn match_and_consume_if<F: FnOnce(&Token) -> bool>(&mut self, f: F) -> Option<Token<'a>> {
        if self.lexer.peek().filter(|tok| f(*tok)).is_some() {
            self.lexer.next()
        } else {
            None
        }
    }

    fn match_and_consume_any(&mut self, tokens: &[TokenType]) -> Option<Token<'a>> {
        self.match_and_consume_if(|token| tokens.contains(&token.id))
    }

    fn match_and_consume(&mut self, token: TokenType) -> Option<Token<'a>> {
        self.match_and_consume_if(|tok| tok.id == token)
    }

    fn match_and_consume_while<F, R, Tx>(&mut self, f: F, tx: Tx) -> Vec<R>
    where
        F: Fn(&Token) -> bool,
        Tx: Fn(&Token) -> R,
    {
        let mut result = Vec::new();
        while let Some(token) = self.match_and_consume_if(&f) {
            result.push(tx(&token));
        }
        result
    }

    // step past a token we *know* satisfies the predicate
    fn consume_if<F: FnOnce(&Token) -> bool>(&mut self, f: F) {
        let tok = self.lexer.next();
        assert!(tok.filter(f).is_some());
    }
    fn consume(&mut self, token: TokenType) {
        self.consume_if(|tok| tok.id == token)
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError<'a>> {
        self.parse_term()
    }

    fn parse_term(&mut self) -> Result<Expression, ParseError<'a>> {
        self.parse_binary_expression(&[TokenType::Plus, TokenType::Minus], |p| p.parse_factor())
    }

    fn parse_factor(&mut self) -> Result<Expression, ParseError<'a>> {
        self.parse_binary_expression(&[TokenType::Multiply, TokenType::Divide], |p| {
            p.parse_unary_expression()
        })
    }

    fn parse_binary_expression<F>(
        &mut self,
        operators: &[TokenType],
        next: F,
    ) -> Result<Expression, ParseError<'a>>
    where
        F: Fn(&mut Parser<'a>) -> Result<Expression, ParseError<'a>>,
    {
        let mut expr = next(self)?;
        while let Some(operator) = self
            .match_and_consume_any(operators)
            .map(|token| get_binary_operator(token.id).unwrap())
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
            .match_and_consume(TokenType::Minus)
            .map(|token| get_unary_operator(token.id).unwrap())
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
            .map(Into::into)
            .or_else(|| {
                self.parse_pronoun()
                    .or_else(|| self.parse_literal_expression().map(Into::into))
            })
            .ok_or_else(|| {
                self.new_parse_error(ParseErrorCode::Generic(
                    "Expected primary expression".into(),
                ))
            })
    }

    fn parse_pronoun(&mut self) -> Option<PrimaryExpression> {
        self.match_and_consume(TokenType::Pronoun)
            .map(|_| PrimaryExpression::Pronoun)
    }

    fn parse_literal_expression(&mut self) -> Option<LiteralExpression> {
        self.lexer.next().and_then(|token| match token.id {
            TokenType::Null => Some(LiteralExpression::Null),
            TokenType::Number(n) => Some(LiteralExpression::Number(n)),
            TokenType::StringLiteral(s) => Some(LiteralExpression::String(s.to_owned())),
            TokenType::Empty => Some(LiteralExpression::String(String::new())),
            TokenType::True => Some(LiteralExpression::Boolean(true)),
            TokenType::False => Some(LiteralExpression::Boolean(false)),
            _ => None,
        })
    }

    fn match_and_extract_text<F: FnOnce(&Token) -> bool>(&mut self, f: F) -> Option<String> {
        self.match_and_consume_if(f)
            .map(|tok| tok.spelling.to_owned())
    }

    fn parse_common_identifier(&mut self) -> Result<Option<CommonIdentifier>, ParseError<'a>> {
        if let Some(prefix) = self
            .match_and_consume(TokenType::CommonVariablePrefix)
            .map(|tok| tok.spelling)
        {
            let next_word = self
                .match_and_consume_any(&[TokenType::Word, TokenType::CapitalizedWord]) // capitalized words are invalid, but we want to match them for good error messages
                .map(|tok| tok.spelling)
                .ok_or_else(|| {
                    self.new_parse_error(ParseErrorCode::MissingIDAfterCommonPrefix(prefix.into()))
                })?;
            let identifier = next_word
                .chars()
                .all(|c| c.is_ascii_lowercase())
                .then(|| next_word)
                .ok_or_else(|| {
                    self.new_parse_error(ParseErrorCode::UppercaseAfterCommonPrefix(
                        prefix.into(),
                        next_word.into(),
                    ))
                })?;
            Ok(Some(CommonIdentifier(prefix.into(), identifier.into())))
        } else {
            Ok(None)
        }
    }

    fn parse_simple_identifier(&mut self) -> Option<SimpleIdentifier> {
        self.match_and_consume_any(&[TokenType::Word, TokenType::CapitalizedWord])
            .map(|tok| SimpleIdentifier(tok.spelling.into()))
    }

    fn parse_capitalized_identifier(&mut self) -> Option<Identifier> {
        let names = self.match_and_consume_while(
            |tok| tok.id == TokenType::CapitalizedWord,
            |tok| tok.spelling.to_owned(),
        );
        match names.len() {
            0 => None,
            1 => Some(SimpleIdentifier(take_first(names)).into()),
            _ => Some(ProperIdentifier(names).into()),
        }
    }

    fn parse_identifier(&mut self) -> Result<Option<Identifier>, ParseError<'a>> {
        Ok(self.parse_common_identifier()?.map(Into::into).or_else(|| {
            self.parse_capitalized_identifier()
                .or_else(|| self.parse_simple_identifier().map(Into::into))
        }))
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError<'a>> {
        let current_token = self
            .current()
            .ok_or_else(|| self.new_parse_error(ParseErrorCode::UnexpectedEndOfTokens))?;
        Ok(match current_token.id {
            TokenType::Put => self.parse_put_assignment()?.into(),
            TokenType::Let => self.parse_let_assignment()?.into(),
            TokenType::Word | TokenType::CapitalizedWord | TokenType::CommonVariablePrefix => {
                self.parse_basic_assignment()?.into()
            }

            _ => {
                let error = self.new_parse_error(ParseErrorCode::UnexpectedToken);
                self.lexer.next();
                Err(error)?
            }
        })
    }

    fn expect_token(&mut self, tok: TokenType<'a>) -> Result<Token<'a>, ParseError<'a>> {
        self.match_and_consume(tok)
            .ok_or_else(|| self.new_parse_error(ParseErrorCode::ExpectedToken(tok)))
    }

    fn expect_token_or_aliases(
        &mut self,
        token: TokenType<'a>,
        aliases: &[TokenType<'a>],
    ) -> Result<Token<'a>, ParseError<'a>> {
        self.match_and_consume_if(|tok| tok.id == token || aliases.contains(&tok.id))
            .ok_or_else(|| self.new_parse_error(ParseErrorCode::ExpectedToken(token)))
    }

    fn expect_identifier(&mut self) -> Result<Identifier, ParseError<'a>> {
        self.parse_identifier()?
            .ok_or_else(|| self.new_parse_error(ParseErrorCode::ExpectedIdentifier))
    }

    fn parse_put_assignment(&mut self) -> Result<Assignment, ParseError<'a>> {
        self.consume(TokenType::Put);
        let value = boxed_expr(self.parse_expression()?);
        self.expect_token(TokenType::Into)?;
        let dest = self.expect_identifier()?;
        Ok(Assignment {
            dest,
            value,
            operator: None,
        })
    }
    fn parse_let_assignment(&mut self) -> Result<Assignment, ParseError<'a>> {
        self.consume(TokenType::Let);
        let dest = self.expect_identifier()?;
        self.expect_token(TokenType::Be)?;
        let operator = self
            .match_and_consume_any(&[
                TokenType::Plus,
                TokenType::With,
                TokenType::Minus,
                TokenType::Multiply,
                TokenType::Divide,
            ])
            .map(|tok| get_binary_operator(tok.id).unwrap());
        let value = boxed_expr(self.parse_expression()?);
        Ok(Assignment {
            dest,
            value,
            operator,
        })
    }
    fn parse_basic_assignment(&mut self) -> Result<Assignment, ParseError<'a>> {
        // not using expect_identifier since we know for sure the first token is a Word or CapitalizedWord,
        // so we either have an identifier or a parse error at the beginning
        let dest = self.parse_identifier()?.unwrap();
        self.expect_token_or_aliases(
            TokenType::Is,
            &[TokenType::ApostropheS, TokenType::ApostropheRE],
        )?;
        let value = boxed_expr(self.parse_expression()?);
        Ok(Assignment {
            dest,
            value,
            operator: None,
        })
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
    );
}

#[test]
fn parse_assignment() {
    let parse = |text| Parser::for_source_code(text).parse_statement();
    assert_eq!(
        parse("Variable is 1"),
        Ok(Assignment {
            dest: SimpleIdentifier("Variable".into()).into(),
            value: boxed_expr(LiteralExpression::Number(1.0)),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Tommy is a rockstar"),
        Ok(Assignment {
            dest: SimpleIdentifier("Tommy".into()).into(),
            value: boxed_expr(CommonIdentifier("a".into(), "rockstar".into())), // might be wrong--this should be a poetic literal instead?
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("X is 2"),
        Ok(Assignment {
            dest: SimpleIdentifier("X".into()).into(),
            value: boxed_expr(LiteralExpression::Number(2.0)),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Y is 3"),
        Ok(Assignment {
            dest: SimpleIdentifier("Y".into()).into(),
            value: boxed_expr(LiteralExpression::Number(3.0)),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("noise is silence"),
        Ok(Assignment {
            dest: SimpleIdentifier("noise".into()).into(),
            value: boxed_expr(LiteralExpression::String(String::new())),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Put x plus y into result"),
        Ok(Assignment {
            dest: SimpleIdentifier("result".into()).into(),
            value: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(SimpleIdentifier("x".into())),
                rhs: boxed_expr(SimpleIdentifier("y".into()))
            }),
            operator: None
        }
        .into())
    );

    assert_eq!(
        parse("Variable's 1"),
        Ok(Assignment {
            dest: SimpleIdentifier("Variable".into()).into(),
            value: boxed_expr(LiteralExpression::Number(1.0)),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Variables are 1"),
        Ok(Assignment {
            dest: SimpleIdentifier("Variables".into()).into(),
            value: boxed_expr(LiteralExpression::Number(1.0)),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("We're 1"),
        Ok(Assignment {
            dest: SimpleIdentifier("We".into()).into(),
            value: boxed_expr(LiteralExpression::Number(1.0)),
            operator: None
        }
        .into())
    );

    assert_eq!(
        parse("Put 123 into X"),
        Ok(Assignment {
            dest: SimpleIdentifier("X".into()).into(),
            value: boxed_expr(LiteralExpression::Number(123.0)),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Put \"Hello San Francisco\" into the message"),
        Ok(Assignment {
            dest: CommonIdentifier("the".into(), "message".into()).into(),
            value: boxed_expr(LiteralExpression::String("Hello San Francisco".into())),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Let my balance be 1000000"),
        Ok(Assignment {
            dest: CommonIdentifier("my".into(), "balance".into()).into(),
            value: boxed_expr(LiteralExpression::Number(1000000.0)),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("Let the survivors be the brave without the fallen"),
        Ok(Assignment {
            dest: CommonIdentifier("the".into(), "survivors".into()).into(),
            value: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Minus,
                lhs: boxed_expr(CommonIdentifier("the".into(), "brave".into())),
                rhs: boxed_expr(CommonIdentifier("the".into(), "fallen".into()))
            }),
            operator: None
        }
        .into())
    );

    assert_eq!(
        parse("Let X be with 10"),
        Ok(Assignment {
            dest: SimpleIdentifier("X".into()).into(),
            value: boxed_expr(LiteralExpression::Number(10.0)),
            operator: Some(BinaryOperator::Plus),
        }
        .into())
    );
    assert_eq!(
        parse("Let the children be without fear"),
        Ok(Assignment {
            dest: CommonIdentifier("the".into(), "children".into()).into(),
            value: boxed_expr(SimpleIdentifier("fear".into())),
            operator: Some(BinaryOperator::Minus),
        }
        .into())
    );
    assert_eq!(
        parse("Let my heart be over the moon"),
        Ok(Assignment {
            dest: CommonIdentifier("my".into(), "heart".into()).into(),
            value: boxed_expr(CommonIdentifier("the".into(), "moon".into())),
            operator: Some(BinaryOperator::Divide),
        }
        .into())
    );

    assert_eq!(
        parse("Put the whole of your heart into my hands"),
        Ok(Assignment {
            dest: CommonIdentifier("my".into(), "hands".into()).into(),
            value: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Multiply,
                lhs: boxed_expr(CommonIdentifier("the".into(), "whole".into())),
                rhs: boxed_expr(CommonIdentifier("your".into(), "heart".into()))
            }),
            operator: None
        }
        .into())
    );
    assert_eq!(
        parse("My world is nothing without your love"),
        Ok(Assignment {
            dest: CommonIdentifier("My".into(), "world".into()).into(),
            value: boxed_expr(BinaryExpression {
                operator: BinaryOperator::Minus,
                lhs: boxed_expr(LiteralExpression::Null),
                rhs: boxed_expr(CommonIdentifier("your".into(), "love".into()))
            }),
            operator: None
        }
        .into())
    );
}
