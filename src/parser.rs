use std::ops::Not;

use itertools::Itertools;

use crate::{
    ast::{
        ArrayPop, ArrayPush, ArrayPushRHS, ArraySubscript, Assignment, AssignmentLHS,
        BinaryExpression, BinaryOperator, Block, CommonIdentifier, Dec, Expression, ExpressionList,
        Identifier, If, Inc, Input, LiteralExpression, Mutation, MutationOperator, Output,
        PoeticAssignment, PoeticNumberAssignment, PoeticNumberAssignmentRHS, PoeticNumberLiteral,
        PoeticNumberLiteralElem, PoeticStringAssignment, PrimaryExpression, Program,
        ProperIdentifier, Rounding, RoundingDirection, SimpleIdentifier, Statement,
        StatementWithLine, UnaryExpression, UnaryOperator, Until, While,
    },
    lexer::{self, CommentSkippingLexer, Lexer, Token, TokenType},
};

trait MatchesToken {
    fn matches_token(&self, token: &Token) -> bool;
}

impl<'a> MatchesToken for Token<'a> {
    fn matches_token(&self, token: &Token) -> bool {
        self == token
    }
}

impl<'a> MatchesToken for TokenType<'a> {
    fn matches_token(&self, token: &Token) -> bool {
        *self == token.id
    }
}

impl<F: Fn(&Token) -> bool> MatchesToken for F {
    fn matches_token(&self, token: &Token) -> bool {
        (*self)(token)
    }
}

impl<'a> MatchesToken for &[TokenType<'a>] {
    fn matches_token(&self, token: &Token) -> bool {
        self.contains(&token.id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParseErrorCode<'a> {
    Generic(String),
    MissingIDAfterCommonPrefix(String),
    UppercaseAfterCommonPrefix(String, String),
    MutationOperandMustBeIdentifier(PrimaryExpression),
    ListExpressionWithoutOperator,
    ExpectedPrimaryExpression,
    ExpectedIdentifier,
    ExpectedText(String),
    ExpectedToken(TokenType<'a>),
    ExpectedOneOfTokens(Vec<TokenType<'a>>),
    ExpectedPoeticNumberLiteral,
    ExpectedSpaceAfterSays(Token<'a>),
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
    lexer: CommentSkippingLexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: CommentSkippingLexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn for_source_code(text: &'a str) -> Self {
        Self::new(Lexer::new(text).skip_comments())
    }

    pub fn parse(mut self) -> Result<Program, ParseError<'a>> {
        Ok(Program {
            code: vec![self.parse_block()?],
        })
    }
}

fn get_unary_operator(token: TokenType) -> Option<UnaryOperator> {
    match token {
        TokenType::Minus => Some(UnaryOperator::Minus),
        TokenType::Not => Some(UnaryOperator::Not),

        _ => None,
    }
}

fn get_binary_operator(token: TokenType) -> Option<BinaryOperator> {
    match token {
        TokenType::Plus | TokenType::With => Some(BinaryOperator::Plus),
        TokenType::Minus => Some(BinaryOperator::Minus),
        TokenType::Multiply => Some(BinaryOperator::Multiply),
        TokenType::Divide => Some(BinaryOperator::Divide),
        TokenType::And => Some(BinaryOperator::And),
        TokenType::Or => Some(BinaryOperator::Or),
        TokenType::Nor => Some(BinaryOperator::Nor),
        TokenType::Greater | TokenType::Bigger => Some(BinaryOperator::Greater),
        TokenType::GreaterEq | TokenType::Big => Some(BinaryOperator::GreaterEq),
        TokenType::Less | TokenType::Smaller => Some(BinaryOperator::Less),
        TokenType::LessEq | TokenType::Small => Some(BinaryOperator::LessEq),
        TokenType::Isnt => Some(BinaryOperator::NotEq),

        _ => None,
    }
}

fn get_mutation_operator(token: TokenType) -> Option<MutationOperator> {
    match token {
        TokenType::Cut => Some(MutationOperator::Cut),
        TokenType::Join => Some(MutationOperator::Join),
        TokenType::Cast => Some(MutationOperator::Cast),

        _ => None,
    }
}

fn get_rounding_direction(token: TokenType) -> Option<RoundingDirection> {
    match token {
        TokenType::Up => Some(RoundingDirection::Up),
        TokenType::Down => Some(RoundingDirection::Down),
        TokenType::Round => Some(RoundingDirection::Nearest),

        _ => None,
    }
}

fn boxed_expr<E>(x: impl Into<E>) -> Box<E> {
    Box::new(x.into())
}

fn take_first<T>(vec: Vec<T>) -> T {
    assert!(!vec.is_empty());
    vec.into_iter().next().unwrap()
}

fn is_literal_word(token: TokenType) -> bool {
    matches!(
        token,
        TokenType::Null
            | TokenType::Number(_)
            | TokenType::StringLiteral(_)
            | TokenType::Empty
            | TokenType::True
            | TokenType::False
    )
}

impl<'a> Parser<'a> {
    fn current(&self) -> Option<Token<'a>> {
        self.lexer.clone().next()
    }

    fn current_or_error(&self) -> Result<Token<'a>, ParseError<'a>> {
        self.current()
            .ok_or_else(|| self.new_parse_error(ParseErrorCode::UnexpectedEndOfTokens))
    }

    fn current_line(&self) -> usize {
        self.lexer.underlying().current_line()
    }

    fn new_parse_error(&self, code: ParseErrorCode<'a>) -> ParseError<'a> {
        ParseError::new(code, self.current())
    }

    fn error_if<F: FnOnce() -> ParseErrorCode<'a>>(
        &self,
        condition: bool,
        f: F,
    ) -> Result<(), ParseError<'a>> {
        condition
            .not()
            .then(|| ())
            .ok_or_else(|| self.new_parse_error(f()))
    }

    fn match_and_consume<M: MatchesToken>(&mut self, m: M) -> Option<Token<'a>> {
        if self
            .lexer
            .clone()
            .next()
            .filter(|tok| m.matches_token(tok))
            .is_some()
        {
            self.lexer.next()
        } else {
            None
        }
    }

    fn match_and_consume_while<M, R, Tx>(&mut self, m: M, tx: Tx) -> Result<Vec<R>, ParseError<'a>>
    where
        for<'b> &'b M: MatchesToken,
        Tx: Fn(&Token, &mut Parser<'a>) -> Result<R, ParseError<'a>>,
    {
        let mut result = Vec::new();
        while let Some(token) = self.match_and_consume(&m) {
            result.push(tx(&token, self)?);
        }
        Ok(result)
    }

    fn match_until_next(&mut self, token: TokenType) -> Option<Token<'a>> {
        self.lexer
            .take_while_ref(|tok| tok.id != token)
            .for_each(drop);
        self.current()
    }

    // step past a token we *know* satisfies the predicate
    fn consume<M: MatchesToken>(&mut self, m: M) -> Token<'a> {
        let tok = self.lexer.next();
        assert!(tok.filter(|tok| m.matches_token(tok)).is_some());
        tok.unwrap()
    }

    fn parse_expression_list(&mut self) -> Result<ExpressionList, ParseError<'a>> {
        let first = self.parse_expression()?;
        let rest = {
            let mut exprs = Vec::new();
            while let Some(e) = match self.parse_expression() {
                Ok(e) => Some(e),
                Err(e) => {
                    if e.code == ParseErrorCode::ExpectedPrimaryExpression {
                        None
                    } else {
                        Err(e)?
                    }
                }
            } {
                exprs.push(e)
            }
            exprs
        };
        Ok(ExpressionList { first, rest })
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError<'a>> {
        self.parse_logical_expression()
    }

    fn parse_logical_expression(&mut self) -> Result<Expression, ParseError<'a>> {
        self.parse_binary_expression(&[TokenType::And, TokenType::Or, TokenType::Nor], |p| {
            p.parse_comparison_expression()
        })
    }

    fn parse_fancy_comparison_expression(
        &mut self,
        lhs: Expression,
    ) -> Result<Expression, ParseError<'a>> {
        let operator = self
            .match_and_consume(TokenType::As)
            .map(|_| {
                let operator =
                    get_binary_operator(self.expect_any(&[TokenType::Big, TokenType::Small])?.id)
                        .unwrap();
                self.expect_token(TokenType::As)?;
                Ok(operator)
            })
            .or_else(|| {
                self.match_and_consume([TokenType::Bigger, TokenType::Smaller].as_ref())
                    .map(|tok| {
                        let operator = get_binary_operator(tok.id).unwrap();
                        self.expect_token(TokenType::Than)?;
                        Ok(operator)
                    })
            })
            .unwrap_or_else(|| {
                Ok(self
                    .match_and_consume(TokenType::Not)
                    .map(|_| BinaryOperator::NotEq)
                    .unwrap_or(BinaryOperator::Eq))
            })?;
        let rhs = boxed_expr(self.parse_term()?);
        let lhs = boxed_expr(lhs);
        Ok(BinaryExpression { operator, rhs, lhs }.into())
    }

    fn parse_comparison_expression(&mut self) -> Result<Expression, ParseError<'a>> {
        let expr = self.parse_term()?;
        let is_operators = [
            TokenType::Is,
            TokenType::ApostropheS,
            TokenType::ApostropheRE,
        ];
        if self.match_and_consume(is_operators.as_ref()).is_some() {
            let mut expr = self.parse_fancy_comparison_expression(expr)?;
            while self.match_and_consume(is_operators.as_ref()).is_some() {
                expr = self.parse_fancy_comparison_expression(expr)?;
            }
            Ok(expr)
        } else {
            self.parse_binary_expression_loop(
                [
                    TokenType::Less,
                    TokenType::LessEq,
                    TokenType::Greater,
                    TokenType::GreaterEq,
                    TokenType::Isnt,
                ]
                .as_ref(),
                |p| p.parse_term(),
                expr,
            )
        }
    }

    fn parse_term(&mut self) -> Result<Expression, ParseError<'a>> {
        self.parse_binary_expression(&[TokenType::Plus, TokenType::With, TokenType::Minus], |p| {
            p.parse_factor()
        })
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
        let expr = next(self)?;
        self.parse_binary_expression_loop(operators, next, expr)
    }

    fn parse_binary_expression_loop<F>(
        &mut self,
        operators: &[TokenType],
        next: F,
        mut expr: Expression,
    ) -> Result<Expression, ParseError<'a>>
    where
        F: Fn(&mut Parser<'a>) -> Result<Expression, ParseError<'a>>,
    {
        while let Some(operator) = self
            .match_and_consume(operators)
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
            .match_and_consume([TokenType::Minus, TokenType::Not].as_ref())
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
        let expr: PrimaryExpression = self
            .parse_identifier()?
            .map(Into::into)
            .or_else(|| self.parse_literal_expression().map(Into::into))
            .ok_or_else(|| self.new_parse_error(ParseErrorCode::ExpectedPrimaryExpression))?;
        self.parse_array_subscript_after(expr)
    }

    fn parse_array_subscript_after<As: From<ArraySubscript>>(
        &mut self,
        expr: impl Into<PrimaryExpression> + Into<As>,
    ) -> Result<As, ParseError<'a>> {
        if let Some(subscript) = self
            .match_and_consume(TokenType::At)
            .map(|_| self.parse_primary_expression())
            .transpose()?
        {
            let array = boxed_expr(expr);
            let subscript = boxed_expr(subscript);
            Ok(ArraySubscript { array, subscript }.into())
        } else {
            Ok(expr.into())
        }
    }

    fn parse_assignment_lhs(&mut self) -> Result<AssignmentLHS, ParseError<'a>> {
        let ident = self.expect_identifier()?;
        self.parse_array_subscript_after(ident)
    }

    fn parse_pronoun(&mut self) -> Option<Identifier> {
        self.match_and_consume(TokenType::Pronoun)
            .map(|_| Identifier::Pronoun)
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

    fn parse_common_identifier(&mut self) -> Result<Option<CommonIdentifier>, ParseError<'a>> {
        if let Some(prefix) = self
            .match_and_consume(TokenType::CommonVariablePrefix)
            .map(|tok| tok.spelling)
        {
            let next_word = self
                .match_and_consume([TokenType::Word, TokenType::CapitalizedWord].as_ref()) // capitalized words are invalid, but we want to match them for good error messages
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
        self.match_and_consume([TokenType::Word, TokenType::CapitalizedWord].as_ref())
            .map(|tok| SimpleIdentifier(tok.spelling.into()))
    }

    fn parse_capitalized_identifier(&mut self) -> Option<Identifier> {
        let names = self
            .match_and_consume_while(
                |tok: &Token| tok.id == TokenType::CapitalizedWord,
                |tok, _| Ok(tok.spelling.to_owned()),
            )
            .unwrap();
        match names.len() {
            0 => None,
            1 => Some(SimpleIdentifier(take_first(names)).into()),
            _ => Some(ProperIdentifier(names).into()),
        }
    }

    fn parse_identifier(&mut self) -> Result<Option<Identifier>, ParseError<'a>> {
        Ok(self
            .parse_common_identifier()?
            .map(Into::into)
            .or_else(|| self.parse_pronoun())
            .or_else(|| self.parse_capitalized_identifier())
            .or_else(|| self.parse_simple_identifier().map(Into::into)))
    }

    fn parse_statement(&mut self) -> Result<Option<Statement>, ParseError<'a>> {
        self.current()
            .and_then(|current_token| -> Option<Result<Statement, _>> {
                match current_token.id {
                    TokenType::Put => Some(self.parse_put_assignment().map(Into::into)),
                    TokenType::Let => Some(self.parse_let_assignment().map(Into::into)),
                    TokenType::Word
                    | TokenType::CapitalizedWord
                    | TokenType::CommonVariablePrefix => {
                        Some(self.parse_poetic_assignment().map(Into::into))
                    }

                    TokenType::If => Some(self.parse_if_statement().map(Into::into)),

                    TokenType::While | TokenType::Until => Some(self.parse_loop(&current_token.id)),

                    TokenType::Else => None,
                    TokenType::Newline => None,

                    TokenType::Build => Some(self.parse_build().map(Into::into)),
                    TokenType::Knock => Some(self.parse_knock().map(Into::into)),

                    TokenType::Say | TokenType::SayAlias => Some(self.parse_say().map(Into::into)),
                    TokenType::Listen => Some(self.parse_listen().map(Into::into)),

                    TokenType::Cut | TokenType::Join | TokenType::Cast => {
                        Some(self.parse_mutation().map(Into::into))
                    }
                    TokenType::Turn => Some(self.parse_rounding().map(Into::into)),

                    TokenType::Break => Some(self.parse_break()),
                    TokenType::Continue => Some(self.parse_simple_continue()),
                    TokenType::Take => Some(self.parse_take_it_to_the_top()),

                    TokenType::Rock => Some(self.parse_array_push().map(Into::into)),
                    TokenType::Roll => Some(self.parse_array_pop().map(Into::into)),

                    _ => {
                        let error = self.new_parse_error(ParseErrorCode::UnexpectedToken);
                        self.lexer.next();
                        Some(Err(error))
                    }
                }
            })
            .transpose()
    }

    fn parse_block_loop(&mut self) -> Result<Vec<StatementWithLine>, ParseError<'a>> {
        let mut statements = Vec::new();
        while let (line, Some(s)) = (self.current_line(), self.parse_statement()?) {
            statements.push(StatementWithLine(s, line));
            self.expect_token_or_end(TokenType::Newline)?;
        }
        Ok(statements)
    }

    fn parse_block(&mut self) -> Result<Block, ParseError<'a>> {
        let statements = self.parse_block_loop()?;
        self.expect_token_or_end(TokenType::Newline)?;
        Ok(Block(statements))
    }

    fn expect_token(&mut self, tok: TokenType<'a>) -> Result<Token<'a>, ParseError<'a>> {
        self.match_and_consume(tok)
            .ok_or_else(|| self.new_parse_error(ParseErrorCode::ExpectedToken(tok)))
    }

    fn expect_token_ispelled(&mut self, spelling: &str) -> Result<Token<'a>, ParseError<'a>> {
        self.match_and_consume(|tok: &Token| tok.is_ispelled(spelling))
            .ok_or_else(|| self.new_parse_error(ParseErrorCode::ExpectedText(spelling.into())))
    }

    fn expect_token_or_end(
        &mut self,
        tok: TokenType<'a>,
    ) -> Result<Option<Token<'a>>, ParseError<'a>> {
        let current = self.current();
        match current {
            Some(_) => current
                .filter(|token| token.id == tok)
                .map(|_| self.lexer.next())
                .ok_or_else(|| self.new_parse_error(ParseErrorCode::ExpectedToken(tok))),
            None => Ok(None),
        }
    }

    fn expect_any(&mut self, tokens: &[TokenType<'a>]) -> Result<Token<'a>, ParseError<'a>> {
        self.match_and_consume(tokens).ok_or_else(|| {
            self.new_parse_error(ParseErrorCode::ExpectedOneOfTokens(tokens.to_owned()))
        })
    }

    fn expect_identifier(&mut self) -> Result<Identifier, ParseError<'a>> {
        self.parse_identifier()?
            .ok_or_else(|| self.new_parse_error(ParseErrorCode::ExpectedIdentifier))
    }

    fn parse_put_assignment(&mut self) -> Result<Assignment, ParseError<'a>> {
        self.consume(TokenType::Put);
        let value = self.parse_expression()?.into();
        self.expect_token(TokenType::Into)?;
        let dest = self.parse_assignment_lhs()?;
        Ok(Assignment {
            dest,
            value,
            operator: None,
        })
    }
    fn parse_let_assignment(&mut self) -> Result<Assignment, ParseError<'a>> {
        self.consume(TokenType::Let);
        let dest = self.parse_assignment_lhs()?;
        self.expect_token(TokenType::Be)?;
        let operator = self
            .match_and_consume(
                [
                    TokenType::Plus,
                    TokenType::With,
                    TokenType::Minus,
                    TokenType::Multiply,
                    TokenType::Divide,
                ]
                .as_ref(),
            )
            .map(|tok| get_binary_operator(tok.id).unwrap());
        let value = self.parse_expression_list()?;
        self.error_if(operator.is_none() && value.has_multiple(), || {
            ParseErrorCode::ListExpressionWithoutOperator
        })?;
        Ok(Assignment {
            dest,
            value,
            operator,
        })
    }

    fn parse_poetic_number_literal(&mut self) -> Result<PoeticNumberLiteral, ParseError<'a>> {
        let elems = self.match_and_consume_while(
            |tok: &Token| {
                matches!(
                    tok.id,
                    TokenType::Dot | TokenType::ApostropheS | TokenType::ApostropheRE
                ) || *tok == Token::new(TokenType::Minus, "-")
                    || lexer::is_word(tok.spelling)
            },
            |tok, myself| match tok {
                Token {
                    id: TokenType::Dot, ..
                } => Ok(PoeticNumberLiteralElem::Dot),
                Token {
                    id: TokenType::ApostropheS | TokenType::ApostropheRE,
                    ..
                } => Ok(PoeticNumberLiteralElem::WordSuffix(tok.spelling.into())),
                Token {
                    id: TokenType::Minus,
                    spelling: "-",
                } => {
                    let next_token = myself.lexer.next().ok_or_else(|| {
                        myself.new_parse_error(ParseErrorCode::UnexpectedEndOfTokens)
                    })?;
                    lexer::is_word(next_token.spelling)
                        .then(|| next_token)
                        .ok_or_else(|| {
                            ParseError::new(ParseErrorCode::UnexpectedToken, Some(next_token))
                        })
                        .map(|word| {
                            PoeticNumberLiteralElem::WordSuffix("-".to_owned() + word.spelling)
                        })
                }
                _ => Ok(PoeticNumberLiteralElem::Word(tok.spelling.into())),
            },
        )?;
        (!elems.is_empty())
            .then(|| PoeticNumberLiteral { elems }.into())
            .ok_or_else(|| self.new_parse_error(ParseErrorCode::ExpectedPoeticNumberLiteral))
    }

    fn parse_poetic_number_assignment_rhs(
        &mut self,
    ) -> Result<PoeticNumberAssignmentRHS, ParseError<'a>> {
        is_literal_word(self.current_or_error()?.id)
            .then(|| self.parse_expression().map(Into::into))
            .unwrap_or_else(|| self.parse_poetic_number_literal().map(Into::into))
    }

    fn parse_poetic_string_assignment_rhs(
        &mut self,
        says_token: &Token<'a>,
    ) -> Result<String, ParseError<'a>> {
        let text = self
            .match_until_next(TokenType::Newline)
            .map(|end| {
                self.lexer
                    .underlying()
                    .get_literal_text_between(says_token, &end)
            })
            .unwrap_or_else(|| self.lexer.underlying().get_literal_text_after(says_token))
            .unwrap();
        text.strip_prefix(says_token.spelling)
            .unwrap()
            .strip_prefix(" ")
            .ok_or_else(|| {
                self.new_parse_error(ParseErrorCode::ExpectedSpaceAfterSays(*says_token))
            })
            .map(Into::into)
    }

    fn parse_poetic_assignment(&mut self) -> Result<PoeticAssignment, ParseError<'a>> {
        let dest = self.parse_assignment_lhs()?;

        match self.expect_any(&[
            TokenType::Is,
            TokenType::ApostropheS,
            TokenType::ApostropheRE,
            TokenType::Says,
            TokenType::Say,
        ])? {
            token if token.id == TokenType::Says || token.id == TokenType::Say => self
                .parse_poetic_string_assignment_rhs(&token)
                .map(|rhs| PoeticStringAssignment { dest, rhs }.into()),
            _ => self
                .parse_poetic_number_assignment_rhs()
                .map(|rhs| PoeticNumberAssignment { dest, rhs }.into()),
        }
    }

    fn parse_if_statement(&mut self) -> Result<If, ParseError<'a>> {
        self.consume(TokenType::If);
        let condition = self.parse_expression()?;
        self.expect_token_or_end(TokenType::Newline)?;
        let then_block = Block(self.parse_block_loop()?);
        let else_block = self
            .match_and_consume(TokenType::Else)
            .map(|_| {
                self.expect_token_or_end(TokenType::Newline)?;
                self.parse_block()
            })
            .transpose()?;
        Ok(If {
            condition,
            then_block,
            else_block,
        })
    }

    fn parse_loop(&mut self, start_token: &TokenType) -> Result<Statement, ParseError<'a>> {
        self.consume([TokenType::While, TokenType::Until].as_ref());
        let is_while = *start_token == TokenType::While;
        let condition = self.parse_expression()?;
        self.expect_token_or_end(TokenType::Newline)?;
        let block = Block(self.parse_block_loop()?);
        Ok(if is_while {
            While { condition, block }.into()
        } else {
            Until { condition, block }.into()
        })
    }

    fn parse_build_knock_helper(
        &mut self,
        begin: TokenType,
        suffix: TokenType<'a>,
    ) -> Result<(Identifier, usize), ParseError<'a>> {
        self.consume(begin);
        let dest = self.expect_identifier()?;
        self.expect_token(suffix)?;
        let extra_count = {
            let mut c = 0;
            while let Some(_) = self.match_and_consume(suffix) {
                c += 1
            }
            c
        };
        Ok((dest, 1 + extra_count))
    }

    fn parse_build(&mut self) -> Result<Inc, ParseError<'a>> {
        let (dest, amount) = self.parse_build_knock_helper(TokenType::Build, TokenType::Up)?;
        Ok(Inc { dest, amount })
    }

    fn parse_knock(&mut self) -> Result<Dec, ParseError<'a>> {
        let (dest, amount) = self.parse_build_knock_helper(TokenType::Knock, TokenType::Down)?;
        Ok(Dec { dest, amount })
    }

    fn parse_say(&mut self) -> Result<Output, ParseError<'a>> {
        self.consume([TokenType::Say, TokenType::SayAlias].as_ref());
        self.parse_expression().map(|value| Output { value })
    }

    fn parse_listen(&mut self) -> Result<Input, ParseError<'a>> {
        self.consume(TokenType::Listen);
        self.match_and_consume(TokenType::To)
            .map(|_| {
                self.parse_assignment_lhs()
                    .map(|dest| Input { dest: Some(dest) })
            })
            .unwrap_or_else(|| Ok(Input { dest: None }))
    }

    fn check_mutation_args(
        &self,
        operand: &PrimaryExpression,
        dest: &Option<AssignmentLHS>,
    ) -> Result<(), ParseError<'a>> {
        // this isn't strictly a *parse* error, but it's easy enough to handle here;
        // mutations without a destination must have operands that are identifiers
        if dest.is_some() {
            Ok(())
        } else {
            matches!(*operand, PrimaryExpression::Identifier(_))
                .then(|| ())
                .ok_or_else(|| {
                    self.new_parse_error(ParseErrorCode::MutationOperandMustBeIdentifier(
                        operand.clone(),
                    ))
                })
        }
    }

    fn parse_mutation(&mut self) -> Result<Mutation, ParseError<'a>> {
        let operator = get_mutation_operator(
            self.consume([TokenType::Cut, TokenType::Join, TokenType::Cast].as_ref())
                .id,
        )
        .unwrap();
        let operand = self.parse_primary_expression()?;
        let dest = self
            .match_and_consume(TokenType::Into)
            .map(|_| self.parse_assignment_lhs())
            .transpose()?;
        self.check_mutation_args(&operand, &dest)?;
        let param = self
            .match_and_consume(TokenType::With)
            .map(|_| self.parse_expression())
            .transpose()?;
        Ok(Mutation {
            operator,
            operand,
            dest,
            param,
        })
    }

    fn parse_rounding_direction(&mut self) -> Option<RoundingDirection> {
        self.match_and_consume([TokenType::Up, TokenType::Down, TokenType::Round].as_ref())
            .and_then(|tok| get_rounding_direction(tok.id))
    }

    fn parse_rounding(&mut self) -> Result<Rounding, ParseError<'a>> {
        self.consume(TokenType::Turn);

        let direction = self.parse_rounding_direction();
        let operand = self.parse_expression()?;
        let direction = direction
            .or_else(|| self.parse_rounding_direction())
            .ok_or_else(|| {
                self.new_parse_error(ParseErrorCode::ExpectedOneOfTokens(vec![
                    TokenType::Up,
                    TokenType::Down,
                    TokenType::Round,
                ]))
            })?;
        Ok(Rounding { direction, operand })
    }

    fn parse_break(&mut self) -> Result<Statement, ParseError<'a>> {
        self.consume(TokenType::Break);
        self.match_and_consume(|tok: &Token| tok.is_ispelled("it"))
            .map(|_| self.expect_token(TokenType::Down))
            .transpose()
            .map(|_| Statement::Break)
    }

    fn parse_simple_continue(&mut self) -> Result<Statement, ParseError<'a>> {
        self.consume(TokenType::Continue);
        Ok(Statement::Continue)
    }
    fn parse_take_it_to_the_top(&mut self) -> Result<Statement, ParseError<'a>> {
        self.consume(TokenType::Take);
        self.expect_token_ispelled("it")?;
        self.expect_token(TokenType::To)?;
        self.expect_token_ispelled("the")?;
        self.expect_token(TokenType::Top)?;
        Ok(Statement::Continue)
    }

    fn parse_array_push_rhs(&mut self) -> Result<ArrayPushRHS, ParseError<'a>> {
        match self.expect_any(&[TokenType::With, TokenType::Like])?.id {
            TokenType::With => self.parse_expression_list().map(Into::into),
            TokenType::Like => self.parse_poetic_number_literal().map(Into::into),

            _ => unreachable!(),
        }
    }

    fn parse_array_push(&mut self) -> Result<ArrayPush, ParseError<'a>> {
        self.consume(TokenType::Rock);
        let array = self.parse_primary_expression()?;
        let value = self.parse_array_push_rhs()?;
        Ok(ArrayPush { array, value })
    }

    fn parse_array_pop(&mut self) -> Result<ArrayPop, ParseError<'a>> {
        self.consume(TokenType::Roll);
        let array = self.parse_primary_expression()?;
        let dest = self
            .match_and_consume(TokenType::Into)
            .map(|_| self.parse_assignment_lhs())
            .transpose()?;
        Ok(ArrayPop { array, dest })
    }
}

pub fn parse(text: &str) -> Result<Program, ParseError> {
    Parser::for_source_code(text).parse()
}

#[cfg(test)]
mod test {
    use crate::ast::Output;

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
            Ok(Expression::Binary(BinaryExpression {
                operator: BinaryOperator::Plus,
                lhs: boxed_expr(LiteralExpression::Number(1.0)),
                rhs: boxed_expr(LiteralExpression::Number(1.0))
            }))
        );
        assert_eq!(
            parse("1 with 1"),
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
            parse("1 without 1"),
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
            "it", "he", "she", "him", "her", "they", "them", "ze", "hir", "zie", "zir", "xe",
            "xem", "ve", "ver",
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
            parse("Let X be with 10, 11, 12"),
            Ok(Assignment {
                dest: SimpleIdentifier("X".into()).into(),
                value: ExpressionList {
                    first: LiteralExpression::Number(10.0).into(),
                    rest: vec![
                        LiteralExpression::Number(11.0).into(),
                        LiteralExpression::Number(12.0).into()
                    ]
                },
                operator: Some(BinaryOperator::Plus),
            }
            .into())
        );
        assert_eq!(
            parse("Let X be with 10, 11 with 12, 13"),
            Ok(Assignment {
                dest: SimpleIdentifier("X".into()).into(),
                value: ExpressionList {
                    first: LiteralExpression::Number(10.0).into(),
                    rest: vec![
                        BinaryExpression {
                            operator: BinaryOperator::Plus,
                            lhs: boxed_expr(LiteralExpression::Number(11.0)),
                            rhs: boxed_expr(LiteralExpression::Number(12.0))
                        }
                        .into(),
                        LiteralExpression::Number(13.0).into()
                    ]
                },
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
                    id: TokenType::Number(2.0),
                    spelling: "2"
                })
            })
        );

        assert_eq!(
            parse("Let X be 10, 11 with 12, 13"),
            Err(ParseError {
                code: ParseErrorCode::ListExpressionWithoutOperator,
                token: None
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
                    Parser::for_source_code(text).parse_poetic_assignment().unwrap(),
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
        assert_eq!(val("Sweet Lucy was a dancer"), 16.0);
        assert_eq!(val("A killer is on the loose"), 235.0);
        assert_eq!(
        val("My dreams were ice. A life unfulfilled; wakin' everybody up, taking booze and pills"),
        3.1415926535
    );
        assert_eq!(
        val("My dreams were ice... A. life .. ...unfulfilled;....;;;''''' wakin' .everybody. .up, ..taking booze ....and pills......"),
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
            parse("cut it into pieces with my knife, with my knife"),
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
}
