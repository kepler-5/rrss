use derive_more::Constructor;
use itertools::Itertools;

use crate::frontend::{ast::*, lexer::*};

#[cfg(test)]
mod tests;

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

#[derive(Clone, Constructor, Debug, PartialEq)]
pub struct ParseError<'a> {
    pub code: ParseErrorCode<'a>,
    pub token: Option<Token<'a>>,
}

#[derive(Clone, Constructor, Debug, PartialEq)]
pub struct ParseErrorWithLine<'a>(ParseError<'a>, usize);

pub struct Parser<'a> {
    lexer: CommentSkippingLexer<'a>,
    parsing_list: bool,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: CommentSkippingLexer<'a>) -> Self {
        Self {
            lexer,
            parsing_list: false,
        }
    }

    pub fn for_source_code(text: &'a str) -> Self {
        Self::new(Lexer::new(text).skip_comments())
    }

    pub fn parse(mut self) -> Result<Program, ParseErrorWithLine<'a>> {
        let mut blocks = Vec::new();
        while self.current().is_some() {
            if let Some(block) =
                Some(self.parse_block().map_err(|e| self.add_line(e))?).filter(|b| !b.is_empty())
            {
                blocks.push(block);
            }
        }
        Ok(Program { code: blocks })
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

    fn current_matches<M: MatchesToken>(&self, m: M) -> bool {
        self.current().filter(|tok| m.matches_token(tok)).is_some()
    }

    fn current_line(&self) -> usize {
        self.lexer.underlying().current_line()
    }

    fn new_parse_error(&self, code: ParseErrorCode<'a>) -> ParseError<'a> {
        ParseError::new(code, self.current())
    }

    fn add_line(&self, error: ParseError<'a>) -> ParseErrorWithLine<'a> {
        ParseErrorWithLine(error, self.current_line())
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
        Tx: Fn(&Token, &mut Parser<'a>) -> Result<Option<R>, ParseError<'a>>,
    {
        let mut result = Vec::new();
        while let Some(token) = self.match_and_consume(&m) {
            if let Some(r) = tx(&token, self)? {
                result.push(r);
            }
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

    fn parse_expression_list<
        E: Into<Expression>,
        F: Fn(&mut Parser<'a>) -> Result<E, ParseError<'a>>,
    >(
        &mut self,
        next: F,
    ) -> Result<ExpressionList, ParseError<'a>> {
        let first = next(self)?.into();
        let rest = if self.parsing_list {
            Vec::new()
        } else {
            self.parsing_list = true; // nested lists are not allowed
            let mut exprs = Vec::new();
            while let Some(e) = self
                .match_and_consume(TokenType::Comma)
                .map(|_| {
                    self.match_and_consume(TokenType::And);
                    next(self)
                })
                .transpose()?
                .map(Into::into)
            {
                exprs.push(e)
            }
            exprs
        };
        self.parsing_list = false;
        Ok(ExpressionList { first, rest })
    }

    fn parse_toplevel_expression_list(&mut self) -> Result<ExpressionList, ParseError<'a>> {
        self.parse_expression_list(|p| p.parse_expression())
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
            let rhs = boxed_expr(self.parse_expression_list(&next)?);
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
            .parse_identifier_or_function_call()?
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

    fn parse_assignment_lhs_with(
        &mut self,
        ident: Identifier,
    ) -> Result<AssignmentLHS, ParseError<'a>> {
        self.parse_array_subscript_after(ident)
    }
    fn parse_assignment_lhs(&mut self) -> Result<AssignmentLHS, ParseError<'a>> {
        let ident = self.expect_identifier()?;
        self.parse_assignment_lhs_with(ident)
    }

    fn parse_pronoun(&mut self) -> Option<Identifier> {
        self.match_and_consume(TokenType::Pronoun)
            .map(|_| Identifier::Pronoun)
    }

    fn parse_literal_expression(&mut self) -> Option<LiteralExpression> {
        self.current()
            .and_then(|token| match token.id {
                TokenType::Null => Some(LiteralExpression::Null),
                TokenType::Number(n) => Some(LiteralExpression::Number(n)),
                TokenType::StringLiteral(s) => Some(LiteralExpression::String(s.to_owned())),
                TokenType::Empty => Some(LiteralExpression::String(String::new())),
                TokenType::True => Some(LiteralExpression::Boolean(true)),
                TokenType::False => Some(LiteralExpression::Boolean(false)),
                _ => None,
            })
            .map(|tok| {
                self.lexer.next();
                tok
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

    fn parse_capitalized_identifier(&mut self) -> Option<VariableName> {
        let names = self
            .match_and_consume_while(
                |tok: &Token| tok.id == TokenType::CapitalizedWord,
                |tok, _| Ok(Some(tok.spelling.to_owned())),
            )
            .unwrap();
        match names.len() {
            0 => None,
            1 => Some(SimpleIdentifier(take_first(names)).into()),
            _ => Some(ProperIdentifier(names).into()),
        }
    }

    fn parse_variable_name(&mut self) -> Result<Option<VariableName>, ParseError<'a>> {
        Ok(self
            .parse_common_identifier()?
            .map(Into::into)
            .or_else(|| self.parse_capitalized_identifier().map(Into::into))
            .or_else(|| self.parse_simple_identifier().map(Into::into)))
    }

    fn parse_identifier(&mut self) -> Result<Option<Identifier>, ParseError<'a>> {
        Ok(self
            .parse_variable_name()?
            .map(Into::into)
            .or_else(|| self.parse_pronoun()))
    }

    fn parse_rest_of_function_call(&mut self) -> Result<Vec<PrimaryExpression>, ParseError<'a>> {
        self.parse_parameter_list(|p| p.parse_primary_expression())
    }

    fn parse_function_call(&mut self, name: VariableName) -> Result<FunctionCall, ParseError<'a>> {
        self.consume(TokenType::Taking);
        self.parse_rest_of_function_call()
            .map(|args| FunctionCall { name, args })
    }

    fn parse_identifier_or_function_call(
        &mut self,
    ) -> Result<Option<PrimaryExpression>, ParseError<'a>> {
        if let pronoun @ Some(_) = self.parse_pronoun() {
            Ok(pronoun.map(Into::into))
        } else {
            self.parse_variable_name()?
                .map(|name| {
                    if self.current_matches(TokenType::Taking) {
                        self.parse_function_call(name).map(Into::into)
                    } else {
                        Ok(name.into())
                    }
                })
                .transpose()
        }
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
                        Some(self.parse_statement_starting_with_word())
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

                    TokenType::Return => Some(self.parse_return().map(Into::into)),

                    _ => {
                        let error = self.new_parse_error(ParseErrorCode::UnexpectedToken);
                        self.lexer.next();
                        Some(Err(error))
                    }
                }
            })
            .transpose()
    }

    fn expect_eol(&mut self) -> Result<(), ParseError<'a>> {
        self.match_and_consume(TokenType::Comma);
        self.expect_token_or_end(TokenType::Newline).map(|_| ())
    }

    fn parse_block_loop(&mut self) -> Result<Vec<StatementWithLine>, ParseError<'a>> {
        let mut statements = Vec::new();
        while let (line, Some(s)) = (self.current_line(), self.parse_statement()?) {
            statements.push(StatementWithLine(s, line));
            self.expect_eol()?;
        }
        Ok(statements)
    }

    fn parse_block(&mut self) -> Result<Block, ParseError<'a>> {
        let statements = self.parse_block_loop()?;
        self.expect_eol()?;
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

    fn expect_variable_name(&mut self) -> Result<VariableName, ParseError<'a>> {
        self.parse_variable_name()?
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
        let value = self.parse_toplevel_expression_list()?;
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
                    TokenType::Dot
                        | TokenType::Comma
                        | TokenType::ApostropheS
                        | TokenType::ApostropheRE
                ) || *tok == Token::new(TokenType::Minus, "-")
                    || is_word(tok.spelling)
            },
            |tok, myself| match tok {
                tok if tok.id == TokenType::Comma => Ok(None),
                tok if tok.id == TokenType::Dot => Ok(Some(PoeticNumberLiteralElem::Dot)),
                tok if matches!(tok.id, TokenType::ApostropheS | TokenType::ApostropheRE) => Ok(
                    Some(PoeticNumberLiteralElem::WordSuffix(tok.spelling.into())),
                ),
                Token {
                    id: TokenType::Minus,
                    spelling: "-",
                } => {
                    let next_token = myself.lexer.next().ok_or_else(|| {
                        myself.new_parse_error(ParseErrorCode::UnexpectedEndOfTokens)
                    })?;
                    is_word(next_token.spelling)
                        .then(|| next_token)
                        .ok_or_else(|| {
                            ParseError::new(ParseErrorCode::UnexpectedToken, Some(next_token))
                        })
                        .map(|word| {
                            Some(PoeticNumberLiteralElem::WordSuffix(
                                "-".to_owned() + word.spelling,
                            ))
                        })
                }
                _ => Ok(Some(PoeticNumberLiteralElem::Word(tok.spelling.into()))),
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

    fn parse_poetic_assignment(
        &mut self,
        name: VariableName,
    ) -> Result<PoeticAssignment, ParseError<'a>> {
        let dest = self.parse_assignment_lhs_with(name.into())?;

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

    fn parse_parameter_list<R, P: FnMut(&mut Parser<'a>) -> Result<R, ParseError<'a>>>(
        &mut self,
        mut p: P,
    ) -> Result<Vec<R>, ParseError<'a>> {
        let mut params = vec![p(self)?];
        while let Some(sep) = self.match_and_consume(
            [
                TokenType::Ampersand,
                TokenType::Comma,
                TokenType::ApostropheNApostrophe,
            ]
            .as_ref(),
        ) {
            if sep.id == TokenType::Comma {
                self.match_and_consume(TokenType::And);
            }
            params.push(p(self)?);
        }
        Ok(params)
    }

    fn parse_function(&mut self, name: VariableName) -> Result<Function, ParseError<'a>> {
        self.consume(TokenType::Takes);
        let params = self.parse_parameter_list(|p| p.expect_variable_name())?;
        self.expect_eol()?;
        let body = self.parse_block()?;
        Ok(Function { name, params, body })
    }

    fn parse_statement_starting_with_word(&mut self) -> Result<Statement, ParseError<'a>> {
        let name = self
            .parse_variable_name()?
            .ok_or_else(|| self.new_parse_error(ParseErrorCode::ExpectedIdentifier))?;
        match self.current().map(|tok| tok.id) {
            Some(TokenType::Takes) => self.parse_function(name).map(Into::into),
            Some(TokenType::Taking) => self.parse_function_call(name).map(Into::into),

            _ => self.parse_poetic_assignment(name).map(Into::into),
        }
    }

    fn parse_if_statement(&mut self) -> Result<If, ParseError<'a>> {
        self.consume(TokenType::If);
        let condition = self.parse_expression()?;
        self.expect_eol()?;
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
        self.expect_eol()?;
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
            self.match_and_consume(TokenType::Comma);
            while let Some(_) = self.match_and_consume(suffix) {
                c += 1;
                self.match_and_consume(TokenType::Comma);
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
            TokenType::With => self.parse_toplevel_expression_list().map(Into::into),
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

    fn parse_return(&mut self) -> Result<Return, ParseError<'a>> {
        let return_token = self.consume(TokenType::Return);
        return_token
            .is_ispelled("give")
            .then(|| self.match_and_consume(TokenType::Back));
        let value = self.parse_expression()?;
        self.match_and_consume(TokenType::Back);
        Ok(Return { value })
    }
}

pub fn parse(text: &str) -> Result<Program, ParseErrorWithLine> {
    Parser::for_source_code(text).parse()
}
