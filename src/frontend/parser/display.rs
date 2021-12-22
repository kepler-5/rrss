use std::fmt::Display;

use super::*;

impl Display for TokenType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::ApostropheS => f.write_str("'s"),
            TokenType::ApostropheRE => f.write_str("'re"),
            TokenType::ApostropheNApostrophe => f.write_str("'n'"),

            _ => f.write_str(&format!("{:?}", self).to_lowercase()),
        }
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.spelling)
    }
}

fn write_list<I>(f: &mut std::fmt::Formatter<'_>, mut iter: I) -> std::fmt::Result
where
    I: ExactSizeIterator,
    I::Item: Display,
{
    assert!(iter.len() != 0);
    match iter.len() {
        0 => unreachable!(),

        1 => write!(f, "`{}`", iter.next().unwrap()),
        2 => {
            write!(
                f,
                "`{}` or `{}`",
                iter.next().unwrap(),
                iter.next().unwrap()
            )
        }

        n => {
            for _ in 0..(n - 1) {
                write!(f, "`{}`, ", iter.next().unwrap())?;
            }
            write!(f, "or `{}`", iter.next().unwrap())
        }
    }
}

fn expected_id_description(e: &PrimaryExpression) -> &'static str {
    match e {
        PrimaryExpression::Literal(_) => "literal",
        PrimaryExpression::ArraySubscript(_) => "array subscript expression",
        PrimaryExpression::FunctionCall(_) => "function call",

        PrimaryExpression::Identifier(_) => unreachable!(),
    }
}

impl Display for ParseError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tok = &self.token;

        macro_rules! if_token {
            ($fmt:literal) => {
                tok.map_or_else(String::new, |tok| format!($fmt, tok))
            };
        }
        macro_rules! found_suffix {
            () => {
                if_token!(", found `{}`")
            };
        }
        match &self.code {
            ParseErrorCode::Generic(s) => {
                write!(f, "{}{}", s, if_token!(" at `{}`"))
            }
            ParseErrorCode::MissingIDAfterCommonPrefix(prefix) => write!(
                f,
                "Missing identifier after `{}`{}",
                prefix,
                found_suffix!()
            ),
            ParseErrorCode::UppercaseAfterCommonPrefix(prefix, next) => write!(
                f,
                "Unexpected uppercase after common variable prefix; `{} {}` is an invalid common variable name",
                prefix, next
            ),
            ParseErrorCode::MutationOperandMustBeIdentifier(e) => write!(
                f, 
                "Mutation operand with no `into` destination must be identifier; found {}", 
                expected_id_description(e)
            ),
            ParseErrorCode::ExpectedPrimaryExpression => {
                write!(f, "Expected primary expression{}", found_suffix!())
            }
            ParseErrorCode::ExpectedIdentifier => {
                write!(f, "Expected identifier{}", found_suffix!())
            }
            ParseErrorCode::ExpectedText(text) => {
                write!(f, "Expected `{}`{}", text, found_suffix!())
            }
            ParseErrorCode::ExpectedToken(t) => {
                write!(f, "Expected `{}`{}", t, found_suffix!())
            }
            ParseErrorCode::ExpectedOneOfTokens(tokens) => {
                f.write_str("Expected ")?;
                let result = write_list(f, tokens.iter());
                tok.map(|tok| write!(f, ", found `{}`", tok))
                    .unwrap_or(result)
            }
            ParseErrorCode::ExpectedPoeticNumberLiteral => {
                write!(f, "Expected poetic number literal{}", found_suffix!())
            }
            ParseErrorCode::ExpectedSpaceAfterSays(says) => write!(
                f,
                "Expected space after `{}`{}",
                says.spelling,
                found_suffix!()
            ),
            ParseErrorCode::UnexpectedToken => {
                write!(f, "Unexpected token `{}`", tok.unwrap())
            }
            ParseErrorCode::UnexpectedEndOfTokens => f.write_str("Unexpected end of tokens"),
        }
    }
}

impl std::error::Error for ParseError<'_> {}
