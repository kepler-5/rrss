use super::*;

fn line_range(start: u32, end: u32) -> SourceRange {
    ((1, start), (1, end)).into()
}

fn range_on_line(line: u32, range: (u32, u32)) -> SourceRange {
    ((line, range.0), (line, range.1)).into()
}

fn lex(buf: &str) -> Vec<Token> {
    Lexer::new(buf).collect()
}

fn types(buf: &str) -> Vec<TokenType> {
    lex(buf).iter().map(|tok| tok.id).collect()
}

fn no_ranges(buf: &str) -> Vec<(TokenType, &str)> {
    lex(buf).iter().map(|tok| (tok.id, tok.spelling)).collect()
}

fn skipping_comments(buf: &str) -> Vec<Token> {
    Lexer::new(buf).skip_comments().collect()
}

#[test]
fn lex_basic() {
    assert_eq!(lex(""), []);
    assert_eq!(lex("       "), []);
    assert_eq!(
        lex("mysterious"),
        [Token::new(
            TokenType::Mysterious,
            "mysterious",
            line_range(0, 10)
        )]
    );
    assert_eq!(
        lex("      mysterious"),
        [Token::new(
            TokenType::Mysterious,
            "mysterious",
            line_range(6, 16)
        )]
    );
    assert_eq!(
        lex("  mysterious   mysterious    "),
        [
            Token::new(TokenType::Mysterious, "mysterious", line_range(2, 12)),
            Token::new(TokenType::Mysterious, "mysterious", line_range(15, 25))
        ]
    );
}

#[test]
fn lex_literals() {
    assert_eq!(
        no_ranges("right yes ok true"),
        [
            (TokenType::True, "right"),
            (TokenType::True, "yes"),
            (TokenType::True, "ok"),
            (TokenType::True, "true"),
        ]
    );
    assert_eq!(
        no_ranges("wrong no lies false"),
        [
            (TokenType::False, "wrong"),
            (TokenType::False, "no"),
            (TokenType::False, "lies"),
            (TokenType::False, "false"),
        ],
    );
    assert_eq!(
        no_ranges("empty silence silent"),
        [
            (TokenType::Empty, "empty"),
            (TokenType::Empty, "silence"),
            (TokenType::Empty, "silent")
        ]
    );
    assert_eq!(
        lex("1 2.0 3.4  .6 -7"),
        [
            Token::new(TokenType::Number(1.0), "1", line_range(0, 1)),
            Token::new(TokenType::Number(2.0), "2.0", line_range(2, 5)),
            Token::new(TokenType::Number(3.4), "3.4", line_range(6, 9)),
            Token::new(TokenType::Number(0.6), ".6", line_range(11, 13)),
            Token::new(TokenType::Minus, "-", line_range(14, 15)),
            Token::new(TokenType::Number(7.0), "7", line_range(15, 16)),
        ]
    );
}

#[test]
fn lex_symbols() {
    assert_eq!(
        lex("+ - * /"),
        [
            Token::new(TokenType::Plus, "+", line_range(0, 1)),
            Token::new(TokenType::Minus, "-", line_range(2, 3)),
            Token::new(TokenType::Multiply, "*", line_range(4, 5)),
            Token::new(TokenType::Divide, "/", line_range(6, 7))
        ]
    );

    assert_eq!(
        lex("<= >= < >"),
        [
            Token::new(TokenType::LessEq, "<=", line_range(0, 2)),
            Token::new(TokenType::GreaterEq, ">=", line_range(3, 5)),
            Token::new(TokenType::Less, "<", line_range(6, 7)),
            Token::new(TokenType::Greater, ">", line_range(8, 9))
        ]
    );

    assert_eq!(
        lex("& 'n'"),
        [
            Token::new(TokenType::Ampersand, "&", line_range(0, 1)),
            Token::new(TokenType::ApostropheNApostrophe, "'n'", line_range(2, 5)),
        ],
    );

    assert_eq!(
        types("1+1"),
        [
            TokenType::Number(1.0),
            TokenType::Plus,
            TokenType::Number(1.0)
        ]
    );
    assert_eq!(
        types("1+-1"),
        [
            TokenType::Number(1.0),
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Number(1.0)
        ]
    );
    assert_eq!(
        lex("foo+1"),
        [
            Token::new(TokenType::Word, "foo", line_range(0, 3)),
            Token::new(TokenType::Plus, "+", line_range(3, 4)),
            Token::new(TokenType::Number(1.0), "1", line_range(4, 5))
        ]
    );
    assert_eq!(
        types("foo+-1"),
        [
            TokenType::Word,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Number(1.0)
        ]
    );
    assert_eq!(
        types("1+foo"),
        [TokenType::Number(1.0), TokenType::Plus, TokenType::Word]
    );
    assert_eq!(
        types("1+-foo"),
        [
            TokenType::Number(1.0),
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Word
        ]
    );
    assert_eq!(
        types("foo+foo"),
        [TokenType::Word, TokenType::Plus, TokenType::Word]
    );
    assert_eq!(
        types("foo+-foo"),
        [
            TokenType::Word,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Word
        ]
    );
}

#[test]
fn lex_words_and_newlines() {
    assert_eq!(
        no_ranges("my MY My mY"),
        [
            (TokenType::CommonVariablePrefix, "my"),
            (TokenType::CommonVariablePrefix, "MY"),
            (TokenType::CommonVariablePrefix, "My"),
            (TokenType::CommonVariablePrefix, "mY"),
        ]
    );

    assert_eq!(
        no_ranges("hello world"),
        [(TokenType::Word, "hello"), (TokenType::Word, "world")]
    );

    assert_eq!(
        lex("hello\n world"),
        [
            Token::new(TokenType::Word, "hello", range_on_line(1, (0, 5))),
            Token::new(TokenType::Newline, "\n", range_on_line(1, (5, 6))),
            Token::new(TokenType::Word, "world", range_on_line(2, (1, 6))),
        ]
    );
    assert_eq!(
        lex("hello \n world"),
        [
            Token::new(TokenType::Word, "hello", range_on_line(1, (0, 5))),
            Token::new(TokenType::Newline, "\n", range_on_line(1, (6, 7))),
            Token::new(TokenType::Word, "world", range_on_line(2, (1, 6))),
        ]
    );
    assert_eq!(
        lex("hello\nworld"),
        [
            Token::new(TokenType::Word, "hello", range_on_line(1, (0, 5))),
            Token::new(TokenType::Newline, "\n", range_on_line(1, (5, 6))),
            Token::new(TokenType::Word, "world", range_on_line(2, (0, 5))),
        ]
    );
    assert_eq!(
        lex("hello, world."),
        [
            Token::new(TokenType::Word, "hello", line_range(0, 5)),
            Token::new(TokenType::Comma, ",", line_range(5, 6)),
            Token::new(TokenType::Word, "world", line_range(7, 12)),
            Token::new(TokenType::Dot, ".", line_range(12, 13)),
        ]
    );
    assert_eq!(
        lex("hello?world!"), // the spec isn't crystal clear here, but I'm allowing this
        [
            Token::new(TokenType::Word, "hello", line_range(0, 5)),
            Token::new(TokenType::Word, "world", line_range(6, 11)),
        ]
    );
    assert_eq!(
        lex("Hello World world"),
        [
            Token::new(TokenType::CapitalizedWord, "Hello", line_range(0, 5)),
            Token::new(TokenType::CapitalizedWord, "World", line_range(6, 11)),
            Token::new(TokenType::Word, "world", line_range(12, 17)),
        ]
    );
    assert_eq!(
        lex("\n\n\n"),
        [
            Token::new(TokenType::Newline, "\n", range_on_line(1, (0, 1))),
            Token::new(TokenType::Newline, "\n", range_on_line(2, (0, 1))),
            Token::new(TokenType::Newline, "\n", range_on_line(3, (0, 1))),
        ]
    );
    assert_eq!(
        lex("\r\n \r\n \r\n"),
        [
            Token::new(TokenType::Newline, "\n", range_on_line(1, (1, 2))),
            Token::new(TokenType::Newline, "\n", range_on_line(2, (2, 3))),
            Token::new(TokenType::Newline, "\n", range_on_line(3, (2, 3))),
        ]
    );
}

#[test]
fn lex_keywords() {
    assert_eq!(
        no_ranges("and or nor"),
        [
            (TokenType::And, "and"),
            (TokenType::Or, "or"),
            (TokenType::Nor, "nor"),
        ]
    );

    assert_eq!(
        no_ranges("plus with"),
        [(TokenType::Plus, "plus"), (TokenType::With, "with")]
    );
    assert_eq!(
        no_ranges("minus without"),
        [(TokenType::Minus, "minus"), (TokenType::Minus, "without")]
    );
    assert_eq!(
        no_ranges("times of"),
        [(TokenType::Multiply, "times"), (TokenType::Multiply, "of")]
    );
    assert_eq!(
        no_ranges("over between"),
        [(TokenType::Divide, "over"), (TokenType::Divide, "between")]
    );

    assert_eq!(
        no_ranges("in into"),
        [(TokenType::Into, "in"), (TokenType::Into, "into")]
    );
    assert_eq!(
        no_ranges("is isn't put let be not at like"),
        [
            (TokenType::Is, "is"),
            (TokenType::Isnt, "isn't"),
            (TokenType::Put, "put"),
            (TokenType::Let, "let"),
            (TokenType::Be, "be"),
            (TokenType::Not, "not"),
            (TokenType::At, "at"),
            (TokenType::Like, "like"),
        ]
    );
    assert_eq!(
        no_ranges("is are was were"),
        [
            (TokenType::Is, "is"),
            (TokenType::Is, "are"),
            (TokenType::Is, "was"),
            (TokenType::Is, "were")
        ]
    );
    assert_eq!(
        no_ranges("say says said"),
        [
            (TokenType::Say, "say"),
            (TokenType::Says, "says"),
            (TokenType::Says, "said"),
        ]
    );

    assert_eq!(
        no_ranges("higher greater bigger stronger"),
        [
            (TokenType::Bigger, "higher"),
            (TokenType::Bigger, "greater"),
            (TokenType::Bigger, "bigger"),
            (TokenType::Bigger, "stronger"),
        ],
    );
    assert_eq!(
        no_ranges("lower less smaller weaker"),
        [
            (TokenType::Smaller, "lower"),
            (TokenType::Smaller, "less"),
            (TokenType::Smaller, "smaller"),
            (TokenType::Smaller, "weaker"),
        ],
    );
    assert_eq!(
        no_ranges("high great big strong"),
        [
            (TokenType::Big, "high"),
            (TokenType::Big, "great"),
            (TokenType::Big, "big"),
            (TokenType::Big, "strong"),
        ],
    );
    assert_eq!(
        no_ranges("low little small weak"),
        [
            (TokenType::Small, "low"),
            (TokenType::Small, "little"),
            (TokenType::Small, "small"),
            (TokenType::Small, "weak"),
        ],
    );
    assert_eq!(
        no_ranges("as than"),
        [(TokenType::As, "as"), (TokenType::Than, "than"),],
    );

    assert_eq!(
        no_ranges("if else while until"),
        [
            (TokenType::If, "if"),
            (TokenType::Else, "else"),
            (TokenType::While, "while"),
            (TokenType::Until, "until"),
        ],
    );

    assert_eq!(
        no_ranges("build up knock down"),
        [
            (TokenType::Build, "build"),
            (TokenType::Up, "up"),
            (TokenType::Knock, "knock"),
            (TokenType::Down, "down"),
        ],
    );

    assert_eq!(
        no_ranges("say shout whisper scream listen to"),
        [
            (TokenType::Say, "say"),
            (TokenType::SayAlias, "shout"),
            (TokenType::SayAlias, "whisper"),
            (TokenType::SayAlias, "scream"),
            (TokenType::Listen, "listen"),
            (TokenType::To, "to"),
        ],
    );

    assert_eq!(
        no_ranges("cut split shatter"),
        [
            (TokenType::Cut, "cut"),
            (TokenType::Cut, "split"),
            (TokenType::Cut, "shatter"),
        ],
    );
    assert_eq!(
        no_ranges("join unite"),
        [(TokenType::Join, "join"), (TokenType::Join, "unite"),],
    );
    assert_eq!(
        no_ranges("cast burn"),
        [(TokenType::Cast, "cast"), (TokenType::Cast, "burn"),],
    );

    assert_eq!(
        no_ranges("turn round around"),
        [
            (TokenType::Turn, "turn"),
            (TokenType::Round, "round"),
            (TokenType::Round, "around"),
        ],
    );

    assert_eq!(
        no_ranges("continue break take top"),
        [
            (TokenType::Continue, "continue"),
            (TokenType::Break, "break"),
            (TokenType::Take, "take"),
            (TokenType::Top, "top"),
        ],
    );

    assert_eq!(
        no_ranges("rock roll"),
        [(TokenType::Rock, "rock"), (TokenType::Roll, "roll"),],
    );

    assert_eq!(
        no_ranges("takes wants return give send back"),
        [
            (TokenType::Takes, "takes"),
            (TokenType::Takes, "wants"),
            (TokenType::Return, "return"),
            (TokenType::Return, "give"),
            (TokenType::Return, "send"),
            (TokenType::Back, "back"),
        ],
    );
}
#[test]
fn lex_comments() {
    assert_eq!(
        lex("()"),
        [Token::new(TokenType::Comment(""), "()", line_range(0, 2))]
    );
    assert_eq!(
        lex("(hi)"),
        [Token::new(
            TokenType::Comment("hi"),
            "(hi)",
            line_range(0, 4)
        )]
    );
    assert_eq!(
        lex("(hi there)"),
        [Token::new(
            TokenType::Comment("hi there"),
            "(hi there)",
            line_range(0, 10)
        )]
    );
    assert_eq!(
        lex("hi(hi)hi"),
        [
            Token::new(TokenType::Word, "hi", line_range(0, 2)),
            Token::new(TokenType::Comment("hi"), "(hi)", line_range(2, 6)),
            Token::new(TokenType::Word, "hi", line_range(6, 8))
        ]
    );
    assert_eq!(
        lex("    (multi\nline)"),
        [Token::new(
            TokenType::Comment("multi\nline"),
            "(multi\nline)",
            SourceLocation::new(1, 4).to((2, 5).into())
        )]
    );

    assert_eq!(
        lex("(unterminated"),
        [Token::new(
            TokenType::Error(ErrorMessage("Unterminated comment")),
            "(unterminated",
            line_range(0, 13)
        )]
    );
    assert_eq!(
        lex("(untermi\nated"),
        [Token::new(
            TokenType::Error(ErrorMessage("Unterminated comment")),
            "(untermi\nated",
            SourceLocation::new(1, 0).to((2, 4).into())
        )]
    );
}

#[test]
fn lex_strings() {
    assert_eq!(
        lex("\"Hello San Francisco\""),
        [Token::new(
            TokenType::StringLiteral("Hello San Francisco"),
            "\"Hello San Francisco\"",
            line_range(0, 21)
        )]
    );

    assert_eq!(
        lex("\"unterminated"),
        [Token::new(
            TokenType::Error(ErrorMessage("Unterminated string literal")),
            "\"unterminated",
            line_range(0, 13)
        )]
    );
    assert_eq!(
        lex("\"untermi\nated"),
        [Token::new(
            TokenType::Error(ErrorMessage("Unterminated string literal")),
            "\"untermi\nated",
            SourceLocation::new(1, 0).to((2, 4).into())
        )]
    );
}

#[test]
fn lex_apostrophe_stuff() {
    assert_eq!(
        lex("isnt isn't aint ain't"),
        [
            Token::new(TokenType::Isnt, "isnt", line_range(0, 4)),
            Token::new(TokenType::Isnt, "isn't", line_range(5, 10)),
            Token::new(TokenType::Isnt, "aint", line_range(11, 15)),
            Token::new(TokenType::Isnt, "ain't", line_range(16, 21))
        ]
    );
    assert_eq!(
        lex("foo's"),
        [
            Token::new(TokenType::Word, "foo", line_range(0, 3)),
            Token::new(TokenType::ApostropheS, "'s", line_range(3, 5))
        ]
    );
    assert_eq!(
        lex("he's"),
        [
            Token::new(TokenType::Pronoun, "he", line_range(0, 2)),
            Token::new(TokenType::ApostropheS, "'s", line_range(2, 4))
        ]
    );
    assert_eq!(
        lex("he's's"),
        [
            Token::new(TokenType::Word, "he's", line_range(0, 4)),
            Token::new(TokenType::ApostropheS, "'s", line_range(4, 6))
        ]
    );
    assert_eq!(
        lex("nothing's"),
        [
            Token::new(TokenType::Null, "nothing", line_range(0, 7)),
            Token::new(TokenType::ApostropheS, "'s", line_range(7, 9))
        ]
    );
    assert_eq!(
        lex("we're"),
        [
            Token::new(TokenType::Word, "we", line_range(0, 2)),
            Token::new(TokenType::ApostropheRE, "'re", line_range(2, 5))
        ]
    );

    assert_eq!(
        lex("foo 's"),
        [
            Token::new(TokenType::Word, "foo", line_range(0, 3)),
            Token::new(TokenType::Word, "s", line_range(5, 6))
        ]
    );
    assert_eq!(
        lex("he' s"),
        [
            Token::new(TokenType::Pronoun, "he", line_range(0, 2)),
            Token::new(TokenType::Word, "s", line_range(4, 5))
        ]
    );
    assert_eq!(
        lex("nothing 's"),
        [
            Token::new(TokenType::Null, "nothing", line_range(0, 7)),
            Token::new(TokenType::Word, "s", line_range(9, 10))
        ]
    );
    assert_eq!(
        lex("we' re"),
        [
            Token::new(TokenType::Word, "we", line_range(0, 2)),
            Token::new(TokenType::Word, "re", line_range(4, 6))
        ]
    );

    assert_eq!(
        lex("ain't talkin' 'bout love"),
        [
            Token::new(TokenType::Isnt, "ain't", line_range(0, 5)),
            Token::new(TokenType::Word, "talkin", line_range(6, 12)),
            Token::new(TokenType::Word, "bout", line_range(15, 19)),
            Token::new(TokenType::Word, "love", line_range(20, 24))
        ]
    );
    assert_eq!(
        lex("rock'n'roll"),
        [Token::new(
            TokenType::Word,
            "rock'n'roll",
            line_range(0, 11)
        )]
    );
}

#[test]
fn lex_apostrophe_after_literal() {
    assert_eq!(
        lex("1's"),
        [
            Token::new(TokenType::Number(1.0), "1", line_range(0, 1)),
            Token::new(TokenType::ApostropheS, "'s", line_range(1, 3))
        ]
    );
    assert_eq!(
        lex("\"foo\"'s"),
        [
            Token::new(TokenType::StringLiteral("foo"), "\"foo\"", line_range(0, 5)),
            Token::new(TokenType::ApostropheS, "'s", line_range(5, 7))
        ]
    );
    assert_eq!(
        lex("\"foo\"'re"),
        [
            Token::new(TokenType::StringLiteral("foo"), "\"foo\"", line_range(0, 5)),
            Token::new(TokenType::ApostropheRE, "'re", line_range(5, 8))
        ]
    );
}

#[test]
fn lex_errors() {
    assert_eq!(
        lex("3bca"),
        [Token::new(
            TokenType::Error(ErrorMessage("Invalid token")),
            "3bca",
            line_range(0, 4)
        )]
    );
    assert_eq!(
        lex("abc3"),
        [Token::new(
            TokenType::Error(ErrorMessage(
                "Identifier may not contain non-alphabetic characters"
            )),
            "abc3",
            line_range(0, 4)
        )]
    );
    assert_eq!(
        lex("ab3c"),
        [Token::new(
            TokenType::Error(ErrorMessage(
                "Identifier may not contain non-alphabetic characters"
            )),
            "ab3c",
            line_range(0, 4)
        )]
    );

    // underscores are technically ascii punctuation, but I'm specifically disallowing them attached to identifiers
    assert_eq!(
        lex("ab_c"),
        [Token::new(
            TokenType::Error(ErrorMessage(
                "Identifier may not contain non-alphabetic characters"
            )),
            "ab_c",
            line_range(0, 4)
        )]
    );

    assert_eq!(
        lex("_ _hilarious_easter_egg"),
        [
            Token::new(
                TokenType::Error(ErrorMessage(
                    "'_' is not a valid character because it can't be sung"
                )),
                "_",
                line_range(0, 1)
            ),
            Token::new(
                TokenType::Error(ErrorMessage(
                    "'_' is not a valid character because it can't be sung"
                )),
                "_hilarious_easter_egg",
                line_range(2, 23)
            )
        ]
    )
}

#[test]
fn skip_comments() {
    assert_eq!(skipping_comments(""), []);
    assert_eq!(skipping_comments("()"), []);
    assert_eq!(
        skipping_comments("hi(hi)hi"),
        [
            Token::new(TokenType::Word, "hi", line_range(0, 2)),
            Token::new(TokenType::Word, "hi", line_range(6, 8))
        ]
    );
}

#[test]
fn test_is_word() {
    assert!(is_word("foo"));
    assert!(is_word("baR"));
    assert!(!is_word("baz quux"));
    assert!(!is_word("+"));
    assert!(is_word("rock'n'roll"));
}

#[test]
fn get_literal_text() {
    let mut lexer = Lexer::new("flan is my friend's  favorite   *  food");

    let tokens = lexer.by_ref().collect::<Vec<_>>();
    assert_eq!(
        tokens,
        [
            Token::new(TokenType::Word, "flan", line_range(0, 4)),
            Token::new(TokenType::Is, "is", line_range(5, 7)),
            Token::new(TokenType::CommonVariablePrefix, "my", line_range(8, 10)),
            Token::new(TokenType::Word, "friend", line_range(11, 17)),
            Token::new(TokenType::ApostropheS, "'s", line_range(17, 19)),
            Token::new(TokenType::Word, "favorite", line_range(21, 29)),
            Token::new(TokenType::Multiply, "*", line_range(32, 33)),
            Token::new(TokenType::Word, "food", line_range(35, 39)),
        ]
    );

    // get_start_index_of shouldn't recognize tokens we create ourselves--
    // only tokens we actually got back from the lexer
    assert_eq!(
        lexer.get_start_index_of(&Token::new(TokenType::Word, "flan", line_range(0, 4))),
        None
    );
    assert_eq!(lexer.get_start_index_of(tokens.first().unwrap()), Some(0));

    assert_eq!(
        lexer.get_literal_text_between(tokens.first().unwrap(), tokens.last().unwrap()),
        Some("flan is my friend's  favorite   *  ")
    );
    assert_eq!(
        lexer.get_literal_text_between(tokens.last().unwrap(), tokens.first().unwrap()),
        None
    );
    assert_eq!(
        lexer.get_literal_text_between(
            tokens.first().unwrap(),
            &Token::new(TokenType::Word, "food", line_range(35, 39))
        ),
        None
    );
    assert_eq!(
        lexer.get_literal_text_between(
            &Token::new(TokenType::Word, "flan", line_range(0, 4)),
            tokens.last().unwrap()
        ),
        None
    );
    assert_eq!(
        lexer.get_literal_text_between(&tokens[3], tokens.last().unwrap()),
        Some("friend's  favorite   *  ")
    );

    assert_eq!(
        lexer.get_literal_text_after(tokens.first().unwrap()),
        Some("flan is my friend's  favorite   *  food")
    );
    assert_eq!(
        lexer.get_literal_text_after(&Token::new(TokenType::Word, "flan", line_range(0, 4))),
        None
    );
    assert_eq!(
        lexer.get_literal_text_after(&tokens[3]),
        Some("friend's  favorite   *  food")
    );
}

#[test]
fn current_loc() {
    let loc_at = |buf, idx| {
        let mut lexer = Lexer::new(buf);
        lexer.by_ref().take(idx).for_each(drop);
        lexer.current_loc()
    };

    assert_eq!(loc_at("hello", 0), (1, 0).into());
    assert_eq!(loc_at("hello", 1), (1, 5).into());
    assert_eq!(loc_at("hello\n", 1), (1, 5).into());
    assert_eq!(loc_at("hello\n", 2), (2, 0).into());
    assert_eq!(loc_at("(hello, \nworld)hi", 1), (2, 6).into());
    assert_eq!(loc_at("\n\n\n\n\n(hello, \nworld)hi", 999), (7, 8).into());
}
