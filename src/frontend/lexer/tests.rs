use super::*;

#[test]
fn lex() {
    let lex = |buf| Lexer::new(buf).collect::<Vec<_>>();
    fn types<'a>(tokens: Vec<Token<'a>>) -> Vec<TokenType<'a>> {
        tokens.into_iter().map(|tok| tok.id).collect()
    }
    assert_eq!(lex(""), vec![]);
    assert_eq!(lex("       "), vec![]);
    assert_eq!(
        lex("mysterious"),
        vec![Token::new(TokenType::Mysterious, "mysterious")]
    );
    assert_eq!(
        lex("      mysterious"),
        vec![Token::new(TokenType::Mysterious, "mysterious")]
    );
    assert_eq!(
        lex("  mysterious   mysterious    "),
        vec![
            Token::new(TokenType::Mysterious, "mysterious"),
            Token::new(TokenType::Mysterious, "mysterious")
        ]
    );
    assert_eq!(
        lex("right yes ok true"),
        vec![
            Token::new(TokenType::True, "right"),
            Token::new(TokenType::True, "yes"),
            Token::new(TokenType::True, "ok"),
            Token::new(TokenType::True, "true"),
        ]
    );
    assert_eq!(
        lex("wrong no lies false"),
        vec![
            Token::new(TokenType::False, "wrong"),
            Token::new(TokenType::False, "no"),
            Token::new(TokenType::False, "lies"),
            Token::new(TokenType::False, "false"),
        ],
    );
    assert_eq!(
        lex("empty silence silent"),
        vec![
            Token::new(TokenType::Empty, "empty"),
            Token::new(TokenType::Empty, "silence"),
            Token::new(TokenType::Empty, "silent")
        ]
    );
    assert_eq!(
        lex("1 2.0 3.4  .6 -7"),
        vec![
            Token::new(TokenType::Number(1.0), "1"),
            Token::new(TokenType::Number(2.0), "2.0"),
            Token::new(TokenType::Number(3.4), "3.4"),
            Token::new(TokenType::Number(0.6), ".6"),
            Token::new(TokenType::Minus, "-"),
            Token::new(TokenType::Number(7.0), "7"),
        ]
    );
    assert_eq!(
        lex("my MY My mY"),
        vec![
            Token::new(TokenType::CommonVariablePrefix, "my"),
            Token::new(TokenType::CommonVariablePrefix, "MY"),
            Token::new(TokenType::CommonVariablePrefix, "My"),
            Token::new(TokenType::CommonVariablePrefix, "mY"),
        ]
    );

    assert_eq!(
        lex("hello world"),
        vec![
            Token::new(TokenType::Word, "hello"),
            Token::new(TokenType::Word, "world")
        ]
    );

    assert_eq!(
        lex("hello\n world"),
        vec![
            Token::new(TokenType::Word, "hello"),
            Token::new(TokenType::Newline, "\n"),
            Token::new(TokenType::Word, "world"),
        ]
    );
    assert_eq!(
        lex("hello \n world"),
        vec![
            Token::new(TokenType::Word, "hello"),
            Token::new(TokenType::Newline, "\n"),
            Token::new(TokenType::Word, "world"),
        ]
    );
    assert_eq!(
        lex("hello\nworld"),
        vec![
            Token::new(TokenType::Word, "hello"),
            Token::new(TokenType::Newline, "\n"),
            Token::new(TokenType::Word, "world"),
        ]
    );
    assert_eq!(
        lex("hello, world."),
        vec![
            Token::new(TokenType::Word, "hello"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Word, "world"),
            Token::new(TokenType::Dot, "."),
        ]
    );
    assert_eq!(
        lex("hello?world!"), // the spec isn't crystal clear here, but I'm allowing this
        vec![
            Token::new(TokenType::Word, "hello"),
            Token::new(TokenType::Word, "world"),
        ]
    );
    assert_eq!(
        lex("Hello World world"),
        vec![
            Token::new(TokenType::CapitalizedWord, "Hello"),
            Token::new(TokenType::CapitalizedWord, "World"),
            Token::new(TokenType::Word, "world"),
        ]
    );
    assert_eq!(
        types(lex("\n\n\n")),
        vec![TokenType::Newline, TokenType::Newline, TokenType::Newline]
    );
    assert_eq!(
        types(lex("\r\n \r\n \r\n")),
        vec![TokenType::Newline, TokenType::Newline, TokenType::Newline]
    );

    assert_eq!(
        lex("+ - * /"),
        vec![
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::Minus, "-"),
            Token::new(TokenType::Multiply, "*"),
            Token::new(TokenType::Divide, "/")
        ]
    );

    assert_eq!(
        lex("<= >= < >"),
        vec![
            Token::new(TokenType::LessEq, "<="),
            Token::new(TokenType::GreaterEq, ">="),
            Token::new(TokenType::Less, "<"),
            Token::new(TokenType::Greater, ">")
        ]
    );
    assert_eq!(
        lex("and or nor"),
        vec![
            Token::new(TokenType::And, "and"),
            Token::new(TokenType::Or, "or"),
            Token::new(TokenType::Nor, "nor"),
        ]
    );

    assert_eq!(
        lex("plus with"),
        vec![
            Token::new(TokenType::Plus, "plus"),
            Token::new(TokenType::With, "with")
        ]
    );
    assert_eq!(
        lex("minus without"),
        vec![
            Token::new(TokenType::Minus, "minus"),
            Token::new(TokenType::Minus, "without")
        ]
    );
    assert_eq!(
        lex("times of"),
        vec![
            Token::new(TokenType::Multiply, "times"),
            Token::new(TokenType::Multiply, "of")
        ]
    );
    assert_eq!(
        lex("over between"),
        vec![
            Token::new(TokenType::Divide, "over"),
            Token::new(TokenType::Divide, "between")
        ]
    );

    assert_eq!(
        lex("in into"),
        vec![
            Token::new(TokenType::Into, "in"),
            Token::new(TokenType::Into, "into")
        ]
    );
    assert_eq!(
        lex("is isn't put let be not at like"),
        vec![
            Token::new(TokenType::Is, "is"),
            Token::new(TokenType::Isnt, "isn't"),
            Token::new(TokenType::Put, "put"),
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Be, "be"),
            Token::new(TokenType::Not, "not"),
            Token::new(TokenType::At, "at"),
            Token::new(TokenType::Like, "like"),
        ]
    );
    assert_eq!(
        lex("is are was were"),
        vec![
            Token::new(TokenType::Is, "is"),
            Token::new(TokenType::Is, "are"),
            Token::new(TokenType::Is, "was"),
            Token::new(TokenType::Is, "were")
        ]
    );
    assert_eq!(
        lex("say says said"),
        vec![
            Token::new(TokenType::Say, "say"),
            Token::new(TokenType::Says, "says"),
            Token::new(TokenType::Says, "said"),
        ]
    );

    assert_eq!(
        lex("higher greater bigger stronger"),
        vec![
            Token::new(TokenType::Bigger, "higher"),
            Token::new(TokenType::Bigger, "greater"),
            Token::new(TokenType::Bigger, "bigger"),
            Token::new(TokenType::Bigger, "stronger"),
        ],
    );
    assert_eq!(
        lex("lower less smaller weaker"),
        vec![
            Token::new(TokenType::Smaller, "lower"),
            Token::new(TokenType::Smaller, "less"),
            Token::new(TokenType::Smaller, "smaller"),
            Token::new(TokenType::Smaller, "weaker"),
        ],
    );
    assert_eq!(
        lex("high great big strong"),
        vec![
            Token::new(TokenType::Big, "high"),
            Token::new(TokenType::Big, "great"),
            Token::new(TokenType::Big, "big"),
            Token::new(TokenType::Big, "strong"),
        ],
    );
    assert_eq!(
        lex("low little small weak"),
        vec![
            Token::new(TokenType::Small, "low"),
            Token::new(TokenType::Small, "little"),
            Token::new(TokenType::Small, "small"),
            Token::new(TokenType::Small, "weak"),
        ],
    );
    assert_eq!(
        lex("as than"),
        vec![
            Token::new(TokenType::As, "as"),
            Token::new(TokenType::Than, "than"),
        ],
    );

    assert_eq!(
        lex("if else while until"),
        vec![
            Token::new(TokenType::If, "if"),
            Token::new(TokenType::Else, "else"),
            Token::new(TokenType::While, "while"),
            Token::new(TokenType::Until, "until"),
        ],
    );

    assert_eq!(
        lex("build up knock down"),
        vec![
            Token::new(TokenType::Build, "build"),
            Token::new(TokenType::Up, "up"),
            Token::new(TokenType::Knock, "knock"),
            Token::new(TokenType::Down, "down"),
        ],
    );

    assert_eq!(
        lex("say shout whisper scream listen to"),
        vec![
            Token::new(TokenType::Say, "say"),
            Token::new(TokenType::SayAlias, "shout"),
            Token::new(TokenType::SayAlias, "whisper"),
            Token::new(TokenType::SayAlias, "scream"),
            Token::new(TokenType::Listen, "listen"),
            Token::new(TokenType::To, "to"),
        ],
    );

    assert_eq!(
        lex("cut split shatter"),
        vec![
            Token::new(TokenType::Cut, "cut"),
            Token::new(TokenType::Cut, "split"),
            Token::new(TokenType::Cut, "shatter"),
        ],
    );
    assert_eq!(
        lex("join unite"),
        vec![
            Token::new(TokenType::Join, "join"),
            Token::new(TokenType::Join, "unite"),
        ],
    );
    assert_eq!(
        lex("cast burn"),
        vec![
            Token::new(TokenType::Cast, "cast"),
            Token::new(TokenType::Cast, "burn"),
        ],
    );

    assert_eq!(
        lex("turn round around"),
        vec![
            Token::new(TokenType::Turn, "turn"),
            Token::new(TokenType::Round, "round"),
            Token::new(TokenType::Round, "around"),
        ],
    );

    assert_eq!(
        lex("continue break take top"),
        vec![
            Token::new(TokenType::Continue, "continue"),
            Token::new(TokenType::Break, "break"),
            Token::new(TokenType::Take, "take"),
            Token::new(TokenType::Top, "top"),
        ],
    );

    assert_eq!(
        lex("rock roll"),
        vec![
            Token::new(TokenType::Rock, "rock"),
            Token::new(TokenType::Roll, "roll"),
        ],
    );

    assert_eq!(
        lex("takes wants return give send back"),
        vec![
            Token::new(TokenType::Takes, "takes"),
            Token::new(TokenType::Takes, "wants"),
            Token::new(TokenType::Return, "return"),
            Token::new(TokenType::Return, "give"),
            Token::new(TokenType::Return, "send"),
            Token::new(TokenType::Back, "back"),
        ],
    );

    assert_eq!(
        lex("& 'n'"),
        vec![
            Token::new(TokenType::Ampersand, "&"),
            Token::new(TokenType::ApostropheNApostrophe, "'n'"),
        ],
    );

    assert_eq!(lex("()"), vec![Token::new(TokenType::Comment(""), "()")]);
    assert_eq!(
        lex("(hi)"),
        vec![Token::new(TokenType::Comment("hi"), "(hi)")]
    );
    assert_eq!(
        lex("(hi there)"),
        vec![Token::new(TokenType::Comment("hi there"), "(hi there)")]
    );
    assert_eq!(
        lex("hi(hi)hi"),
        vec![
            Token::new(TokenType::Word, "hi"),
            Token::new(TokenType::Comment("hi"), "(hi)"),
            Token::new(TokenType::Word, "hi")
        ]
    );

    assert_eq!(
        lex("\"Hello San Francisco\""),
        vec![Token::new(
            TokenType::StringLiteral("Hello San Francisco"),
            "\"Hello San Francisco\""
        )]
    );

    assert_eq!(
        types(lex("1+1")),
        vec![
            TokenType::Number(1.0),
            TokenType::Plus,
            TokenType::Number(1.0)
        ]
    );
    assert_eq!(
        types(lex("1+-1")),
        vec![
            TokenType::Number(1.0),
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Number(1.0)
        ]
    );
    assert_eq!(
        lex("foo+1"),
        vec![
            Token::new(TokenType::Word, "foo"),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::Number(1.0), "1")
        ]
    );
    assert_eq!(
        types(lex("foo+-1")),
        vec![
            TokenType::Word,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Number(1.0)
        ]
    );
    assert_eq!(
        types(lex("1+foo")),
        vec![TokenType::Number(1.0), TokenType::Plus, TokenType::Word]
    );
    assert_eq!(
        types(lex("1+-foo")),
        vec![
            TokenType::Number(1.0),
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Word
        ]
    );
    assert_eq!(
        types(lex("foo+foo")),
        vec![TokenType::Word, TokenType::Plus, TokenType::Word]
    );
    assert_eq!(
        types(lex("foo+-foo")),
        vec![
            TokenType::Word,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Word
        ]
    );
}

#[test]
fn lex_apostrophe_stuff() {
    let lex = |buf| Lexer::new(buf).collect::<Vec<_>>();

    assert_eq!(
        lex("isnt isn't aint ain't"),
        vec![
            Token::new(TokenType::Isnt, "isnt"),
            Token::new(TokenType::Isnt, "isn't"),
            Token::new(TokenType::Isnt, "aint"),
            Token::new(TokenType::Isnt, "ain't")
        ]
    );
    assert_eq!(
        lex("foo's"),
        vec![
            Token::new(TokenType::Word, "foo"),
            Token::new(TokenType::ApostropheS, "'s")
        ]
    );
    assert_eq!(
        lex("he's"),
        vec![
            Token::new(TokenType::Pronoun, "he"),
            Token::new(TokenType::ApostropheS, "'s")
        ]
    );
    assert_eq!(
        lex("he's's"),
        vec![
            Token::new(TokenType::Word, "he's"),
            Token::new(TokenType::ApostropheS, "'s")
        ]
    );
    assert_eq!(
        lex("nothing's"),
        vec![
            Token::new(TokenType::Null, "nothing"),
            Token::new(TokenType::ApostropheS, "'s")
        ]
    );
    assert_eq!(
        lex("we're"),
        vec![
            Token::new(TokenType::Word, "we"),
            Token::new(TokenType::ApostropheRE, "'re")
        ]
    );

    assert_eq!(
        lex("foo 's"),
        vec![
            Token::new(TokenType::Word, "foo"),
            Token::new(TokenType::Word, "s")
        ]
    );
    assert_eq!(
        lex("he' s"),
        vec![
            Token::new(TokenType::Pronoun, "he"),
            Token::new(TokenType::Word, "s")
        ]
    );
    assert_eq!(
        lex("nothing 's"),
        vec![
            Token::new(TokenType::Null, "nothing"),
            Token::new(TokenType::Word, "s")
        ]
    );
    assert_eq!(
        lex("we' re"),
        vec![
            Token::new(TokenType::Word, "we"),
            Token::new(TokenType::Word, "re")
        ]
    );

    assert_eq!(
        lex("ain't talkin' 'bout love"),
        vec![
            Token::new(TokenType::Isnt, "ain't"),
            Token::new(TokenType::Word, "talkin"),
            Token::new(TokenType::Word, "bout"),
            Token::new(TokenType::Word, "love")
        ]
    );
    assert_eq!(
        lex("rock'n'roll"),
        vec![Token::new(TokenType::Word, "rock'n'roll")]
    );
}

#[test]
#[ignore = "This fails currently and I'll fix it sometime soon!"]
fn lex_apostrophe_after_literal() {
    let lex = |buf| Lexer::new(buf).collect::<Vec<_>>();

    assert_eq!(
        lex("1's"),
        vec![
            Token::new(TokenType::Number(1.0), "1"),
            Token::new(TokenType::ApostropheS, "'s")
        ]
    );
    assert_eq!(
        lex("\"foo\"'s"),
        vec![
            Token::new(TokenType::Number(1.0), "1"),
            Token::new(TokenType::ApostropheS, "'s")
        ]
    );
}

#[test]
fn lex_errors() {
    let lex = |buf| Lexer::new(buf).collect::<Vec<_>>();

    assert_eq!(
        lex("3bca"),
        vec![Token::new(
            TokenType::Error(ErrorMessage("Invalid token")),
            "3bca"
        )]
    );
    assert_eq!(
        lex("abc3"),
        vec![Token::new(
            TokenType::Error(ErrorMessage(
                "Identifier may not contain non-alphabetic characters"
            )),
            "abc3"
        )]
    );
    assert_eq!(
        lex("ab3c"),
        vec![Token::new(
            TokenType::Error(ErrorMessage(
                "Identifier may not contain non-alphabetic characters"
            )),
            "ab3c"
        )]
    );

    // underscores are technically ascii punctuation, but I'm specifically disallowing them attached to identifiers
    assert_eq!(
        lex("ab_c"),
        vec![Token::new(
            TokenType::Error(ErrorMessage(
                "Identifier may not contain non-alphabetic characters"
            )),
            "ab_c"
        )]
    );

    assert_eq!(
        lex("_ _hilarious_easter_egg"),
        vec![
            Token::new(
                TokenType::Error(ErrorMessage(
                    "'_' is not a valid character because it can't be sung"
                )),
                "_"
            ),
            Token::new(
                TokenType::Error(ErrorMessage(
                    "'_' is not a valid character because it can't be sung"
                )),
                "_hilarious_easter_egg"
            )
        ]
    )
}

#[test]
fn skip_comments() {
    let lex = |buf| Lexer::new(buf).skip_comments().collect::<Vec<_>>();
    assert_eq!(lex(""), vec![]);
    assert_eq!(lex("()"), vec![]);
    assert_eq!(
        lex("hi(hi)hi"),
        vec![
            Token::new(TokenType::Word, "hi"),
            Token::new(TokenType::Word, "hi")
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
        vec![
            Token::new(TokenType::Word, "flan"),
            Token::new(TokenType::Is, "is"),
            Token::new(TokenType::CommonVariablePrefix, "my"),
            Token::new(TokenType::Word, "friend"),
            Token::new(TokenType::ApostropheS, "'s"),
            Token::new(TokenType::Word, "favorite"),
            Token::new(TokenType::Multiply, "*"),
            Token::new(TokenType::Word, "food"),
        ]
    );

    // get_start_index_of shouldn't recognize tokens we create ourselves--
    // only tokens we actually got back from the lexer
    assert_eq!(
        lexer.get_start_index_of(&Token::new(TokenType::Word, "flan")),
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
            &Token::new(TokenType::Word, "food")
        ),
        None
    );
    assert_eq!(
        lexer
            .get_literal_text_between(&Token::new(TokenType::Word, "flan"), tokens.last().unwrap()),
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
        lexer.get_literal_text_after(&Token::new(TokenType::Word, "flan")),
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
