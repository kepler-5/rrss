use std::{collections::HashMap, iter::repeat, slice::SliceIndex, str::CharIndices};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ErrorMessage(&'static str);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token<'a> {
    Word(&'a str),
    CapitalizedWord(&'a str),
    StringLiteral(&'a str),
    Number(f64),
    Mysterious,
    Null,
    True,
    False,
    Empty,

    CommonVariablePrefix(&'a str),
    Pronoun,

    Plus,
    Minus,
    Multiply,
    Divide,

    Put,
    Into,
    Let,
    Be,
    With,

    Newline,
    Comment(&'a str),
    Error(&'a str, ErrorMessage),
}

impl<'a> Token<'a> {
    pub fn is_comment(&self) -> bool {
        matches!(self, Token::Comment(_))
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token<'static>> = {
        let mut m = HashMap::new();
        m.insert("mysterious", Token::Mysterious);

        let mut alias = |token, names: &[&'static str]| {
            m.extend(names.iter().cloned().zip(repeat(token)));
        };

        alias(
            Token::Null,
            &["null", "nothing", "nowhere", "nobody", "gone"],
        );

        alias(Token::True, &["true", "right", "yes", "ok"]);
        alias(Token::False, &["false", "wrong", "no", "lies"]);

        alias(Token::Empty, &["empty", "silent", "silence"]);

        alias(
            Token::Pronoun,
            &[
                "it", "he", "she", "him", "her", "they", "them", "ze", "hir", "zie", "zir", "xe",
                "xem", "ve", "ver",
            ],
        );

        alias(Token::Plus, &["plus"]);
        alias(Token::Minus, &["minus", "without"]);
        alias(Token::Multiply, &["times", "of"]);
        alias(Token::Divide, &["over", "between"]);

        m.insert("with", Token::With);

        m.extend(
            ["a", "an", "the", "my", "your", "our"]
                .iter()
                .map(|pre| (*pre, Token::CommonVariablePrefix(pre))),
        );

        m.insert("put", Token::Put);
        m.insert("into", Token::Into);
        m.insert("let", Token::Let);
        m.insert("be", Token::Be);

        m
    };
}

fn match_keyword<'a>(word: &'a str) -> Option<Token<'a>> {
    fn transform<'a>(tok: &Token<'static>, word: &'a str) -> Token<'a> {
        match tok {
            Token::CommonVariablePrefix(_) => Token::CommonVariablePrefix(word), // keep the capitalization of CommonVariablePrefix as written in the source

            _ => tok.clone(),
        }
    }
    KEYWORDS
        .get(word.to_lowercase().as_str())
        .map(|tok| transform(tok, word))
}

fn is_ignorable_whitespace(c: char) -> bool {
    c.is_whitespace() && c != '\n'
}

fn is_ignorable_punctuation(c: char) -> bool {
    c.is_ascii_punctuation() && c != '_'
}

fn find_word_start<'a>(
    char_indices: &mut CharIndices,
) -> Option<<CharIndices<'a> as Iterator>::Item> {
    char_indices.find(|&(_, c)| !is_ignorable_whitespace(c))
}

struct LexResult<'a> {
    token: Token<'a>,
    end: usize,
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    buf: &'a str,
    char_indices: CharIndices<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &'a str) -> Self {
        Self {
            buf,
            char_indices: buf.char_indices(),
        }
    }

    pub fn skip_comments(self) -> CommentSkippingLexer<'a> {
        CommentSkippingLexer::new(self)
    }

    fn substr<I: SliceIndex<str>>(&self, slice: I) -> &'a <I as SliceIndex<str>>::Output {
        #[cfg(not(debug_assertions))]
        unsafe {
            self.buf.get_unchecked(slice)
        }
        #[cfg(debug_assertions)]
        &self.buf[slice]
    }

    fn find_next_index<P: Fn(char) -> bool>(&self, pred: P) -> usize {
        self.char_indices
            .clone()
            .find(|t| pred(t.1))
            .map(|t| t.0)
            .unwrap_or(self.buf.len())
    }

    fn scan_number(&self, start: usize) -> Option<LexResult<'a>> {
        let end = self
            .find_next_index(|c| c.is_whitespace() || (is_ignorable_punctuation(c) && c != '.'));
        self.substr(start..end)
            .parse::<f64>()
            .ok()
            .map(|n| LexResult {
                token: Token::Number(n),
                end,
            })
    }

    fn find_next_word_end(&self) -> usize {
        self.find_next_index(|c| c.is_whitespace() || is_ignorable_punctuation(c))
    }

    fn find_word_type(&self, word: &'a str) -> Token<'a> {
        assert!(!word.is_empty());
        if word.chars().next().unwrap().is_uppercase() {
            Token::CapitalizedWord(word)
        } else {
            Token::Word(word)
        }
    }

    fn scan_word(&self, start: usize) -> LexResult<'a> {
        let end = self.find_next_word_end();
        Some(self.substr(start..end))
            .filter(|s| s.chars().all(|c| c.is_alphabetic()))
            .map(|word| LexResult {
                token: self.find_word_type(word),
                end,
            })
            .unwrap_or_else(|| LexResult {
                token: Token::Error(
                    self.substr(start..end),
                    ErrorMessage("Identifier may not contain non-alphabetic characters"),
                ),
                end,
            })
    }

    fn scan_keyword(&self, start: usize) -> Option<LexResult<'a>> {
        let end = self.find_next_word_end();
        match_keyword(self.substr(start..end)).map(|token| LexResult { token, end })
    }

    fn scan_delimited<F: FnOnce(&'a str) -> Token<'a>>(
        &self,
        open: usize,
        close_char: char,
        factory: F,
        error: ErrorMessage,
    ) -> LexResult<'a> {
        let mut iter = self.char_indices.clone();
        if let Some((close, _)) = iter.find(|&(_, c)| c == close_char) {
            LexResult {
                token: factory(self.substr((open + 1)..close)),
                end: close + 1,
            }
        } else {
            LexResult {
                token: Token::Error(self.substr(open..), error),
                end: self.buf.len(),
            }
        }
    }

    fn scan_comment(&self, open: usize) -> LexResult<'a> {
        self.scan_delimited(
            open,
            ')',
            |s| Token::Comment(s),
            ErrorMessage("Unterminated comment"),
        )
    }

    fn scan_string_literal(&self, open: usize) -> LexResult<'a> {
        self.scan_delimited(
            open,
            '"',
            |s| Token::StringLiteral(s),
            ErrorMessage("Unterminated string literal"),
        )
    }

    fn make_error_token(&self, start: usize, error: ErrorMessage) -> LexResult<'a> {
        let end = self.find_next_word_end();
        LexResult {
            token: Token::Error(self.substr(start..end), error),
            end,
        }
    }

    fn advance_to(&mut self, idx: usize, start: usize) {
        assert!(idx > start);
        for _ in 0..(idx - start - 1) {
            self.char_indices.next();
        }
    }

    fn match_loop(&mut self) -> Option<<Self as Iterator>::Item> {
        loop {
            if let Some((start, start_char)) = find_word_start(&mut self.char_indices) {
                let char_token = |token| LexResult {
                    token,
                    end: start + 1,
                };
                let LexResult { token, end } = match start_char {
                    '\n' => char_token(Token::Newline),

                    '.' => {
                        // might be a number starting with a decimal, like .123
                        if let Some(number) = self.scan_number(start) {
                            number
                        } else {
                            continue; // ignore
                        }
                    }

                    '+' => char_token(Token::Plus),
                    '-' => char_token(Token::Minus),
                    '*' => char_token(Token::Multiply),
                    '/' => char_token(Token::Divide),

                    '"' => self.scan_string_literal(start),
                    '(' => self.scan_comment(start),

                    '_' => self.make_error_token(
                        start,
                        ErrorMessage("'_' is not a valid character because it can't be sung"),
                    ),

                    c => {
                        if is_ignorable_punctuation(c) {
                            continue;
                        }
                        let error = || self.make_error_token(start, ErrorMessage("Invalid token"));
                        if c.is_numeric() {
                            self.scan_number(start).unwrap_or_else(error)
                        } else if c.is_alphabetic() {
                            self.scan_keyword(start)
                                .unwrap_or_else(|| self.scan_word(start))
                        } else {
                            error()
                        }
                    }
                };
                self.advance_to(end, start);
                return Some(token);
            } else {
                return None;
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.match_loop()
    }
}

pub struct CommentSkippingLexer<'a> {
    lexer: Lexer<'a>,
}

impl<'a> CommentSkippingLexer<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }
}

impl<'a> Iterator for CommentSkippingLexer<'a> {
    type Item = <Lexer<'a> as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.find(|token| !token.is_comment())
    }
}

#[test]
fn lex() {
    let lex = |buf| Lexer::new(buf).collect::<Vec<_>>();
    assert_eq!(lex(""), vec![]);
    assert_eq!(lex("       "), vec![]);
    assert_eq!(lex("mysterious"), vec![Token::Mysterious]);
    assert_eq!(lex("      mysterious"), vec![Token::Mysterious]);
    assert_eq!(
        lex("  mysterious   mysterious    "),
        vec![Token::Mysterious, Token::Mysterious]
    );
    assert_eq!(
        lex("right yes ok true"),
        vec![Token::True, Token::True, Token::True, Token::True]
    );
    assert_eq!(
        lex("wrong no lies false"),
        vec![Token::False, Token::False, Token::False, Token::False],
    );
    assert_eq!(
        lex("empty silence silent"),
        vec![Token::Empty, Token::Empty, Token::Empty]
    );
    assert_eq!(
        lex("1 2 3.4  .6 -7"),
        vec![
            Token::Number(1.0),
            Token::Number(2.0),
            Token::Number(3.4),
            Token::Number(0.6),
            Token::Minus,
            Token::Number(7.0)
        ]
    );
    assert_eq!(
        lex("my MY My mY"),
        vec![
            Token::CommonVariablePrefix("my"),
            Token::CommonVariablePrefix("MY"),
            Token::CommonVariablePrefix("My"),
            Token::CommonVariablePrefix("mY"),
        ]
    );
    assert_eq!(
        lex("hello world"),
        vec![Token::Word("hello"), Token::Word("world")]
    );
    assert_eq!(
        lex("hello\n world"),
        vec![Token::Word("hello"), Token::Newline, Token::Word("world")]
    );
    assert_eq!(
        lex("hello \n world"),
        vec![Token::Word("hello"), Token::Newline, Token::Word("world")]
    );
    assert_eq!(
        lex("hello\nworld"),
        vec![Token::Word("hello"), Token::Newline, Token::Word("world")]
    );
    assert_eq!(
        lex("hello, world."),
        vec![Token::Word("hello"), Token::Word("world")]
    );
    assert_eq!(
        lex("hello?world!"), // the spec isn't crystal clear here, but I'm allowing this
        vec![Token::Word("hello"), Token::Word("world")]
    );
    assert_eq!(
        lex("Hello World world"),
        vec![
            Token::CapitalizedWord("Hello"),
            Token::CapitalizedWord("World"),
            Token::Word("world"),
        ]
    );
    assert_eq!(
        lex("\n\n\n"),
        vec![Token::Newline, Token::Newline, Token::Newline]
    );
    assert_eq!(
        lex("\r\n \r\n \r\n"),
        vec![Token::Newline, Token::Newline, Token::Newline]
    );

    assert_eq!(
        lex("+ - * /"),
        vec![Token::Plus, Token::Minus, Token::Multiply, Token::Divide]
    );

    assert_eq!(lex("plus with"), vec![Token::Plus, Token::With]);
    assert_eq!(lex("minus without"), vec![Token::Minus, Token::Minus]);
    assert_eq!(lex("times of"), vec![Token::Multiply, Token::Multiply]);
    assert_eq!(lex("over between"), vec![Token::Divide, Token::Divide]);

    assert_eq!(lex("()"), vec![Token::Comment("")]);
    assert_eq!(lex("(hi)"), vec![Token::Comment("hi")]);
    assert_eq!(lex("(hi there)"), vec![Token::Comment("hi there")]);
    assert_eq!(
        lex("hi(hi)hi"),
        vec![Token::Word("hi"), Token::Comment("hi"), Token::Word("hi")]
    );

    assert_eq!(
        lex("\"Hello San Francisco\""),
        vec![Token::StringLiteral("Hello San Francisco")]
    );

    assert_eq!(
        lex("1+1"),
        vec![Token::Number(1.0), Token::Plus, Token::Number(1.0)]
    );
    assert_eq!(
        lex("1+-1"),
        vec![
            Token::Number(1.0),
            Token::Plus,
            Token::Minus,
            Token::Number(1.0)
        ]
    );
    assert_eq!(
        lex("foo+1"),
        vec![Token::Word("foo"), Token::Plus, Token::Number(1.0)]
    );
    assert_eq!(
        lex("foo+-1"),
        vec![
            Token::Word("foo"),
            Token::Plus,
            Token::Minus,
            Token::Number(1.0)
        ]
    );
    assert_eq!(
        lex("1+foo"),
        vec![Token::Number(1.0), Token::Plus, Token::Word("foo")]
    );
    assert_eq!(
        lex("1+-foo"),
        vec![
            Token::Number(1.0),
            Token::Plus,
            Token::Minus,
            Token::Word("foo")
        ]
    );
    assert_eq!(
        lex("foo+foo"),
        vec![Token::Word("foo"), Token::Plus, Token::Word("foo")]
    );
    assert_eq!(
        lex("foo+-foo"),
        vec![
            Token::Word("foo"),
            Token::Plus,
            Token::Minus,
            Token::Word("foo")
        ]
    );
}

#[test]
fn lex_errors() {
    let lex = |buf| Lexer::new(buf).collect::<Vec<_>>();

    assert_eq!(
        lex("3bca"),
        vec![Token::Error("3bca", ErrorMessage("Invalid token"))]
    );
    assert_eq!(
        lex("abc3"),
        vec![Token::Error(
            "abc3",
            ErrorMessage("Identifier may not contain non-alphabetic characters")
        )]
    );
    assert_eq!(
        lex("ab3c"),
        vec![Token::Error(
            "ab3c",
            ErrorMessage("Identifier may not contain non-alphabetic characters")
        )]
    );

    // underscores are technically ascii punctuation, but I'm specifically disallowing them attached to identifiers
    assert_eq!(
        lex("ab_c"),
        vec![Token::Error(
            "ab_c",
            ErrorMessage("Identifier may not contain non-alphabetic characters")
        )]
    );

    assert_eq!(
        lex("_ _hilarious_easter_egg"),
        vec![
            Token::Error(
                "_",
                ErrorMessage("'_' is not a valid character because it can't be sung")
            ),
            Token::Error(
                "_hilarious_easter_egg",
                ErrorMessage("'_' is not a valid character because it can't be sung")
            )
        ]
    )
}

#[test]
fn skip_comments() {
    let lex = |buf| Lexer::new(buf).skip_comments().collect::<Vec<_>>();
    assert_eq!(lex(""), vec![]);
    assert_eq!(lex("()"), vec![]);
    assert_eq!(lex("hi(hi)hi"), vec![Token::Word("hi"), Token::Word("hi")]);
}
