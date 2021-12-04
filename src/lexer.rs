use std::{collections::HashMap, iter::repeat, slice::SliceIndex, str::CharIndices};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ErrorMessage(&'static str);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType<'a> {
    Word,
    CapitalizedWord,
    StringLiteral(&'a str),
    Number(f64),
    Mysterious,
    Null,
    True,
    False,
    Empty,

    CommonVariablePrefix,
    Pronoun,

    Plus,
    Minus,
    Multiply,
    Divide,

    Is,
    Isnt,
    Put,
    Into,
    Let,
    Be,
    With,

    ApostropheS,
    ApostropheRE,

    Dot,
    Newline,
    Comment(&'a str),
    Error(ErrorMessage),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Token<'a> {
    pub id: TokenType<'a>,
    pub spelling: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(id: TokenType<'a>, spelling: &'a str) -> Self {
        Token { id, spelling }
    }
    pub fn is_comment(&self) -> bool {
        self.id.is_comment()
    }
}

impl<'a> TokenType<'a> {
    pub fn is_comment(&self) -> bool {
        matches!(self, TokenType::Comment(_))
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType<'static>> = {
        let mut m = HashMap::new();
        m.insert("mysterious", TokenType::Mysterious);

        let mut alias = |token, names: &[&'static str]| {
            m.extend(names.iter().cloned().zip(repeat(token)));
        };

        alias(
            TokenType::Null,
            &["null", "nothing", "nowhere", "nobody", "gone"],
        );

        alias(TokenType::True, &["true", "right", "yes", "ok"]);
        alias(TokenType::False, &["false", "wrong", "no", "lies"]);

        alias(TokenType::Empty, &["empty", "silent", "silence"]);

        alias(
            TokenType::Pronoun,
            &[
                "it", "he", "she", "him", "her", "they", "them", "ze", "hir", "zie", "zir", "xe",
                "xem", "ve", "ver",
            ],
        );

        alias(TokenType::Plus, &["plus"]);
        alias(TokenType::Minus, &["minus", "without"]);
        alias(TokenType::Multiply, &["times", "of"]);
        alias(TokenType::Divide, &["over", "between"]);

        alias(TokenType::Into, &["in", "into"]);

        alias(TokenType::Is, &["is", "are", "was", "were"]);
        alias(
            TokenType::Isnt,
            &[
                "isnt", "isn't", "aint", "ain't", "arent", "aren't", "wasnt", "wasn't", "werent",
                "weren't",
            ],
        );

        m.extend([
            ("with", TokenType::With),
            ("put", TokenType::Put),
            ("let", TokenType::Let),
            ("be", TokenType::Be),
        ]);

        m.extend(
            ["a", "an", "the", "my", "your", "our"]
                .iter()
                .map(|pre| (*pre, TokenType::CommonVariablePrefix)),
        );

        m
    };
}

fn match_keyword<'a>(word: &'a str) -> Option<Token<'a>> {
    KEYWORDS
        .get(word.to_lowercase().as_str())
        .map(|tok| Token::new(*tok, word))
}

fn is_ignorable_whitespace(c: char) -> bool {
    c.is_whitespace() && c != '\n'
}

fn is_ignorable_punctuation(c: char) -> bool {
    c.is_ascii_punctuation() && c != '_' && c != '\''
}

pub fn is_word(text: &str) -> bool {
    text.chars()
        .all(|c| !(c.is_whitespace() || is_ignorable_punctuation(c)))
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
    staged: Option<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &'a str) -> Self {
        Self {
            buf,
            char_indices: buf.char_indices(),
            staged: None,
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
        let text = self.substr(start..end);
        text.parse::<f64>().ok().map(|n| LexResult {
            token: Token::new(TokenType::Number(n), text),
            end,
        })
    }

    fn find_next_word_end(&self) -> usize {
        self.find_next_index(|c| c.is_whitespace() || is_ignorable_punctuation(c))
    }

    fn find_word_type(&self, word: &'a str) -> Token<'a> {
        assert!(!word.is_empty());
        match_keyword(word).unwrap_or_else(|| {
            Token::new(
                word.chars()
                    .next()
                    .unwrap()
                    .is_uppercase()
                    .then(|| TokenType::CapitalizedWord)
                    .unwrap_or(TokenType::Word),
                word,
            )
        })
    }

    fn last_n(text: &str, n: usize) -> &str {
        assert!(text.len() >= n);
        &text[(text.len() - n)..]
    }

    fn tokenize_word(&mut self, word: &'a str, end: usize) -> LexResult<'a> {
        let (stripped, staged_type) = word
            .strip_suffix("'s")
            .map(|stripped| (stripped, Some((TokenType::ApostropheS, 2))))
            .or_else(|| {
                word.strip_suffix("'re")
                    .map(|stripped| (stripped, Some((TokenType::ApostropheRE, 3))))
            })
            .unwrap_or_else(|| (word.trim_end_matches('\''), None));
        self.staged = staged_type.map(|(id, len)| Token::new(id, Self::last_n(word, len)));
        let token = self.find_word_type(stripped);
        LexResult { token, end }
    }

    fn scan_word(&mut self, start: usize) -> LexResult<'a> {
        let end = self.find_next_word_end();
        let text = self.substr(start..end);
        Some(text)
            .filter(|s| s.chars().all(|c| c.is_alphabetic() || c == '\''))
            .map(|word| self.tokenize_word(word, end))
            .unwrap_or_else(|| LexResult {
                token: Token::new(
                    TokenType::Error(ErrorMessage(
                        "Identifier may not contain non-alphabetic characters",
                    )),
                    text,
                ),
                end,
            })
    }

    fn scan_keyword(&self, start: usize) -> Option<LexResult<'a>> {
        let end = self.find_next_word_end();
        match_keyword(self.substr(start..end)).map(|token| LexResult { token, end })
    }

    fn scan_delimited<F: FnOnce(&'a str) -> TokenType<'a>>(
        &self,
        open: usize,
        close_char: char,
        factory: F,
        error: ErrorMessage,
    ) -> LexResult<'a> {
        let mut iter = self.char_indices.clone();
        if let Some((close, _)) = iter.find(|&(_, c)| c == close_char) {
            let end = close + 1;
            let text = self.substr(open..end);
            LexResult {
                token: Token::new(factory(self.substr((open + 1)..close)), text),
                end,
            }
        } else {
            LexResult {
                token: Token::new(TokenType::Error(error), self.substr(open..)),
                end: self.buf.len(),
            }
        }
    }

    fn scan_comment(&self, open: usize) -> LexResult<'a> {
        self.scan_delimited(
            open,
            ')',
            |s| TokenType::Comment(s),
            ErrorMessage("Unterminated comment"),
        )
    }

    fn scan_string_literal(&self, open: usize) -> LexResult<'a> {
        self.scan_delimited(
            open,
            '"',
            |s| TokenType::StringLiteral(s),
            ErrorMessage("Unterminated string literal"),
        )
    }

    fn make_error_token(&self, start: usize, error: ErrorMessage) -> LexResult<'a> {
        let end = self.find_next_word_end();
        LexResult {
            token: Token::new(TokenType::Error(error), self.substr(start..end)),
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
                let char_token = |token_type| LexResult {
                    token: Token::new(token_type, self.substr(start..(start + 1))),
                    end: start + 1,
                };
                let LexResult { token, end } = match start_char {
                    '\n' => char_token(TokenType::Newline),

                    '.' => {
                        // might be a number starting with a decimal, like .123
                        if let Some(number) = self.scan_number(start) {
                            number
                        } else {
                            char_token(TokenType::Dot)
                        }
                    }

                    '\'' => continue,
                    '+' => char_token(TokenType::Plus),
                    '-' => char_token(TokenType::Minus),
                    '*' => char_token(TokenType::Multiply),
                    '/' => char_token(TokenType::Divide),

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
        self.staged.take().or_else(|| self.match_loop())
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
        lex("is isn't put let be"),
        vec![
            Token::new(TokenType::Is, "is"),
            Token::new(TokenType::Isnt, "isn't"),
            Token::new(TokenType::Put, "put"),
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Be, "be"),
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
