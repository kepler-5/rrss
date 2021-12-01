use std::{collections::HashMap, iter::repeat, slice::SliceIndex, str::CharIndices};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ErrorMessage(&'static str);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token<'a> {
    Word(&'a str),
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

    Newline,
    Comment(&'a str),
    Error(&'a str, ErrorMessage),
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

        m.extend(
            ["a", "an", "the", "my", "your", "our"]
                .iter()
                .map(|pre| (*pre, Token::CommonVariablePrefix(pre))),
        );

        m
    };
}

fn match_keyword<'a>(word: &'a str) -> Option<Token<'a>> {
    KEYWORDS.get(word.to_lowercase().as_str()).cloned()
}

fn is_ignorable_whitespace(c: char) -> bool {
    c.is_whitespace() && c != '\n'
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

    fn substr<I: SliceIndex<str>>(&self, slice: I) -> &'a <I as SliceIndex<str>>::Output {
        // unsafe { self.buf.get_unchecked(slice) }
        &self.buf[slice]
    }

    fn find_next_iter<P: Fn(char) -> bool>(&self, pred: P) -> CharIndices<'a> {
        let mut iter = self.char_indices.clone();
        iter.find(|t| pred(t.1));
        iter
    }

    fn find_next_index<P: Fn(char) -> bool>(&self, pred: P) -> usize {
        self.char_indices
            .clone()
            .find(|t| pred(t.1))
            .map(|t| t.0)
            .unwrap_or(self.buf.len())
    }

    fn scan_number(&self, start: usize) -> Option<LexResult<'a>> {
        let end = self.find_next_index(|c| c.is_whitespace());
        self.substr(start..end)
            .parse::<f64>()
            .ok()
            .map(|n| LexResult {
                token: Token::Number(n),
                end,
            })
    }

    fn find_next_word_end(&self) -> usize {
        self.find_next_index(|c| c.is_whitespace() || c.is_ascii_punctuation())
    }

    fn scan_word(&self, start: usize) -> LexResult<'a> {
        let end = self.find_next_word_end();
        Some(self.substr(start..end))
            .filter(|s| s.chars().all(|c| c.is_alphabetic()))
            .map(|word| LexResult {
                token: Token::Word(word),
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

    fn scan_comment(&self, open: usize) -> LexResult<'a> {
        let mut iter = self.char_indices.clone();
        if let Some((close, _)) = iter.find(|&(_, c)| c == ')') {
            LexResult {
                token: Token::Comment(self.substr((open + 1)..close)),
                end: close + 1,
            }
        } else {
            LexResult {
                token: Token::Error(self.substr(open..), ErrorMessage("Unterminated comment")),
                end: self.buf.len(),
            }
        }
    }

    fn make_error_token(&self, start: usize) -> LexResult<'a> {
        let end = self.find_next_word_end();
        LexResult {
            token: Token::Error(self.substr(start..end), ErrorMessage("Invalid token")),
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
                    '-' => self
                        .scan_number(start)
                        .unwrap_or_else(|| char_token(Token::Minus)),
                    '*' => char_token(Token::Multiply),
                    '/' => char_token(Token::Divide),

                    '(' => self.scan_comment(start),

                    c => {
                        if c.is_ascii_punctuation() {
                            continue;
                        }
                        let error = || self.make_error_token(start);
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
            Token::Number(-7.0)
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

    assert_eq!(lex("()"), vec![Token::Comment("")]);
    assert_eq!(lex("(hi)"), vec![Token::Comment("hi")]);
    assert_eq!(lex("(hi there)"), vec![Token::Comment("hi there")]);
    assert_eq!(
        lex("hi(hi)hi"),
        vec![Token::Word("hi"), Token::Comment("hi"), Token::Word("hi")]
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
        lex("ab_c"),
        vec![Token::Error(
            "ab_c",
            ErrorMessage("Identifier may not contain non-alphabetic characters")
        )]
    );
}
