use std::{collections::HashMap, iter::repeat, slice::SliceIndex, str::CharIndices};

use derive_more::Constructor;

#[cfg(test)]
mod tests;

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
    At,
    Like,

    Plus,
    Minus,
    Multiply,
    Divide,

    Is,
    Isnt,
    Says,
    Put,
    Into,
    Let,
    Be,
    With,
    Not,

    ApostropheS,
    ApostropheRE,

    And,
    Or,
    Nor,
    As,
    Big,
    Bigger,
    Small,
    Smaller,
    Than,

    Greater,
    GreaterEq,
    Less,
    LessEq,

    If,
    Else,
    While,
    Until,
    Continue,
    Break,
    Take,
    Top,

    Say,
    SayAlias,
    Listen,
    To,

    Build,
    Knock,
    Up,
    Down,

    Cut,
    Join,
    Cast,
    Turn,
    Round,

    Rock,
    Roll,

    Takes,
    Taking,
    Return,
    Back,
    Ampersand,
    ApostropheNApostrophe,

    Comma,
    Dot,
    Newline,
    Comment(&'a str),
    Error(ErrorMessage),
}

#[derive(Clone, Copy, Constructor, Debug, PartialEq)]
pub struct Token<'a> {
    pub id: TokenType<'a>,
    pub spelling: &'a str,
}

impl<'a> Token<'a> {
    pub fn is_comment(&self) -> bool {
        self.id.is_comment()
    }
    pub fn is_ispelled(&self, text: &str) -> bool {
        assert!(text.chars().all(|c| c.is_lowercase()));
        self.spelling.to_lowercase() == text
    }
}

impl<'a> TokenType<'a> {
    pub fn is_comment(&self) -> bool {
        matches!(self, TokenType::Comment(_))
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType<'static>> = {
        let mut m = HashMap::with_capacity(128);
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

        alias(TokenType::Says, &["says", "said"]);

        alias(
            TokenType::Bigger,
            &["higher", "greater", "bigger", "stronger"],
        );
        alias(TokenType::Smaller, &["lower", "less", "smaller", "weaker"]);
        alias(TokenType::Big, &["high", "great", "big", "strong"]);
        alias(TokenType::Small, &["low", "little", "small", "weak"]);

        alias(TokenType::SayAlias, &["shout", "whisper", "scream"]);

        alias(TokenType::Cut, &["cut", "split", "shatter"]);
        alias(TokenType::Join, &["join", "unite"]);
        alias(TokenType::Cast, &["cast", "burn"]);
        alias(TokenType::Round, &["round", "around"]);
        alias(TokenType::Takes, &["takes", "wants"]);
        alias(TokenType::Return, &["return", "give", "send"]);

        m.extend([
            ("with", TokenType::With),
            ("put", TokenType::Put),
            ("let", TokenType::Let),
            ("be", TokenType::Be),
            ("and", TokenType::And),
            ("or", TokenType::Or),
            ("nor", TokenType::Nor),
            ("not", TokenType::Not),
            ("as", TokenType::As),
            ("than", TokenType::Than),
            ("if", TokenType::If),
            ("else", TokenType::Else),
            ("while", TokenType::While),
            ("until", TokenType::Until),
            ("build", TokenType::Build),
            ("knock", TokenType::Knock),
            ("up", TokenType::Up),
            ("down", TokenType::Down),
            ("say", TokenType::Say),
            ("listen", TokenType::Listen),
            ("to", TokenType::To),
            ("turn", TokenType::Turn),
            ("continue", TokenType::Continue),
            ("break", TokenType::Break),
            ("take", TokenType::Take),
            ("top", TokenType::Top),
            ("rock", TokenType::Rock),
            ("roll", TokenType::Roll),
            ("at", TokenType::At),
            ("like", TokenType::Like),
            ("taking", TokenType::Taking),
            ("back", TokenType::Back),
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
    char_indices
        .as_str()
        .starts_with("'n'")
        .then(|| char_indices.next())
        .unwrap_or_else(|| char_indices.find(|&(_, c)| !is_ignorable_whitespace(c)))
}

struct LexResult<'a> {
    token: Token<'a>,
    end: usize,
    newlines: usize,
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    buf: &'a str,
    char_indices: CharIndices<'a>,
    staged: Option<Token<'a>>,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &'a str) -> Self {
        Self {
            buf,
            char_indices: buf.char_indices(),
            staged: None,
            line: 1,
        }
    }

    pub fn skip_comments(self) -> CommentSkippingLexer<'a> {
        CommentSkippingLexer::new(self)
    }

    pub fn get_start_index_of(&self, cursor: &Token<'a>) -> Option<usize> {
        let cursor_start = cursor.spelling.as_ptr();
        let bytes = self.buf.as_bytes();
        bytes
            .as_ptr_range()
            .contains(&cursor_start)
            .then(|| unsafe { cursor_start.offset_from(bytes.as_ptr()) } as usize)
    }

    pub fn get_literal_text_between(&self, start: &Token<'a>, end: &Token<'a>) -> Option<&'a str> {
        self.get_start_index_of(start).and_then(|s| {
            self.get_start_index_of(end)
                .and_then(|e| self.buf.get(s..e))
        })
    }

    pub fn get_literal_text_after(&self, cursor: &Token<'a>) -> Option<&'a str> {
        self.get_start_index_of(cursor).map(|s| &self.buf[s..])
    }

    pub fn current_line(&self) -> usize {
        self.line
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
            newlines: 0,
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
        LexResult {
            token,
            end,
            newlines: 0,
        }
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
                newlines: 0,
            })
    }

    fn scan_keyword(&self, start: usize) -> Option<LexResult<'a>> {
        let end = self.find_next_word_end();
        match_keyword(self.substr(start..end)).map(|token| LexResult {
            token,
            end,
            newlines: 0,
        })
    }

    fn scan_delimited<F: FnOnce(&'a str) -> TokenType<'a>>(
        &self,
        open: usize,
        close_char: char,
        factory: F,
        error: ErrorMessage,
    ) -> LexResult<'a> {
        let mut newlines = 0usize;
        if let Some((close, _)) = self
            .char_indices
            .clone()
            .inspect(|&(_, c)| {
                if c == '\n' {
                    newlines += 1
                }
            })
            .find(|&(_, c)| c == close_char)
        {
            let end = close + 1;
            let text = self.substr(open..end);
            LexResult {
                token: Token::new(factory(self.substr((open + 1)..close)), text),
                end,
                newlines,
            }
        } else {
            LexResult {
                token: Token::new(TokenType::Error(error), self.substr(open..)),
                end: self.buf.len(),
                newlines,
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

    fn scan_apostrophe_n_apostrophe(&self, start: usize) -> Option<LexResult<'a>> {
        self.substr(start..).strip_prefix("'n'").map(|_| LexResult {
            token: Token::new(TokenType::ApostropheNApostrophe, "'n'"),
            end: start + 3,
            newlines: 0,
        })
    }

    fn make_error_token(&self, start: usize, error: ErrorMessage) -> LexResult<'a> {
        let end = self.find_next_word_end();
        LexResult {
            token: Token::new(TokenType::Error(error), self.substr(start..end)),
            end,
            newlines: 0,
        }
    }

    fn advance_to(&mut self, idx: usize, start: usize) {
        assert!(idx > start);
        for _ in 0..(idx - start - 1) {
            self.char_indices.next();
        }
    }

    fn next_char(&self) -> Option<char> {
        self.char_indices.clone().next().map(|(_, c)| c)
    }

    fn char_token(&self, token_type: TokenType<'a>, start: usize) -> LexResult<'a> {
        LexResult {
            token: Token::new(token_type, self.substr(start..(start + 1))),
            end: start + 1,
            newlines: (token_type == TokenType::Newline) as usize,
        }
    }
    fn two_char_token(&self, token_type: TokenType<'a>, start: usize) -> LexResult<'a> {
        LexResult {
            token: Token::new(token_type, self.substr(start..(start + 2))),
            end: start + 2,
            newlines: 0,
        }
    }

    fn match_loop(&mut self) -> Option<<Self as Iterator>::Item> {
        loop {
            if let Some((start, start_char)) = find_word_start(&mut self.char_indices) {
                let LexResult {
                    token,
                    end,
                    newlines,
                } = match start_char {
                    '\n' => self.char_token(TokenType::Newline, start),

                    '.' => {
                        // might be a number starting with a decimal, like .123
                        if let Some(number) = self.scan_number(start) {
                            number
                        } else {
                            self.char_token(TokenType::Dot, start)
                        }
                    }
                    ',' => self.char_token(TokenType::Comma, start),
                    '&' => self.char_token(TokenType::Ampersand, start),

                    '+' => self.char_token(TokenType::Plus, start),
                    '-' => self.char_token(TokenType::Minus, start),
                    '*' => self.char_token(TokenType::Multiply, start),
                    '/' => self.char_token(TokenType::Divide, start),

                    '"' => self.scan_string_literal(start),
                    '(' => self.scan_comment(start),

                    '_' => self.make_error_token(
                        start,
                        ErrorMessage("'_' is not a valid character because it can't be sung"),
                    ),

                    '<' => self
                        .next_char()
                        .filter(|c| *c == '=')
                        .map(|_| self.two_char_token(TokenType::LessEq, start))
                        .unwrap_or_else(|| self.char_token(TokenType::Less, start)),

                    '>' => self
                        .next_char()
                        .filter(|c| *c == '=')
                        .map(|_| self.two_char_token(TokenType::GreaterEq, start))
                        .unwrap_or_else(|| self.char_token(TokenType::Greater, start)),

                    c => {
                        if let Some(result) = self.scan_apostrophe_n_apostrophe(start) {
                            result
                        } else if is_ignorable_punctuation(c) || c == '\'' {
                            continue;
                        } else {
                            let error =
                                || self.make_error_token(start, ErrorMessage("Invalid token"));
                            if c.is_numeric() {
                                self.scan_number(start).unwrap_or_else(error)
                            } else if c.is_alphabetic() {
                                self.scan_keyword(start)
                                    .unwrap_or_else(|| self.scan_word(start))
                            } else {
                                error()
                            }
                        }
                    }
                };
                self.advance_to(end, start);
                self.line += newlines;
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

#[derive(Clone, Debug)]
pub struct CommentSkippingLexer<'a> {
    lexer: Lexer<'a>,
}

impl<'a> CommentSkippingLexer<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }
    pub fn underlying(&self) -> &Lexer<'a> {
        &self.lexer
    }
}

impl<'a> Iterator for CommentSkippingLexer<'a> {
    type Item = <Lexer<'a> as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.find(|token| !token.is_comment())
    }
}