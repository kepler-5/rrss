use std::{collections::HashMap, iter::repeat, slice::SliceIndex, str::CharIndices};

use derive_more::{Constructor, IsVariant};
use lazy_static::lazy_static;

use crate::frontend::source_range::SourceLocation;

use super::source_range::SourceRange;

#[cfg(test)]
mod tests;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ErrorMessage(&'static str);

#[derive(Clone, Copy, Debug, IsVariant, PartialEq)]
pub enum TokenType<'a> {
    Word,
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

#[derive(Clone, Constructor, Debug, PartialEq)]
pub struct Token<'a> {
    pub id: TokenType<'a>,
    pub spelling: &'a str,
    pub range: SourceRange,
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

fn match_keyword<'a>(word: &'a str) -> Option<TokenType<'a>> {
    KEYWORDS.get(word.to_lowercase().as_str()).copied()
}

fn is_ignorable_whitespace(c: char) -> bool {
    c.is_whitespace() && c != '\n'
}

fn is_ignorable_punctuation(c: char) -> bool {
    c.is_ascii_punctuation() && c != '_' && c != '\''
}

pub fn is_word(text: &str) -> bool {
    !text.is_empty()
        && text
            .chars()
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
    newlines: u32,
    new_line_start: Option<u32>,
}

impl<'a> LexResult<'a> {
    fn extended_to(mut self, other: &LexResult) -> Self {
        self.end = self.end.max(other.end);
        self.newlines = self.newlines + other.newlines;
        self.new_line_start = self.new_line_start.max(other.new_line_start);
        self
    }
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    buf: &'a str,
    char_indices: CharIndices<'a>,
    staged: Option<Token<'a>>,
    line: u32,
    line_start: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &'a str) -> Self {
        Self {
            buf,
            char_indices: buf.char_indices(),
            staged: None,
            line: 1,
            line_start: 0,
        }
    }

    pub fn skip_comments(self) -> CommentSkippingLexer<'a> {
        CommentSkippingLexer::new(self)
    }

    pub fn get_start_index_of(&self, cursor: &Token<'a>) -> Option<usize> {
        self.get_index_of(cursor.spelling.as_ptr())
    }

    fn get_index_of(&self, cursor: *const u8) -> Option<usize> {
        let bytes = self.buf.as_bytes();
        let range = bytes.as_ptr_range();
        (range.contains(&cursor) || range.end == cursor)
            .then(|| unsafe { cursor.offset_from(bytes.as_ptr()) } as usize)
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

    pub fn current_line(&self) -> u32 {
        self.line
    }

    pub fn current_loc(&self) -> SourceLocation {
        SourceLocation::new(self.line, self.current_idx() as u32 - self.line_start)
    }

    pub fn current_idx(&self) -> usize {
        self.staged
            .as_ref()
            .map(|tok| self.get_start_index_of(tok).unwrap())
            .or_else(|| self.char_indices.clone().next().map(|(i, _)| i))
            .unwrap_or_else(|| self.buf.len())
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
            .map_or(self.buf.len(), |t| t.0)
    }

    fn make_loc_from(line: u32, line_start: u32, offset: usize) -> SourceLocation {
        let from_line_start = u32::try_from(offset)
            .ok()
            .and_then(|x| x.checked_sub(line_start))
            .unwrap();
        (line, from_line_start).into()
    }

    fn make_loc(&self, offset: usize) -> SourceLocation {
        Self::make_loc_from(self.line, self.line_start, offset)
    }

    fn make_range(&self, start: usize, end: usize) -> SourceRange {
        self.make_loc(start).to(self.make_loc(end))
    }

    fn scan_number(&mut self, start: usize) -> Option<LexResult<'a>> {
        let end = self.find_next_index(|c| !(c.is_ascii_alphanumeric() || c == '.'));
        let text = self.substr(start..end);
        text.parse::<f64>().ok().map(|n| {
            self.maybe_followed_by_apostrophe_suffix(LexResult {
                token: Token::new(TokenType::Number(n), text, self.make_range(start, end)),
                end,
                newlines: 0,
                new_line_start: None,
            })
        })
    }

    fn find_next_word_end(&self) -> usize {
        self.find_next_index(|c| c.is_whitespace() || is_ignorable_punctuation(c))
    }

    fn find_word_type(&self, word: &'a str) -> TokenType<'a> {
        debug_assert!(!word.is_empty());
        match_keyword(word).unwrap_or(TokenType::Word)
    }

    fn last_n(text: &str, n: usize) -> &str {
        debug_assert!(text.len() >= n);
        &text[(text.len() - n)..]
    }

    fn make_token_from(&self, text: &'a str, id: TokenType<'a>) -> Token<'a> {
        let start = self.get_index_of(text.as_ptr());
        let end = self.get_index_of(unsafe { text.as_ptr().offset(text.len() as isize) });
        Token::new(id, text, self.make_range(start.unwrap(), end.unwrap()))
    }

    fn tokenize_word(&mut self, word: &'a str, end: usize) -> LexResult<'a> {
        let (stripped, staged_type) = word
            .strip_suffix("'s")
            .or_else(|| word.strip_suffix("'S"))
            .map(|stripped| (stripped, Some((TokenType::ApostropheS, 2))))
            .or_else(|| {
                word.strip_suffix("'re")
                    .or_else(|| word.strip_suffix("'RE"))
                    .or_else(|| word.strip_suffix("'Re"))
                    .or_else(|| word.strip_suffix("'rE"))
                    .map(|stripped| (stripped, Some((TokenType::ApostropheRE, 3))))
            })
            .unwrap_or_else(|| (word.trim_end_matches('\''), None));
        self.staged =
            staged_type.map(|(id, len)| self.make_token_from(Self::last_n(word, len), id));
        let token = self.make_token_from(stripped, self.find_word_type(stripped));
        LexResult {
            token,
            end,
            newlines: 0,
            new_line_start: None,
        }
    }

    fn scan_word(&mut self, start: usize) -> LexResult<'a> {
        let end = self.find_next_word_end();
        let text = self.substr(start..end);
        Some(text)
            .filter(|s| s.chars().all(|c| c.is_alphabetic() || c == '\''))
            .map(|word| self.tokenize_word(word, end))
            .unwrap_or_else(|| LexResult {
                token: self.make_token_from(
                    text,
                    TokenType::Error(ErrorMessage(
                        "Identifier may not contain non-alphabetic characters",
                    )),
                ),
                end,
                newlines: 0,
                new_line_start: None,
            })
    }

    fn scan_keyword(&self, start: usize) -> Option<LexResult<'a>> {
        let end = self.find_next_word_end();
        let text = self.substr(start..end);
        match_keyword(text).map(|id| LexResult {
            token: Token::new(id, text, self.make_range(start, end)),
            end,
            newlines: 0,
            new_line_start: None,
        })
    }

    fn maybe_followed_by_apostrophe_suffix(&mut self, result: LexResult<'a>) -> LexResult<'a> {
        if let Some(suffix) = self.scan_apostrophe_suffix(result.end) {
            let result = result.extended_to(&suffix);
            self.staged = Some(suffix.token);
            result
        } else {
            result
        }
    }

    fn scan_delimited<F: FnOnce(&'a str) -> TokenType<'a>>(
        &mut self,
        open: usize,
        close_char: char,
        factory: F,
        error: ErrorMessage,
    ) -> LexResult<'a> {
        let mut newlines = 0u32;
        let mut new_line_start = None;
        let start_loc = self.make_loc(open);
        let (token_type, text, end) = if let Some(close) = self
            .char_indices
            .clone()
            .inspect(|&(i, c)| {
                if c == '\n' {
                    newlines += 1;
                    new_line_start = Some((i + 1) as u32);
                }
            })
            .find(|&(_, c)| c == close_char)
            .map(|(i, _)| i)
        {
            let token_type = factory(self.substr((open + 1)..close));
            let end = close + 1;
            let text = self.substr(open..end);
            (token_type, text, end)
        } else {
            (TokenType::Error(error), self.substr(open..), self.buf.len())
        };
        let current_line = self.line + newlines;
        let current_line_start = new_line_start.unwrap_or(self.line_start);
        let token = Token::new(
            token_type,
            text,
            start_loc.to(Self::make_loc_from(current_line, current_line_start, end)),
        );
        self.maybe_followed_by_apostrophe_suffix(LexResult {
            token,
            end,
            newlines,
            new_line_start,
        })
    }

    fn scan_comment(&mut self, open: usize) -> LexResult<'a> {
        self.scan_delimited(
            open,
            ')',
            |s| TokenType::Comment(s),
            ErrorMessage("Unterminated comment"),
        )
    }

    fn scan_string_literal(&mut self, open: usize) -> LexResult<'a> {
        self.scan_delimited(
            open,
            '"',
            |s| TokenType::StringLiteral(s),
            ErrorMessage("Unterminated string literal"),
        )
    }

    fn scan_for_text(
        &self,
        start: usize,
        text: &str,
        token_type: TokenType<'a>,
    ) -> Option<LexResult<'a>> {
        debug_assert!(!text.contains('\n'));
        let buf_text = self.substr(start..);
        buf_text.strip_prefix(text).map(|_| LexResult {
            token: self
                .make_token_from(unsafe { buf_text.get_unchecked(..text.len()) }, token_type),
            end: start + text.len(),
            newlines: 0,
            new_line_start: None,
        })
    }

    fn scan_apostrophe_n_apostrophe(&self, start: usize) -> Option<LexResult<'a>> {
        self.scan_for_text(start, "'n'", TokenType::ApostropheNApostrophe)
    }

    fn scan_apostrophe_suffix(&self, start: usize) -> Option<LexResult<'a>> {
        self.scan_for_text(start, "'s", TokenType::ApostropheS)
            .or_else(|| self.scan_for_text(start, "'re", TokenType::ApostropheRE))
    }

    fn make_error_token(&self, start: usize, error: ErrorMessage) -> LexResult<'a> {
        let end = self.find_next_word_end();
        LexResult {
            token: self.make_token_from(self.substr(start..end), TokenType::Error(error)),
            end,
            newlines: 0,
            new_line_start: None,
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
        let token = self.make_token_from(self.substr(start..(start + 1)), token_type);
        let end = start + 1;
        match token_type {
            TokenType::Newline => LexResult {
                token,
                end,
                newlines: 1,
                new_line_start: Some(end as u32),
            },
            _ => LexResult {
                token,
                end,
                newlines: 0,
                new_line_start: None,
            },
        }
    }
    fn two_char_token(&self, token_type: TokenType<'a>, start: usize) -> LexResult<'a> {
        LexResult {
            token: self.make_token_from(self.substr(start..(start + 2)), token_type),
            end: start + 2,
            newlines: 0,
            new_line_start: None,
        }
    }

    fn match_loop(&mut self) -> Option<<Self as Iterator>::Item> {
        loop {
            if let Some((start, start_char)) = find_word_start(&mut self.char_indices) {
                let LexResult {
                    token,
                    end,
                    newlines,
                    new_line_start,
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
                            if c.is_numeric() {
                                self.scan_number(start).unwrap_or_else(|| {
                                    self.make_error_token(start, ErrorMessage("Invalid token"))
                                })
                            } else if c.is_alphabetic() {
                                self.scan_keyword(start)
                                    .unwrap_or_else(|| self.scan_word(start))
                            } else {
                                self.make_error_token(start, ErrorMessage("Invalid token"))
                            }
                        }
                    }
                };
                self.advance_to(end, start);
                self.line += newlines;
                if let Some(new) = new_line_start {
                    self.line_start = new;
                }
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
