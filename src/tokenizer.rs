use std::ops::Deref;
use std::sync::Arc;
use std::mem::take;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DynPart<L> {
    pub prefix: Arc<str>,
    pub local: L,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodeStr<L> {
    pub dyn_parts: Vec<DynPart<L>>,
    pub lit_end: Arc<str>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Dot,
    Then,
    Star,
    Plus,
    Minus,
    Equal,
    Slash,
    Comma,
    SigRet,
    Bubble,

    IsEqual,
    Inferior,
    Superior,
    Different,
    LessEqual,
    GreaterEqual,

    Tuple(Vec<TokenData<'a>>),
    Braced(Vec<TokenData<'a>>),
    Bracketed(Vec<TokenData<'a>>),

    Or,
    Semi,
    Colon,
    Question,
    Path,
    CodeStr(CodeStr<&'a str>),
    Alphanumeric(&'a str),

    Fn,
    In,
    Let,
    Use,
    Impl,
    Type,
    Const,
    Struct,

    If,
    For,
    Else,
    Loop,
    Match,
    While,
    Break,
    Return,
    Continue,
}

const SYMBOLS: &[(&str, Token)] = &[
    (".", Token::Dot),
    (",", Token::Comma),
    (";", Token::Semi),
    ("==", Token::IsEqual),
    ("!=", Token::Different),
    ("<=", Token::LessEqual),
    (">=", Token::GreaterEqual),
    ("=>", Token::Then),
    ("->", Token::SigRet),
    ("<", Token::Inferior),
    (">", Token::Superior),
    ("/", Token::Slash),
    ("+", Token::Plus),
    ("-", Token::Minus),
    ("*", Token::Star),
    ("|", Token::Or),
    ("::", Token::Path),
    (":", Token::Colon),
    ("=", Token::Equal),
    ("?", Token::Question),
];

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenData<'a> {
    pub inner: Token<'a>,
    pub line: usize,
}

impl<'a> Deref for TokenData<'a> {
    type Target = Token<'a>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a> TokenData<'a> {
    fn new(inner: Token<'a>, line: usize) -> Self {
        Self { inner, line }
    }
}

// todo remove this and fight the compiler
fn block_ctor(start: char, inner: Vec<TokenData>) -> Token {
    match start {
        '(' => Token::Tuple(inner),
        '{' => Token::Braced(inner),
        '[' => Token::Bracketed(inner),
        _ => unreachable!(),
    }
}

pub fn tokenize<'a>(text: &mut &'a str) -> Result<Vec<TokenData<'a>>, ()> {
    let mut target = Vec::new();
    let mut stack = Vec::new();
    let mut line = 1;

    let blocks = [ ('(', ')'), ('{', '}'), ('[', ']') ];

    'outer: loop {
        *text = text.trim_start_matches(['\t', ' ']);

        if let Some(next) = text.strip_prefix('\n') {
            *text = next;
            line += 1;
            continue;
        }

        if text.is_empty() {
            match stack.is_empty() {
                true => break Ok(target),
                false => break Err(()),
            };
        }

        if try_comment(text) {
            continue;
        }

        if let Some(code_str) = try_string(text)? {
            let token = Token::CodeStr(code_str);
            target.push(TokenData::new(token, line));
            continue;
        }

        for (start, stop) in blocks {
            if let Some(next) = text.strip_prefix(start) {
                stack.push((take(&mut target), stop));
                *text = next;
                continue 'outer;
            }

            if let Some(next) = text.strip_prefix(stop) {
                let (mut tokens, expected) = stack.pop().ok_or(())?;
                (expected == stop).then_some(()).ok_or(())?;

                let inner = take(&mut target);
                let token = block_ctor(start, inner);
                tokens.push(TokenData::new(token, line));

                target = tokens;
                *text = next;
                continue 'outer;
            }
        }

        if let Some(token) = try_symbol(text) {
            target.push(TokenData::new(token, line));
            continue;
        }

        if let Some(token) = try_alphanumeric(text) {
            target.push(TokenData::new(token, line));
            continue;
        }

        break Err(());
    }
}

fn try_comment(text: &mut &str) -> bool {
    if let Some(then) = text.strip_prefix("/*") {
        let Some((comment, mut next)) = then.split_once("*/") else {
            return false;
        };

        let mut extra = comment.split("/*").count() - 1;

        while extra > 0 {
            let Some((_, then)) = next.split_once("*/") else {
                return false;
            };

            next = then;
            extra -= 1;
        }

        *text = next;
        true
    } else {
        false
    }
}

fn try_symbol(text: &mut &str) -> Option<Token<'static>> {
    for (symbol, token) in SYMBOLS {
        if let Some(next) = text.strip_prefix(symbol) {
            *text = next;
            return Some(token.clone());
        }
    }

    None
}

fn try_string<'a>(text: &mut &'a str) -> Result<Option<CodeStr<&'a str>>, ()> {
    if text.starts_with('\'') {
        // literal string
        match parse_string(text, false) {
            Some(code_str) => Ok(Some(code_str)),
            None => Err(()),
        }
    } else if text.starts_with('"') {
        // format string
        match parse_string(text, true) {
            Some(code_str) => Ok(Some(code_str)),
            None => Err(()),
        }
    } else {
        Ok(None)
    }
}

fn try_alphanumeric<'a>(text: &mut &'a str) -> Option<Token<'a>> {
    let mut len = 0;
    let mut iter = text.chars();

    while let Some(c) = iter.next() {
        match c.is_alphanumeric() | (c == '_') {
            true => len += c.len_utf8(),
            false => break,
        }
    }

    if len == 0 {
        return None;
    }

    let (alphanum, next) = text.split_at(len);

    let token = match alphanum {
        "fn" => Token::Fn,
        "in" => Token::In,
        "or" => Token::Bubble,
        "use" => Token::Use,
        "let" => Token::Let,
        "impl" => Token::Impl,
        "type" => Token::Type,
        "const" => Token::Const,
        "struct" => Token::Struct,

        "if" => Token::If,
        "for" => Token::For,
        "else" => Token::Else,
        "loop" => Token::Loop,
        "match" => Token::Match,
        "while" => Token::While,
        "break" => Token::Break,
        "return" => Token::Return,
        "continue" => Token::Continue,

        other => Token::Alphanumeric(other),
    };

    *text = next;
    Some(token)
}

fn parse_string<'a>(text: &mut &'a str, format: bool) -> Option<CodeStr<&'a str>> {
    let mut iter = text.chars();
    let mut dyn_parts = Vec::new();
    let mut lit_end = String::new();
    let mut len = 0;

    let mut next = |len: &mut usize| {
        let c = iter.next()?;
        *len += c.len_utf8();
        Some(c)
    };

    let _first = next(&mut len).unwrap();

    loop {
        match next(&mut len)? {
            '\'' if !format => break,
            '\"' if format => break,
            '{' if format => {
                let start = len;
                let mut end = len;

                loop {
                    match next(&mut len)? {
                        '}' => break,
                        c => end += c.len_utf8(),
                    }
                }

                let local = &text[start..end];

                let dyn_part = DynPart {
                    prefix: lit_end.as_str().into(),
                    local,
                };

                lit_end.clear();
                dyn_parts.push(dyn_part);
            },
            '\\' => lit_end.push(next(&mut len)?),
            c => lit_end.push(c),
        };
    }

    *text = &text[len..];

    Some(CodeStr {
        dyn_parts,
        lit_end: lit_end.into(),
    })
}
