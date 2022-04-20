//! Base parsing tools for WEB language processing.
//!
//! Other WEB parsing modules do asterisk imports of this module.

use nom::{
    error::{ErrorKind, ParseError as NomParseError},
    Err, IResult, InputIter, InputLength, InputTake, Needed, Slice, UnspecializedInput,
};
use std::{
    iter::{Cloned, Enumerate},
    slice::Iter,
};

// Some utility imports for asterisk importers.
pub use crate::{
    parse_base::{SpanValue, StringSpan},
    pascal_token::{DelimiterKind, PascalToken},
    reserved::PascalReservedWord,
};

/// Information about a typeset comment.
///
/// This type is lame. The structure is an interleaving of TeX code and inline
/// Pascal text, but our data structure doesn't capture that very effectively.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypesetComment<'a> {
    Pascal(Vec<PascalToken<'a>>),
    Tex(String),
}

/// A logical token of the WEB language, which we treat as a superset of Pascal.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebToken<'a> {
    /// A basic Pascal token.
    Pascal(PascalToken<'a>),

    /// A typeset comment, which contains alternating bits of TeX code and Pascal
    /// token sequences.
    Comment(Vec<TypesetComment<'a>>),

    /// A reference to a WEB module.
    ModuleReference(StringSpan<'a>),
}

impl<'a> WebToken<'a> {
    pub fn as_pascal(&self) -> Option<&PascalToken> {
        if let WebToken::Pascal(ptok) = self {
            Some(ptok)
        } else {
            None
        }
    }

    pub fn into_pascal(self) -> PascalToken<'a> {
        if let WebToken::Pascal(ptok) = self {
            ptok
        } else {
            panic!("into_pascal() of non-Pascal WEB token");
        }
    }
}

/// A block of WEB syntax: just a sequence of WEB tokens.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebSyntax<'a>(pub Vec<WebToken<'a>>);

/// The parse input: a slice of tokens
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ParseInput<'a>(pub &'a [WebToken<'a>]);

impl<'a> InputLength for ParseInput<'a> {
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

/// This is a monkey-see-monkey-do impl based on how nom does things for slices.
/// The main difference is that unfortunately we have to clone instead of
/// copying.
impl<'a> InputIter for ParseInput<'a> {
    type Item = WebToken<'a>;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = Cloned<Iter<'a, WebToken<'a>>>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.iter_elements().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.0.iter().cloned()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(|b| predicate(b.clone()))
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.0.len() >= count {
            Ok(count)
        } else {
            Err(Needed::new(count - self.0.len()))
        }
    }
}

impl<'a, R> Slice<R> for ParseInput<'a>
where
    &'a [WebToken<'a>]: Slice<R>,
{
    fn slice(&self, range: R) -> Self {
        ParseInput(self.0.slice(range))
    }
}

impl<'a> InputTake for ParseInput<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        ParseInput(&self.0[0..count])
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (ParseInput(suffix), ParseInput(prefix))
    }
}

/// Implementing this gives is InputTakeAtPosition and Compare
impl<'a> UnspecializedInput for ParseInput<'a> {}

/// Our parse error kinds, including a lame catch-all for Nom's built-in ones.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum WebErrorKind {
    Eof,
    ExpectedPascalToken,
    ExpectedIdentifer,
    ExpectedStringLiteral,
    ExpectedIntLiteral,
    ExpectedComment,
    ExpectedToplevel,
    ExpectedReservedWord(PascalReservedWord),
    ExpectedAnyReservedWord,
    ExpectedOpenDelimiter(DelimiterKind),
    ExpectedCloseDelimiter(DelimiterKind),
    Nom(ErrorKind),
}

/// The parse error type.
pub type ParseError<'a> = (ParseInput<'a>, WebErrorKind);

impl<'a> NomParseError<ParseInput<'a>> for ParseError<'a> {
    fn from_error_kind(input: ParseInput<'a>, kind: ErrorKind) -> Self {
        (input, WebErrorKind::Nom(kind))
    }

    fn append(_: ParseInput<'a>, _: ErrorKind, other: Self) -> Self {
        other
    }
}

/// The parse result type.
pub type ParseResult<'a, T> = IResult<ParseInput<'a>, T, ParseError<'a>>;

pub fn new_parse_err<'a, T>(s: ParseInput<'a>, k: WebErrorKind) -> ParseResult<'a, T> {
    Err(Err::Error((s, k)))
}

/// Match and consume the next WEB token.
pub fn next_token<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToken<'a>> {
    let wt = input
        .iter_elements()
        .next()
        .ok_or_else(|| Err::Error((input, WebErrorKind::Eof)))?;
    Ok((input.slice(1..), wt))
}

/// Expect a specific Pascal token.
///
/// This matches use an equality test, so it's probably only what you want if
/// the token variant you're testing for is a content-less one.
pub fn pascal_token<'a>(
    expected: PascalToken<'a>,
) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    move |input: ParseInput<'a>| {
        let (input, wt) = next_token(input)?;

        if let WebToken::Pascal(found) = wt {
            if found == expected {
                return Ok((input, found));
            }
        }

        return new_parse_err(input, WebErrorKind::ExpectedPascalToken);
    }
}

/// Expect a Pascal identifier, returning its text.
///
/// Note that this will not match "formatted identifiers" that have had their
/// behavior overridden with an `@f` command.
pub fn identifier<'a>(input: ParseInput<'a>) -> ParseResult<'a, StringSpan<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(PascalToken::Identifier(s)) = wt {
        Ok((input, s))
    } else if let WebToken::Pascal(PascalToken::Hash(p)) = wt {
        // For our purposes, hash marks act like identifiers
        Ok((
            input,
            StringSpan {
                start: p.clone(),
                end: p.clone(),
                value: "#".into(),
            },
        ))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedIdentifer);
    }
}

/// Expect a Pascal reserved word, returning its span-value.
pub fn reserved_word<'a>(
    rw: PascalReservedWord,
) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, SpanValue<'a, PascalReservedWord>> {
    move |input: ParseInput<'a>| {
        let (input, wt) = next_token(input)?;

        if let WebToken::Pascal(PascalToken::ReservedWord(sv)) = wt {
            if sv.value == rw {
                return Ok((input, sv));
            }
        }

        return new_parse_err(input, WebErrorKind::ExpectedReservedWord(rw));
    }
}

/// Accept any Pascal reserved word.
pub fn any_reserved_word<'a>(
    input: ParseInput<'a>,
) -> ParseResult<'a, SpanValue<'a, PascalReservedWord>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(PascalToken::ReservedWord(sv)) = wt {
        return Ok((input, sv));
    }

    return new_parse_err(input, WebErrorKind::ExpectedAnyReservedWord);
}

/// Expect a Pascal string literal token, returning it.
pub fn string_literal<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(lit @ PascalToken::StringLiteral(..)) = wt {
        Ok((input, lit))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedStringLiteral);
    }
}

/// Expect a Pascal integer literal token, returning it.
pub fn int_literal<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(lit @ PascalToken::IntLiteral(..)) = wt {
        Ok((input, lit))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedIntLiteral);
    }
}

/// An open delimiter.
pub fn open_delimiter<'a>(kind: DelimiterKind) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, ()> {
    move |input: ParseInput<'a>| {
        let (input, wt) = next_token(input)?;

        if let WebToken::Pascal(PascalToken::OpenDelimiter(found_kind)) = wt {
            if found_kind == kind {
                return Ok((input, ()));
            }
        }

        return new_parse_err(input, WebErrorKind::ExpectedOpenDelimiter(kind));
    }
}

/// A close delimiter.
pub fn close_delimiter<'a>(kind: DelimiterKind) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, ()> {
    move |input: ParseInput<'a>| {
        let (input, wt) = next_token(input)?;

        if let WebToken::Pascal(PascalToken::CloseDelimiter(found_kind)) = wt {
            if found_kind == kind {
                return Ok((input, ()));
            }
        }

        return new_parse_err(input, WebErrorKind::ExpectedCloseDelimiter(kind));
    }
}

/// Expect a comment, returning it.
pub fn comment<'a>(input: ParseInput<'a>) -> ParseResult<'a, Vec<TypesetComment<'a>>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Comment(c) = wt {
        Ok((input, c))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedComment);
    }
}

/// Expect a module reference, returning its value.
pub fn module_reference<'a>(input: ParseInput<'a>) -> ParseResult<'a, StringSpan<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::ModuleReference(s) = wt {
        Ok((input, s))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedIdentifer);
    }
}
