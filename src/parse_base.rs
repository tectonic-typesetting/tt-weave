//! Basic types for the parsing infrastructure.

use nom::{
    error::{ErrorKind, ParseError as NomParseError},
    Err, IResult,
};
use nom_locate::LocatedSpan;
use std::{borrow::Cow, fmt};

pub type Span<'a> = LocatedSpan<&'a str>;
pub type ParseError<'a> = (Span<'a>, ErrorKind);
pub type ParseResult<'a, T> = IResult<Span<'a>, T, ParseError<'a>>;

pub fn new_parse_error<'a, T>(s: Span<'a>, k: ErrorKind) -> ParseResult<'a, T> {
    Err(Err::Error(ParseError::from_error_kind(s, k)))
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[allow(dead_code)]
pub struct SpanValue<'a, T> {
    pub start: Span<'a>,
    pub end: Span<'a>,
    pub value: T,
}

impl<'a, T: fmt::Display> fmt::Display for SpanValue<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

pub type StringSpan<'a> = SpanValue<'a, Cow<'a, str>>;

impl<'a> StringSpan<'a> {
    pub fn len(&self) -> usize {
        self.value.as_ref().len()
    }
}
