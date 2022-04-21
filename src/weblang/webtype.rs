//! Types in WEB.
//!
//! I.e., Pascal types.

use nom::{branch::alt, combinator::map, multi::separated_list0, sequence::tuple};

use super::base::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebType<'a> {
    Integer,
    Real,
    Boolean,
    Range(RangeBound<'a>, RangeBound<'a>),
    PackedFileOf(StringSpan<'a>),
    Array(WebArrayType<'a>),
    UserDefined(StringSpan<'a>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RangeBound<'a> {
    Literal(PascalToken<'a>),
    Symbolic(StringSpan<'a>),
}

pub fn parse_type<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebType<'a>> {
    alt((
        named("integer", WebType::Integer),
        named("real", WebType::Real),
        named("boolean", WebType::Boolean),
        parse_packed_file_of,
        parse_array,
        parse_range,
        map(identifier, |s| WebType::UserDefined(s)),
    ))(input)
}

fn named<'a>(
    name: &'a str,
    value: WebType<'a>,
) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, WebType<'a>> + 'a {
    move |input: ParseInput<'a>| {
        let (input, sv) = identifier(input)?;

        if sv.value == name {
            Ok((input, value.clone()))
        } else {
            new_parse_err(input, WebErrorKind::ExpectedIdentifier)
        }
    }
}

fn parse_range<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebType<'a>> {
    map(
        tuple((
            parse_range_bound,
            pascal_token(PascalToken::DoubleDot),
            parse_range_bound,
        )),
        |t| WebType::Range(t.0, t.2),
    )(input)
}

fn parse_range_bound<'a>(input: ParseInput<'a>) -> ParseResult<'a, RangeBound<'a>> {
    alt((
        map(int_literal, |t| RangeBound::Literal(t)),
        map(identifier, |i| RangeBound::Symbolic(i)),
    ))(input)
}

fn parse_packed_file_of<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebType<'a>> {
    map(
        tuple((
            reserved_word(PascalReservedWord::Packed),
            reserved_word(PascalReservedWord::File),
            reserved_word(PascalReservedWord::Of),
            identifier,
        )),
        |t| WebType::PackedFileOf(t.3),
    )(input)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebArrayType<'a> {
    axes: Vec<Box<WebType<'a>>>,
    element: Box<WebType<'a>>,
}

fn parse_array<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebType<'a>> {
    map(
        tuple((
            reserved_word(PascalReservedWord::Array),
            pascal_token(PascalToken::OpenDelimiter(DelimiterKind::SquareBracket)),
            separated_list0(
                pascal_token(PascalToken::Comma),
                map(parse_type, |e| Box::new(e)),
            ),
            pascal_token(PascalToken::CloseDelimiter(DelimiterKind::SquareBracket)),
            reserved_word(PascalReservedWord::Of),
            map(parse_type, |e| Box::new(e)),
        )),
        |t| {
            WebType::Array(WebArrayType {
                axes: t.2,
                element: t.5,
            })
        },
    )(input)
}
