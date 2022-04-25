//! Types in WEB.
//!
//! I.e., Pascal types.

use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many1, separated_list0},
    sequence::tuple,
};

use super::base::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebType<'a> {
    Integer,
    Real,
    Boolean,
    Range(RangeBound<'a>, RangeBound<'a>),
    PackedFileOf(StringSpan<'a>),
    Array(WebArrayType<'a>),
    Record(WebRecordType<'a>),
    UserDefined(StringSpan<'a>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RangeBound<'a> {
    Literal(PascalToken<'a>),
    Symbolic1(StringSpan<'a>),
    Symbolic2(StringSpan<'a>, PascalToken<'a>, PascalToken<'a>),
}

pub fn parse_type<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebType<'a>> {
    alt((
        named("integer", WebType::Integer),
        named("real", WebType::Real),
        named("boolean", WebType::Boolean),
        parse_packed_file_of,
        parse_record,
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
        parse_binary_range_bound,
        map(identifier, |i| RangeBound::Symbolic1(i)),
    ))(input)
}

/// This is for WEB range bounds that rely on math performed on @define
/// constants by the WEB preprocessor.
fn parse_binary_range_bound<'a>(input: ParseInput<'a>) -> ParseResult<'a, RangeBound<'a>> {
    map(
        tuple((
            identifier,
            alt((
                pascal_token(PascalToken::Plus),
                pascal_token(PascalToken::Minus),
            )),
            int_literal,
        )),
        |t| RangeBound::Symbolic2(t.0, t.1, t.2),
    )(input)
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
    is_packed: bool,
    axes: Vec<Box<WebType<'a>>>,
    element: Box<WebType<'a>>,
}

fn parse_array<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebType<'a>> {
    map(
        tuple((
            opt(reserved_word(PascalReservedWord::Packed)),
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
                is_packed: t.0.is_some(),
                axes: t.3,
                element: t.6,
            })
        },
    )(input)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebRecordType<'a> {
    is_packed: bool,
    fields: Vec<WebRecordField<'a>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebRecordField<'a> {
    name: StringSpan<'a>,
    ty: Box<WebType<'a>>,
    comment: Option<Vec<TypesetComment<'a>>>,
}

fn parse_record<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebType<'a>> {
    map(
        tuple((
            opt(reserved_word(PascalReservedWord::Packed)),
            reserved_word(PascalReservedWord::Record),
            many1(parse_record_field),
            reserved_word(PascalReservedWord::End),
        )),
        |t| {
            WebType::Record(WebRecordType {
                is_packed: t.0.is_some(),
                fields: t.2,
            })
        },
    )(input)
}

fn parse_record_field<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebRecordField<'a>> {
    map(
        tuple((
            identifier,
            pascal_token(PascalToken::Colon),
            parse_type,
            pascal_token(PascalToken::Semicolon),
            opt(comment),
        )),
        |t| WebRecordField {
            name: t.0,
            ty: Box::new(t.2),
            comment: t.4,
        },
    )(input)
}
