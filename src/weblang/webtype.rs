//! Types in WEB.
//!
//! I.e., Pascal types.

use nom::{branch::alt, combinator::map, sequence::tuple};

use super::base::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebType<'a> {
    Integer,
    Real,
    Boolean,
    Range(RangeBound<'a>, RangeBound<'a>),
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
        parse_range,
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
