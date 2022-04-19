//! A WEB `@f` format definition.
//!
//! These have the form `@f identifier == reservedword`.
//!
//! TODO: honor these!

use nom::sequence::tuple;

use super::{base::*, WebToplevel};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebFormat<'a> {
    /// The LHS of the format: an identifier.
    lhs: StringSpan<'a>,

    /// The RHS: a reserved word
    rhs: PascalReservedWord,
}

pub fn parse_format<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::Format),
        identifier,
        pascal_token(PascalToken::Equivalence),
        any_reserved_word,
    ))(input)?;

    Ok((
        input,
        WebToplevel::Format(WebFormat {
            lhs: items.1,
            rhs: items.3.value,
        }),
    ))
}
