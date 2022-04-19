//! A "standalone" token with an optional comment.
//!
//! Allowed tokens are identifiers and literals.

use nom::{
    branch::alt,
    combinator::{map, opt},
};

use super::{base::*, WebToplevel};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebStandalone<'a> {
    /// The token.
    token: PascalToken<'a>,

    /// An optional associated comment.
    comment: Option<Vec<TypesetComment<'a>>>,
}

pub fn parse_standalone<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, token) = alt((
        map(identifier, |s| PascalToken::Identifier(s)),
        string_literal,
        int_literal,
    ))(input)?;

    let (input, comment) = opt(comment)(input)?;

    Ok((
        input,
        WebToplevel::Standalone(WebStandalone { token, comment }),
    ))
}
