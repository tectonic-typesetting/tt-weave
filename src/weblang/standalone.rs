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

pub fn parse_standalone_base<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStandalone<'a>> {
    let (input, token) = alt((
        map(identifier, |s| PascalToken::Identifier(s)),
        map(any_reserved_word, |s| PascalToken::ReservedWord(s)),
        transmute_formatted,
        string_literal,
        int_literal,
    ))(input)?;

    let (input, comment) = opt(comment)(input)?;

    Ok((input, WebStandalone { token, comment }))
}

pub fn parse_standalone<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    map(parse_standalone_base, |s| WebToplevel::Standalone(s))(input)
}

fn transmute_formatted<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(PascalToken::FormattedIdentifier(s, _)) = wt {
        Ok((input, PascalToken::Identifier(s)))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedIdentifer);
    }
}
