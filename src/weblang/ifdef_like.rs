//! WEB `#ifdef`-like constructs
//!
//! These are needed for `@define debug = { { blah blah }` type definitions.

use nom::{branch::alt, combinator::opt, sequence::tuple};

use super::{base::*, WebToplevel};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebIfdefLike<'a> {
    /// Whether this is an "open" meta-comment or not (i.e., close).
    is_open: bool,

    /// An optional associated comment
    comment: Option<Vec<TypesetComment<'a>>>,
}

pub fn parse_ifdef_like<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, items) = tuple((
        alt((
            pascal_token(PascalToken::OpenDelimiter(DelimiterKind::MetaComment)),
            pascal_token(PascalToken::CloseDelimiter(DelimiterKind::MetaComment)),
        )),
        opt(comment),
    ))(input)?;

    let is_open = if let PascalToken::OpenDelimiter(DelimiterKind::MetaComment) = items.0 {
        true
    } else {
        false
    };

    let comment = items.1;

    Ok((
        input,
        WebToplevel::IfdefLike(WebIfdefLike { is_open, comment }),
    ))
}
