//! A "standalone" bit of syntax.
//!
//! Allowed tokens are reserved words and formatted identifiers, which act as
//! reserved words. These shouldn't come up in actual code, but are needed for
//! inline TeX discussion of the code.

use nom::{branch::alt, combinator::map};

use crate::prettify::{Prettifier, RenderInline};

use super::{base::*, WebToplevel};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebStandalone<'a> {
    /// The token.
    token: PascalToken<'a>,
}

pub fn parse_standalone_base<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStandalone<'a>> {
    alt((
        map(any_reserved_word, |s| WebStandalone {
            token: PascalToken::ReservedWord(s),
        }),
        any_formatted_identifier,
    ))(input)
}

pub fn parse_standalone<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    map(parse_standalone_base, |s| WebToplevel::Standalone(s))(input)
}

fn any_formatted_identifier<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStandalone<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(token) = wt {
        if let PascalToken::FormattedIdentifier(..) = token {
            return Ok((input, WebStandalone { token }));
        }
    }

    return new_parse_err(input, WebErrorKind::ExpectedIdentifier);
}

impl<'a> RenderInline for WebStandalone<'a> {
    fn measure_inline(&self) -> usize {
        self.token.measure_inline()
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        self.token.render_inline(dest);
    }
}
