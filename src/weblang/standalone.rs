//! A "standalone" token with an optional comment.
//!
//! Allowed tokens are identifiers and literals.

use nom::{
    branch::alt,
    combinator::{map, opt},
};

use crate::prettify::{self, FormatContext, PrettifiedCode};

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
        return new_parse_err(input, WebErrorKind::ExpectedIdentifier);
    }
}

impl<'a> WebStandalone<'a> {
    pub fn measure_horz(&self) -> usize {
        self.token.measure_inline()
            + self
                .comment
                .as_ref()
                .map(|c| 1 + prettify::comment_measure_inline(c))
                .unwrap_or(0)
    }

    pub fn render_horz(&self, ctxt: &FormatContext, dest: &mut PrettifiedCode) {
        self.token.render_inline(ctxt, dest);

        if let Some(c) = self.comment.as_ref() {
            dest.space();
            prettify::comment_render_inline(c, ctxt, dest);
        }

        dest.newline(ctxt);
    }
}
