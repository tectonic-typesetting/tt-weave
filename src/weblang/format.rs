//! A WEB `@f` format definition.
//!
//! These have the form `@f identifier == reservedword`.
//!
//! TODO: honor these!

use nom::{branch::alt, combinator::opt, sequence::tuple};
use std::borrow::Cow;

use crate::prettify::{Prettifier, RenderInline};

use super::{base::*, WebToplevel};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebFormat<'a> {
    /// The LHS of the format: an identifier.
    lhs: StringSpan<'a>,

    /// The RHS: a reserved word
    rhs: PascalReservedWord,

    /// Optional trailing comment.
    comment: Option<WebComment<'a>>,
}

pub fn parse_format<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::Format),
        identifier_or_formatted_or_reserved,
        pascal_token(PascalToken::Equivalence),
        alt((any_reserved_word, true_identifier_workaround)),
        opt(comment),
    ))(input)?;

    Ok((
        input,
        WebToplevel::Format(WebFormat {
            lhs: items.1,
            rhs: items.3.value,
            comment: items.4,
        }),
    ))
}

fn identifier_or_formatted_or_reserved<'a>(
    input: ParseInput<'a>,
) -> ParseResult<'a, StringSpan<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(PascalToken::Identifier(s)) = wt {
        Ok((input, s))
    } else if let WebToken::Pascal(PascalToken::FormattedIdentifier(s, _)) = wt {
        Ok((input, s))
    } else if let WebToken::Pascal(PascalToken::ReservedWord(sv)) = wt {
        let ss = StringSpan {
            value: Cow::Owned(sv.value.to_string()),
            start: sv.start,
            end: sv.end,
        };
        Ok((input, ss))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedIdentifier);
    }
}

/// (Xe)TeX "formats" `type` as `true` to defuse its special-ness. But `true`
/// isn't a reserved word. Since our support for "format" is hacky anyway, hack
/// some more by mapping this to `Define`, which is PascalReservedWord enum
/// variant that we've added relative to WEB.
///
/// (Xe)TeX also formats `mtype` as `type`, which is parsed as an identifier
/// due to the above. So in that case we have to hack in the other direction.
fn true_identifier_workaround<'a>(
    input: ParseInput<'a>,
) -> ParseResult<'a, SpanValue<'a, PascalReservedWord>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(PascalToken::Identifier(s)) = wt {
        if s.value == "true" {
            let rv = SpanValue {
                value: PascalReservedWord::Define,
                start: s.start,
                end: s.end,
            };

            return Ok((input, rv));
        } else if s.value == "type" {
            let rv = SpanValue {
                value: PascalReservedWord::Type,
                start: s.start,
                end: s.end,
            };

            return Ok((input, rv));
        }
    }

    new_parse_err(input, WebErrorKind::Eof)
}

// Prettification

impl<'a> WebFormat<'a> {
    pub fn prettify(&self, dest: &mut Prettifier) {
        dest.keyword("@format");
        dest.space();
        dest.noscope_push(self.lhs.value.as_ref());
        dest.noscope_push(" ~ ");
        dest.noscope_push(self.rhs);
        dest.noscope_push(";");

        if let Some(c) = self.comment.as_ref() {
            dest.space();
            c.render_inline(dest);
        }
    }
}
