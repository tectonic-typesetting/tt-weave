//! A declaration of a label.
//!
//! WEB programs use `@d` definitions to give labels symbolic names.

use nom::{combinator::opt, sequence::tuple};

use crate::prettify::{self, Prettifier};

use super::{base::*, WebToplevel};

/// A label declaration.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebLabelDeclaration<'a> {
    /// The label name.
    name: StringSpan<'a>,

    /// An optional associated comment.
    comment: Option<Vec<TypesetComment<'a>>>,
}

pub fn parse_label_declaration<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::Label),
        identifier,
        pascal_token(PascalToken::Semicolon),
        opt(comment),
    ))(input)?;

    Ok((
        input,
        WebToplevel::LabelDeclaration(WebLabelDeclaration {
            name: items.1,
            comment: items.3,
        }),
    ))
}

impl<'a> WebLabelDeclaration<'a> {
    pub fn prettify(&self, dest: &mut Prettifier) {
        let clen = self
            .comment
            .as_ref()
            .map(|c| prettify::comment_measure_inline(c))
            .unwrap_or(0);
        let slen = self.name.value.len() + 7;

        if dest.fits(clen + slen + 1) {
            dest.noscope_push("label ");
            dest.noscope_push(self.name.value.as_ref());
            dest.noscope_push(';');

            if let Some(c) = self.comment.as_ref() {
                dest.space();
                prettify::comment_render_inline(c, dest);
            }

            dest.newline_needed();
        } else {
            if let Some(c) = self.comment.as_ref() {
                prettify::comment_render_inline(c, dest);
                dest.newline_indent();
            }

            dest.noscope_push("label ");
            dest.noscope_push(self.name.value.as_ref());
            dest.noscope_push(';');
            dest.newline_needed();
        }
    }
}
