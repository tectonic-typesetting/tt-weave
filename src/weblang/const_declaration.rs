//! A declaration of a constant.
//!
//! In Pascal these happen inside `const` blocks but in typical WEB programs
//! it's easiest to treat them as toplevels.

use nom::{
    branch::alt,
    combinator::{map, opt},
    sequence::tuple,
};

use crate::prettify::{Prettifier, RenderInline};

use super::{base::*, WebToplevel};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebConstantDeclaration<'a> {
    /// The name of the constant.
    name: StringSpan<'a>,

    /// The value of the constant.
    value: PascalToken<'a>,

    /// Optional comment.
    comment: Option<WebComment<'a>>,

    /// Optional second comment, needed for XeTeX(2022.0):11.
    second_comment: Option<WebComment<'a>>,
}

pub fn parse_constant_declaration<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    map(
        tuple((
            identifier,
            pascal_token(PascalToken::Equals),
            alt((int_literal, identifier_as_token)),
            pascal_token(PascalToken::Semicolon),
            opt(comment),
            opt(comment),
        )),
        |tup| {
            WebToplevel::ConstDeclaration(WebConstantDeclaration {
                name: tup.0,
                value: tup.2,
                comment: tup.4,
                second_comment: tup.5,
            })
        },
    )(input)
}

// Prettifying

impl<'a> WebConstantDeclaration<'a> {
    pub fn prettify(&self, dest: &mut Prettifier) {
        if let Some(c) = self.comment.as_ref() {
            c.render_inline(dest);
            dest.newline_needed();
        }

        if let Some(c) = self.second_comment.as_ref() {
            c.render_inline(dest);
            dest.newline_needed();
        }

        dest.keyword("const");
        dest.space();
        dest.noscope_push(&self.name);
        dest.noscope_push(" = ");
        self.value.render_inline(dest);
        dest.noscope_push(';');
    }
}
