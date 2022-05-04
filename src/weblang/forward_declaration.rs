//! A forward declaration of a procedure.

use nom::{
    branch::alt,
    combinator::{map, opt},
    sequence::tuple,
};

use crate::prettify::{Prettifier, RenderInline};

use super::{base::*, WebToplevel};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebForwardDeclaration<'a> {
    /// The name(s) of the function or procedure.
    name: StringSpan<'a>,

    /// Optional associated comment.
    comment: Option<WebComment<'a>>,
}

pub fn parse_forward_declaration_base<'a>(
    input: ParseInput<'a>,
) -> ParseResult<'a, WebForwardDeclaration<'a>> {
    let (input, items) = tuple((
        alt((
            reserved_word(PascalReservedWord::Function),
            reserved_word(PascalReservedWord::Procedure),
        )),
        identifier,
        pascal_token(PascalToken::Semicolon),
        identifier,
        pascal_token(PascalToken::Semicolon),
        opt(comment),
    ))(input)?;

    if items.3.value == "forward" {
        Ok((
            input,
            WebForwardDeclaration {
                name: items.1,
                comment: items.5,
            },
        ))
    } else {
        new_parse_err(input, WebErrorKind::ExpectedIdentifier)
    }
}

pub fn parse_forward_declaration<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    map(parse_forward_declaration_base, |fd| {
        WebToplevel::ForwardDeclaration(fd)
    })(input)
}

// Prettifying

impl<'a> WebForwardDeclaration<'a> {
    pub fn prettify(&self, dest: &mut Prettifier) {
        if let Some(c) = self.comment.as_ref() {
            c.render_inline(dest);
            dest.newline_needed();
        }

        dest.keyword("forward_declaration");
        dest.space();
        dest.noscope_push(&self.name);
        dest.noscope_push("();");
    }
}
