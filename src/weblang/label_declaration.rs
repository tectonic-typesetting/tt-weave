//! A declaration of a label.
//!
//! WEB programs use `@d` definitions to give labels symbolic names.

use nom::{combinator::opt, sequence::tuple};

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
