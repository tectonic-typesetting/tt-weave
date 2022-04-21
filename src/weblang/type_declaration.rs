//! A declaration of a type.
//!
//! In Pascal these happen inside `type` blocks but in typical WEB programs there
//! are also "toplevel" instances.

use nom::{
    combinator::{map, opt},
    sequence::tuple,
};

use super::{
    base::*,
    webtype::{parse_type, WebType},
    WebToplevel,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebTypeDeclaration<'a> {
    /// The name of the new type.
    name: StringSpan<'a>,

    /// Its equivalent.
    ty: WebType<'a>,

    /// Optional comment.
    comment: Option<Vec<TypesetComment<'a>>>,
}

pub fn parse_type_declaration<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    map(
        tuple((
            identifier,
            pascal_token(PascalToken::Equals),
            parse_type,
            pascal_token(PascalToken::Semicolon),
            opt(comment),
        )),
        |tup| {
            WebToplevel::TypeDeclaration(WebTypeDeclaration {
                name: tup.0,
                ty: tup.2,
                comment: tup.4,
            })
        },
    )(input)
}
