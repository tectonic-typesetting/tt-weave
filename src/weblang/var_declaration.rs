//! A declaration of a variable.
//!
//! In Pascal these happen inside `var` blocks but in typical WEB programs there
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
pub struct WebVarDeclaration<'a> {
    /// The name of the constant.
    name: StringSpan<'a>,

    /// The type of the constant.
    ty: WebType<'a>,

    /// Optional comment.
    comment: Option<Vec<TypesetComment<'a>>>,
}

pub fn parse_var_declaration<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    map(
        tuple((
            identifier,
            pascal_token(PascalToken::Colon),
            parse_type,
            pascal_token(PascalToken::Semicolon),
            opt(comment),
        )),
        |tup| {
            WebToplevel::VarDeclaration(WebVarDeclaration {
                name: tup.0,
                ty: tup.2,
                comment: tup.4,
            })
        },
    )(input)
}
