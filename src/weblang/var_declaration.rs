//! A declaration of a variable.
//!
//! In Pascal these happen inside `var` blocks but in typical WEB programs there
//! are also "toplevel" instances.

use nom::{
    combinator::{map, opt},
    multi::separated_list0,
    sequence::tuple,
};

use super::{
    base::*,
    webtype::{parse_type, WebType},
    WebToplevel,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebVarDeclaration<'a> {
    /// The name(s) of the constant(s).
    names: Vec<StringSpan<'a>>,

    /// The type of the constant(s).
    ty: WebType<'a>,

    /// Optional comment.
    comment: Option<WebComment<'a>>,
}

pub fn parse_var_declaration<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    map(
        tuple((
            separated_list0(pascal_token(PascalToken::Comma), identifier),
            pascal_token(PascalToken::Colon),
            parse_type,
            pascal_token(PascalToken::Semicolon),
            opt(comment),
        )),
        |tup| {
            WebToplevel::VarDeclaration(WebVarDeclaration {
                names: tup.0,
                ty: tup.2,
                comment: tup.4,
            })
        },
    )(input)
}
