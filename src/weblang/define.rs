//! A WEB `@d` definition.
//!
//! This has the general form `@d LHS == RHS`. The LHS might not be simple
//! identifier if it has macro parameter, and the RHS can be any toplevel.

use nom::{branch::alt, bytes::complete::take_while1, sequence::tuple};

use super::{base::*, parse_toplevel, WebToplevel};

/// A `@d` definition
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebDefine<'a> {
    /// The LHS of the define. This may be a sequence of tokens like `blah(#)`.
    lhs: Vec<PascalToken<'a>>,

    /// The RHS. This could be anything, including partial bits of syntax.
    rhs: Box<WebToplevel<'a>>,
}

pub fn parse_define<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    fn is_define_lhs_token(t: WebToken) -> bool {
        if let Some(pt) = t.as_pascal() {
            match pt {
                PascalToken::Equals | PascalToken::Equivalence => false,
                _ => true,
            }
        } else {
            false
        }
    }

    let (input, items) = tuple((
        reserved_word(PascalReservedWord::Define),
        take_while1(is_define_lhs_token),
        alt((
            pascal_token(PascalToken::Equivalence),
            pascal_token(PascalToken::Equals),
        )),
        parse_toplevel,
    ))(input)?;

    Ok((
        input,
        WebToplevel::Define(WebDefine {
            lhs: items.1 .0.iter().map(|t| t.clone().into_pascal()).collect(),
            rhs: Box::new(items.3),
        }),
    ))
}
