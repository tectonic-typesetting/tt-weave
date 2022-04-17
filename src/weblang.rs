//! Higher-level WEB language processing.
//!
//! This is *mostly* Pascal, but with a few additions.

use crate::{parse_base::StringSpan, pascal_token::PascalToken, reserved::PascalReservedWord};

/// Information about a typeset comment.
///
/// This type is lame. The structure is an interleaving of TeX code and inline
/// Pascal text, but our data structure doesn't capture that very effectively.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypesetComment<'a> {
    Pascal(Vec<PascalToken<'a>>),
    Tex(String),
}

/// A logical token of the WEB language, which we treat as a superset of Pascal.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebToken<'a> {
    /// A basic Pascal token
    Pascal(PascalToken<'a>),

    /// A typeset comment, which contains alternating bits of TeX code and Pascal
    /// token sequences.
    Comment(Vec<TypesetComment<'a>>),

    /// A reference to a WEB module.
    ModuleReference(StringSpan<'a>),
}

impl<'a> WebToken<'a> {
    pub fn as_pascal(&self) -> Option<&PascalToken> {
        if let WebToken::Pascal(ptok) = self {
            Some(ptok)
        } else {
            None
        }
    }

    pub fn is_reserved_word(&self, rw: PascalReservedWord) -> bool {
        if let WebToken::Pascal(ptok) = self {
            ptok.is_reserved_word(rw)
        } else {
            false
        }
    }
}

/// A block of WEB code: just a sequence of WEB tokens.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebSyntax<'a>(pub Vec<WebToken<'a>>);

/// A top-level WEB production.
///
/// Because we're not actually compiling the WEB language in any meaningful we,
/// we're pretty sloppy with the definition of "toplevel" here. It's anything
/// that can show up as a top-level item in WEB code, including `@define` and
/// `@format` statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebToplevel<'a> {
    Define(WebDefine<'a>),
}

/// A `@d` definition
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebDefine<'a> {
    /// The LHS of the define. This may be a sequence of tokens like `blah(#)`.
    lhs: Vec<PascalToken<'a>>,

    /// The RHS. This could be anything, including partial bits of syntax.
    rhs: Box<WebToplevel<'a>>,
}
