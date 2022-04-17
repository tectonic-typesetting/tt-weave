//! Higher-level WEB language processing.
//!
//! This is *mostly* Pascal, but with a few additions.

use crate::{parse_base::StringSpan, pascal_token::PascalToken, reserved::PascalReservedWord};

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

#[derive(Debug)]
pub struct WebCode<'a>(pub Vec<WebToken<'a>>);
