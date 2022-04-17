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
    pub fn is_pascal(&self) -> bool {
        if let WebToken::Pascal(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_comment(&self) -> bool {
        if let WebToken::Comment(_) = self {
            true
        } else {
            false
        }
    }

    pub fn as_pascal(&self) -> Option<&PascalToken> {
        if let WebToken::Pascal(ptok) = self {
            Some(ptok)
        } else {
            None
        }
    }

    pub fn into_pascal(self) -> PascalToken<'a> {
        if let WebToken::Pascal(ptok) = self {
            ptok
        } else {
            panic!("into_pascal() of non-Pascal WEB token");
        }
    }

    pub fn into_comment(self) -> Vec<TypesetComment<'a>> {
        if let WebToken::Comment(tc) = self {
            tc
        } else {
            panic!("into_comment() of non-comment WEB token");
        }
    }

    pub fn is_reserved_word(&self, rw: PascalReservedWord) -> bool {
        if let WebToken::Pascal(ptok) = self {
            ptok.is_reserved_word(rw)
        } else {
            false
        }
    }

    pub fn is_pascal_token(&self, t: PascalToken) -> bool {
        if let WebToken::Pascal(inner) = self {
            inner == &t
        } else {
            false
        }
    }
}

/// A block of WEB syntax: just a sequence of WEB tokens.
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
    /// A `@d` definition.
    Define(WebDefine<'a>),

    /// A single Pascal token.
    Standalone(WebStandalone<'a>),
}

impl<'a> From<WebDefine<'a>> for WebToplevel<'a> {
    fn from(v: WebDefine<'a>) -> Self {
        WebToplevel::Define(v)
    }
}

impl<'a> From<WebStandalone<'a>> for WebToplevel<'a> {
    fn from(v: WebStandalone<'a>) -> Self {
        WebToplevel::Standalone(v)
    }
}

/// A block of WEB code: a sequence of parsed-out WEB toplevels
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebCode<'a>(pub Vec<WebToplevel<'a>>);

impl<'a> WebCode<'a> {
    pub fn parse(syntax: WebSyntax<'a>) -> Option<WebCode<'a>> {
        // TODO: sequence!!!
        let tl = parse_toplevel_entire(syntax.0)?;
        Some(WebCode(vec![tl]))
    }
}

/// Parse out a WEB top-level construct, assuming that *all* of the tokens
/// in the input should go into the result.
pub fn parse_toplevel_entire<'a>(toks: Vec<WebToken<'a>>) -> Option<WebToplevel<'a>> {
    if toks[0].is_reserved_word(PascalReservedWord::Define) {
        return parse_definition(toks).map(|t| t.into());
    }

    if let Some(t) = parse_standalone(toks) {
        return Some(t.into());
    }

    None
}

/// A `@d` definition
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebDefine<'a> {
    /// The LHS of the define. This may be a sequence of tokens like `blah(#)`.
    lhs: Vec<PascalToken<'a>>,

    /// The RHS. This could be anything, including partial bits of syntax.
    rhs: Box<WebToplevel<'a>>,
}

/// Parse out a top-level `@define`:
///
/// `@d <toks...> equiv <anything>`
///
/// toks[0] is the `@d` token here.
fn parse_definition<'a>(mut toks: Vec<WebToken<'a>>) -> Option<WebDefine<'a>> {
    let mut rest = toks.split_off(1);

    // If we were compiling in any real way, we'd make sure the LHS made
    // syntactic sense, but we're not.

    let mut sep_idx = None;

    for (idx, tok) in rest.iter().enumerate() {
        if !tok.is_pascal() {
            return None;
        }

        if tok.is_pascal_token(PascalToken::Equivalence) || tok.is_pascal_token(PascalToken::Equals)
        {
            sep_idx = Some(idx);
            break;
        }
    }

    let sep_idx = sep_idx?;

    if sep_idx >= rest.len() - 1 {
        return None;
    }

    let rhs = rest.split_off(sep_idx + 1);
    let lhs: Vec<_> = rest.drain(..sep_idx).map(|wt| wt.into_pascal()).collect();

    let rhs = parse_toplevel_entire(rhs)?;

    Some(WebDefine {
        lhs,
        rhs: Box::new(rhs),
    })
}

/// A "standalone" token with an optional comment.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebStandalone<'a> {
    /// The token.
    token: PascalToken<'a>,

    /// The comment.
    comment: Option<Vec<TypesetComment<'a>>>,
}

/// Parse out a top-level single standalone token.
///
/// Does not mutate *toks* if the tok-string doesn't match.
fn parse_standalone<'a>(mut toks: Vec<WebToken<'a>>) -> Option<WebStandalone<'a>> {
    if toks.len() > 2 {
        return None;
    }

    if let Some(ptok) = toks[0].as_pascal() {
        match ptok {
            PascalToken::StringLiteral(..)
            | PascalToken::IntLiteral(..)
            | PascalToken::Identifier(..) => {}
            _ => {
                return None;
            }
        }
    }

    let comment = if toks.len() < 2 {
        None
    } else {
        if !toks[1].is_comment() {
            return None;
        }

        Some(toks.pop().unwrap().into_comment())
    };

    Some(WebStandalone {
        token: toks.pop().unwrap().into_pascal(),
        comment,
    })
}
