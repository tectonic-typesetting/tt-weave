//! Pascal preprocessor directives.
//!
//! These are special constructs that would matter if we were actually compiling
//! this Pascal code. We just implement them because we have to.

use nom::{combinator::opt, multi::many0, sequence::tuple};

use crate::prettify::{self, Prettifier};

use super::base::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebPreprocessorDirective<'a> {
    /// The tokens comprising this directive
    tokens: Vec<PascalToken<'a>>,

    /// An optional associated comment
    comment: Option<Vec<TypesetComment<'a>>>,
}

pub fn parse_preprocessor_directive_base<'a>(
    input: ParseInput<'a>,
) -> ParseResult<'a, WebPreprocessorDirective<'a>> {
    let (input, mut items) = tuple((
        pascal_token(PascalToken::OpenDelimiter(DelimiterKind::MetaComment)),
        pascal_token(PascalToken::PasteText),
        many0(any_pascal_except_close_meta),
        pascal_token(PascalToken::CloseDelimiter(DelimiterKind::MetaComment)),
        opt(comment),
    ))(input)?;

    let mut tokens = vec![items.0, items.1];
    tokens.append(&mut items.2);
    tokens.push(items.3);

    let comment = items.4;

    Ok((input, WebPreprocessorDirective { tokens, comment }))
}

fn any_pascal_except_close_meta<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(ptok) = wt {
        if let PascalToken::CloseDelimiter(DelimiterKind::MetaComment) = ptok {
        } else {
            return Ok((input, ptok));
        }
    }

    return new_parse_err(input, WebErrorKind::ExpectedPascalToken);
}

impl<'a> WebPreprocessorDirective<'a> {
    pub fn prettify(&self, dest: &mut Prettifier) {
        if let Some(c) = self.comment.as_ref() {
            prettify::comment_render_inline(c, dest);
            dest.newline_needed();
        }

        dest.noscope_push("pascal_preprocessor!(");

        for t in &self.tokens {
            t.render_inline(dest);
        }

        dest.noscope_push(");");
        dest.newline_needed();
    }
}
