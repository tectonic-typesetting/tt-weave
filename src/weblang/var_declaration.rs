//! A declaration of a variable.
//!
//! In Pascal these happen inside `var` blocks but in typical WEB programs there
//! are also "toplevel" instances.

use nom::{
    combinator::{map, opt},
    multi::separated_list0,
    sequence::tuple,
};

use crate::prettify::{Prettifier, RenderInline};

use super::{
    base::*,
    webtype::{parse_type, WebType},
    WebToplevel,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebVarDeclaration<'a> {
    /// The name(s) of the variable(s).
    names: Vec<StringSpan<'a>>,

    /// The type of the variable(s).
    ty: WebType<'a>,

    /// Optional comment.
    comment: Option<WebComment<'a>>,

    /// Optional second comment.
    second_comment: Option<WebComment<'a>>,
}

pub fn parse_var_declaration_base<'a>(
    input: ParseInput<'a>,
) -> ParseResult<'a, WebVarDeclaration<'a>> {
    map(
        tuple((
            separated_list0(pascal_token(PascalToken::Comma), identifier),
            pascal_token(PascalToken::Colon),
            parse_type,
            pascal_token(PascalToken::Semicolon),
            opt(comment),
            opt(comment),
        )),
        |tup| WebVarDeclaration {
            names: tup.0,
            ty: tup.2,
            comment: tup.4,
            second_comment: tup.5,
        },
    )(input)
}

pub fn parse_var_declaration<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    map(parse_var_declaration_base, |vd| {
        WebToplevel::VarDeclaration(vd)
    })(input)
}

// Prettifying

impl<'a> WebVarDeclaration<'a> {
    pub fn prettify(&self, dest: &mut Prettifier) {
        if let Some(c) = self.comment.as_ref() {
            c.render_inline(dest);
            dest.newline_needed();
        }

        if let Some(c) = self.second_comment.as_ref() {
            c.render_inline(dest);
            dest.newline_needed();
        }

        let mut wi = 4;

        for n in &self.names {
            wi += n.len() + 2; // either ", " or ": "
        }

        wi += self.ty.measure_inline();

        dest.keyword("var");
        dest.space();

        if dest.fits(wi) {
            let mut first = true;

            for n in &self.names {
                if first {
                    first = false;
                } else {
                    dest.noscope_push(", ");
                }

                dest.noscope_push(n);
            }
        } else {
            let i_last = self.names.len() - 1;
            dest.indent_small();

            for (i, n) in self.names.iter().enumerate() {
                dest.noscope_push(n);

                if i != i_last {
                    dest.noscope_push(',');
                    dest.newline_needed();
                }
            }

            dest.dedent_small();
        }

        dest.noscope_push(": ");
        self.ty.render_flex(dest);
        dest.noscope_push(';');
    }
}
