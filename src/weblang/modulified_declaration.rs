//! TODO: replace this with code used for var blocks etc!

use nom::sequence::tuple;

use crate::prettify::{Prettifier, RenderInline};

use super::{base::*, module_reference::parse_module_reference, WebToplevel};

/// A group of declarations done by referencing a module.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebModulifiedDeclaration<'a> {
    /// The kind of declaration
    kind: PascalReservedWord,

    /// The associated module
    module: WebModuleReference<'a>,
}

/// `(const|type|var) <module-ref>`
pub fn parse_modulified_declaration<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    fn declaration_keyword<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalReservedWord> {
        let (input, wt) = next_token(input)?;

        let rw = if let WebToken::Pascal(PascalToken::ReservedWord(sv)) = wt {
            sv.value
        } else if let WebToken::Pascal(PascalToken::FormattedIdentifier(_, rw)) = wt {
            rw
        } else {
            return new_parse_err(input, WebErrorKind::ExpectedPascalToken);
        };

        match rw {
            PascalReservedWord::Const | PascalReservedWord::Type | PascalReservedWord::Var => {
                Ok((input, rw))
            }

            _ => new_parse_err(input, WebErrorKind::ExpectedPascalToken),
        }
    }

    let (input, items) = tuple((declaration_keyword, parse_module_reference))(input)?;

    Ok((
        input,
        WebToplevel::ModulifiedDeclaration(WebModulifiedDeclaration {
            kind: items.0,
            module: items.1,
        }),
    ))
}

impl<'a> WebModulifiedDeclaration<'a> {
    pub fn prettify(&self, dest: &mut Prettifier) {
        dest.keyword(self.kind);
        dest.indent_block();
        dest.newline_indent();
        self.module.render_inline(dest);
        dest.dedent_block();
        dest.newline_needed();
    }
}
