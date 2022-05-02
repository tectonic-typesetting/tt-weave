//! The beginning of the Pascal program definition
//!
//! `PROGRAM $name($arg1, ...);`
//!
//! Really this should have the same structure as a procedure definition, but
//! WEB implementations always split the program across the entire source file,
//! so the program definition is always incomplete.
//!
//! TODO: reuse some of the block stuff built for function definitions.

use nom::{multi::separated_list0, sequence::tuple};

use crate::prettify::Prettifier;

use super::{base::*, WebToplevel};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebProgramDefinition<'a> {
    name: StringSpan<'a>,
    args: Vec<StringSpan<'a>>,
}

pub fn parse_program_definition<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::Program),
        identifier,
        open_delimiter(DelimiterKind::Paren),
        separated_list0(pascal_token(PascalToken::Comma), identifier),
        close_delimiter(DelimiterKind::Paren),
        pascal_token(PascalToken::Semicolon),
    ))(input)?;

    Ok((
        input,
        WebToplevel::ProgramDefinition(WebProgramDefinition {
            name: items.1,
            args: items.3,
        }),
    ))
}

impl<'a> WebProgramDefinition<'a> {
    pub fn prettify(&self, dest: &mut Prettifier) {
        dest.keyword("program");
        dest.space();
        dest.noscope_push(self.name.value.as_ref());
        dest.noscope_push('(');

        let mut first = true;

        for arg in &self.args {
            if first {
                first = false;
            } else {
                dest.noscope_push(", ");
            }

            dest.noscope_push(arg.value.as_ref());
        }

        dest.noscope_push("):");

        // special! Imbalanced indent!
        dest.indent_block();
        dest.newline_needed();
    }
}
