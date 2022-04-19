//! A WEB statement.

use nom::combinator::map;

use super::base::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebStatement<'a> {
    /// A reference to a module.
    ModuleReference(StringSpan<'a>),
}

pub fn parse_statement<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    // TODO: more!
    map(module_reference, |t| WebStatement::ModuleReference(t))(input)
}
