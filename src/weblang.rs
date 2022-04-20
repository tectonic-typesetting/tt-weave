//! Higher-level WEB language processing.
//!
//! This is *mostly* Pascal, but with a few additions. We implement parsing with
//! `nom` where the underlying datatype is a sequence of tokens.

use nom::{
    branch::alt, bytes::complete::take_while, combinator::map, multi::many1, Finish, InputLength,
};

mod base;
mod define;
mod expr;
mod format;
mod function_definition;
mod ifdef_like;
mod label_declaration;
mod modulified_declaration;
mod preprocessor_directive;
mod program_definition;
mod standalone;
mod statement;

use self::base::*;

pub use self::base::{TypesetComment, WebSyntax, WebToken};

/// A top-level WEB production.
///
/// A "top-level" is whatever it takes to make it true that any WEB Pascal block
/// can be expressed as a series of toplevels, including `@define` and `@format`
/// statements. Because we're not actually compiling the WEB language in any
/// meaningful way, we're not very intellectually rigorous.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebToplevel<'a> {
    /// A `@d` definition.
    Define(define::WebDefine<'a>),

    /// A `@f` format definition.
    Format(format::WebFormat<'a>),

    /// A module reference.
    ModuleReference(StringSpan<'a>),

    /// A single Pascal token (with optional comment).
    Standalone(standalone::WebStandalone<'a>),

    /// The program definition.
    ProgramDefinition(program_definition::WebProgramDefinition<'a>),

    /// A label declaration.
    LabelDeclaration(label_declaration::WebLabelDeclaration<'a>),

    /// Declarations that are done by referencing a module.
    ModulifiedDeclaration(modulified_declaration::WebModulifiedDeclaration<'a>),

    /// Definition of a procedure or function
    FunctionDefinition(function_definition::WebFunctionDefinition<'a>),

    /// A meta-comment for WEB's version of `#ifdef` processing
    IfdefLike(ifdef_like::WebIfdefLike<'a>),

    /// A Pascal preprocessor directive comment.
    PreprocessorDirective(preprocessor_directive::WebPreprocessorDirective<'a>),

    /// A Pascal statement.
    Statement(statement::WebStatement<'a>),
}

/// A block of WEB code: a sequence of parsed-out WEB toplevels
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebCode<'a>(pub Vec<WebToplevel<'a>>);

impl<'a> WebCode<'a> {
    /// Parse a sequence of WEB tokens into sequence of toplevels.
    pub fn parse(syntax: &'a WebSyntax<'a>) -> Option<WebCode<'a>> {
        let input = ParseInput(&syntax.0[..]);

        match many1(parse_toplevel)(input).finish() {
            Ok((remainder, value)) => {
                if remainder.input_len() > 0 {
                    eprintln!("\nincomplete parse");
                    return None;
                } else {
                    return Some(WebCode(value));
                }
            }

            Err((_remainder, e)) => {
                eprintln!("parse error: {:?}", e);
                return None;
            }
        }
    }
}

fn is_ignored_token(t: WebToken) -> bool {
    match t {
        WebToken::Pascal(PascalToken::Formatting)
        | WebToken::Pascal(PascalToken::ForcedEol)
        | WebToken::Pascal(PascalToken::TexString(..)) => true,
        _ => false,
    }
}

fn parse_fails<'b>(input: ParseInput<'b>) -> ParseResult<'b, WebToplevel<'b>> {
    if input.input_len() == 0 {
        new_parse_err(input, WebErrorKind::Eof)
    } else {
        eprintln!("\n\nTL fail at: {:?}\n", input);
        new_parse_err(input, WebErrorKind::ExpectedToplevel)
    }
}

fn parse_toplevel<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, _) = take_while(is_ignored_token)(input)?;

    alt((
        // Define comes first since its tail is a toplevel in and of itself.
        define::parse_define,
        format::parse_format,
        map(module_reference, |s| WebToplevel::ModuleReference(s)),
        program_definition::parse_program_definition,
        label_declaration::parse_label_declaration,
        modulified_declaration::parse_modulified_declaration,
        function_definition::parse_function_definition,
        // This goes before ifdef-like since it's more specific:
        preprocessor_directive::parse_preprocessor_directive,
        ifdef_like::parse_ifdef_like,
        statement::parse_statement,
        // This goes second-to-last since it will match nearly anything:
        standalone::parse_standalone,
        // This goes last for debugging:
        parse_fails,
    ))(input)
}
