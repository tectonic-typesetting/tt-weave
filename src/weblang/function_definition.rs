//! A WEB/Pascal function definition.
//!
//! Here we group both functions and procedures together, the difference being
//! whether there's a return value or not.

use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many1, separated_list0},
    sequence::tuple,
};

use super::{
    base::*,
    statement::{parse_statement_base, WebStatement},
    webtype::{parse_type, WebType},
    WebToplevel,
};

/// Definition of a function or procedure.
///
/// For simplicity, we just call them both "functions".
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebFunctionDefinition<'a> {
    /// The name of the function.
    name: StringSpan<'a>,

    /// The function's arguments.
    args: Vec<WebVariables<'a>>,

    /// The return type. If `Some`, this is a function; otherwise it is a
    /// procedure.
    return_type: Option<WebType<'a>>,

    /// The comment associated with the definition of the function.
    opening_comment: Option<Vec<TypesetComment<'a>>>,

    /// Records in the function's `var` block.
    vars: Vec<WebVarBlockItem<'a>>,

    /// The statements that comprise the function.
    stmts: Vec<WebStatement<'a>>,
}

// The `var` block

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebVariables<'a> {
    /// Whether a function argument is marked with the `var` keyword. This may
    /// be more properly per-name, but this is sufficient for our use case.
    is_var: bool,

    /// The name(s) of the variable(s).
    names: Vec<StringSpan<'a>>,

    /// The type of the variable(s).
    ty: WebType<'a>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebVarBlockItem<'a> {
    /// A reference to a module that (hopefully) contains variable definitions.
    ModuleReference(StringSpan<'a>),

    /// Actual in-place definitions
    InPlace(WebVariables<'a>, Option<Vec<TypesetComment<'a>>>),
}

fn parse_var_block_item<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebVarBlockItem<'a>> {
    alt((
        map(module_reference, |t| WebVarBlockItem::ModuleReference(t)),
        map(
            tuple((
                separated_list0(pascal_token(PascalToken::Comma), identifier),
                pascal_token(PascalToken::Colon),
                parse_type,
                pascal_token(PascalToken::Semicolon),
                opt(comment),
            )),
            |tup| {
                WebVarBlockItem::InPlace(
                    WebVariables {
                        is_var: false,
                        names: tup.0,
                        ty: tup.2,
                    },
                    tup.4,
                )
            },
        ),
    ))(input)
}

fn parse_argument_group<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebVariables<'a>> {
    map(
        tuple((
            opt(reserved_word(PascalReservedWord::Var)),
            separated_list0(pascal_token(PascalToken::Comma), identifier),
            pascal_token(PascalToken::Colon),
            parse_type,
        )),
        |tup| WebVariables {
            is_var: tup.0.is_some(),
            names: tup.1,
            ty: tup.3,
        },
    )(input)
}

// Tying it all together

pub fn parse_function_definition<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, items) = tuple((
        alt((
            reserved_word(PascalReservedWord::Function),
            reserved_word(PascalReservedWord::Procedure),
        )),
        identifier,
        opt(tuple((
            open_delimiter(DelimiterKind::Paren),
            separated_list0(pascal_token(PascalToken::Semicolon), parse_argument_group),
            close_delimiter(DelimiterKind::Paren),
        ))),
        opt(tuple((pascal_token(PascalToken::Colon), parse_type))),
        pascal_token(PascalToken::Semicolon),
        opt(comment),
        opt(tuple((
            reserved_word(PascalReservedWord::Var),
            many1(parse_var_block_item),
        ))),
        reserved_word(PascalReservedWord::Begin),
        many1(parse_statement_base),
        reserved_word(PascalReservedWord::End),
        pascal_token(PascalToken::Semicolon),
    ))(input)?;

    let name = items.1;
    let args = items.2.map(|t| t.1).unwrap_or_default();
    let return_type = items.3.map(|t| t.1);
    let opening_comment = items.5;
    let vars = items.6.map(|t| t.1).unwrap_or_default();
    let stmts = items.8;

    Ok((
        input,
        WebToplevel::FunctionDefinition(WebFunctionDefinition {
            name,
            args,
            return_type,
            opening_comment,
            vars,
            stmts,
        }),
    ))
}
