//! A WEB `@d` definition.
//!
//! This has the general form `@d LHS == RHS`. The LHS might not be simple
//! identifier if it has macro parameter, and the RHS can be any toplevel.

use nom::{
    branch::alt,
    bytes::complete::take_while1,
    combinator::{map, opt},
    multi::{many1, separated_list1},
    sequence::tuple,
    InputLength,
};

use super::{
    base::*,
    expr::{parse_expr, WebExpr},
    standalone, statement, WebToplevel,
};

/// A `@d` definition
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebDefine<'a> {
    /// The LHS of the define. This may be a sequence of tokens like `blah(#)`.
    lhs: Vec<PascalToken<'a>>,

    /// The right hand side.
    rhs: WebDefineRhs<'a>,

    /// Optional trailing comment.
    comment: Option<Vec<TypesetComment<'a>>>,
}

pub fn parse_define<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    fn is_define_lhs_token(t: WebToken) -> bool {
        if let Some(pt) = t.as_pascal() {
            match pt {
                PascalToken::Equals | PascalToken::Equivalence => false,
                _ => true,
            }
        } else {
            false
        }
    }

    let (input, items) = tuple((
        reserved_word(PascalReservedWord::Define),
        take_while1(is_define_lhs_token),
        alt((
            pascal_token(PascalToken::Equivalence),
            pascal_token(PascalToken::Equals),
        )),
        alt((
            parse_inverted_statement,
            tuple((parse_define_rhs, opt(comment))),
        )),
    ))(input)?;

    if input.input_len() != 0 {
        eprintln!("\n\n** incomplete @define parse, remaining: {:?}\n", input);
        return new_parse_err(input, WebErrorKind::IncompleteDefine);
    }

    let lhs = items.1 .0.iter().map(|t| t.clone().into_pascal()).collect();
    let rhs = items.3 .0;
    let comment = items.3 .1;

    Ok((input, WebToplevel::Define(WebDefine { lhs, rhs, comment })))
}

/// The right-hand-side of a `@d` definition
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebDefineRhs<'a> {
    Standalone(standalone::WebStandalone<'a>),

    /// The boolean specifies whether it's an opener or closer
    IfdefLike(bool),

    /// Definition of `loop`
    LoopDefinition(StringSpan<'a>),

    /// Definition of `do_nothing`: an empty statement
    EmptyDefinition,

    /// Definition of `othercases`: `{label}{colon}`
    OthercasesDefinition(StringSpan<'a>),

    Statements(Vec<statement::WebStatement<'a>>),

    /// A comma-separated group of exprs, needed for WEAVE#95.
    CommaExprs(Vec<Box<WebExpr<'a>>>),

    /// An expr followed by End, needed for WEAVE#125.
    ExprEnd(Box<WebExpr<'a>>),

    /// An expr followed by an identifier, also needed for WEAVE#125.
    ExprIdent(Box<WebExpr<'a>>, StringSpan<'a>),

    /// Begin followed by an identifier, also needed for WEAVE#125.
    BeginIdent(StringSpan<'a>),

    Expr(Box<WebExpr<'a>>),
}

fn parse_define_rhs<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    alt((
        parse_ifdef_like,
        parse_loop_definition,
        map(peek_end_of_define, |_| WebDefineRhs::EmptyDefinition),
        parse_othercases,
        map(
            tuple((many1(statement::parse_statement_base), peek_end_of_define)),
            |t| WebDefineRhs::Statements(t.0),
        ),
        map(
            tuple((
                reserved_word(PascalReservedWord::Begin),
                identifier,
                peek_end_of_define,
            )),
            |t| WebDefineRhs::BeginIdent(t.1),
        ),
        parse_exprs,
        map(standalone::parse_standalone_base, |s| {
            WebDefineRhs::Standalone(s)
        }),
    ))(input)
}

/// There are some constructs rendered as `@d foo = { comment } begin ... end`.
/// We handle fiddle with these to treat them homogeneously with other statements
fn parse_inverted_statement<'a>(
    input: ParseInput<'a>,
) -> ParseResult<'a, (WebDefineRhs<'a>, Option<Vec<TypesetComment<'a>>>)> {
    map(
        tuple((
            map(comment, |c| Some(c)),
            map(many1(statement::parse_statement_base), |s| {
                WebDefineRhs::Statements(s)
            }),
            peek_end_of_define,
        )),
        |t| (t.1, t.0),
    )(input)
}

/// Verify that the current input position is an "edge" of define content:
/// either an empty input, or there's a comment coming up. But we don't
/// consume the comment.
fn peek_end_of_define<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    match input.input_len() {
        0 => return Ok((input, ())),
        1 => {}
        _ => return new_parse_err(input, WebErrorKind::NotDefineEdge),
    }

    if comment(input).is_ok() {
        Ok((input, ()))
    } else {
        new_parse_err(input, WebErrorKind::NotDefineEdge)
    }
}

fn parse_ifdef_like<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    let (input, items) = tuple((
        alt((
            pascal_token(PascalToken::OpenDelimiter(DelimiterKind::MetaComment)),
            pascal_token(PascalToken::CloseDelimiter(DelimiterKind::MetaComment)),
        )),
        peek_end_of_define,
    ))(input)?;

    let is_open = if let PascalToken::OpenDelimiter(DelimiterKind::MetaComment) = items.0 {
        true
    } else {
        false
    };

    Ok((input, WebDefineRhs::IfdefLike(is_open)))
}

fn parse_loop_definition<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::While),
        identifier,
        reserved_word(PascalReservedWord::Do),
        peek_end_of_define,
    ))(input)?;

    Ok((input, WebDefineRhs::LoopDefinition(items.1)))
}

fn parse_othercases<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    map(
        tuple((
            identifier,
            pascal_token(PascalToken::Colon),
            peek_end_of_define,
        )),
        |t| WebDefineRhs::OthercasesDefinition(t.0),
    )(input)
}

fn parse_exprs<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    let (input, mut exprs) =
        separated_list1(pascal_token(PascalToken::Comma), map(parse_expr, Box::new))(input)?;

    if exprs.len() == 1 {
        if let Ok((next_input, _)) = reserved_word(PascalReservedWord::End)(input) {
            Ok((next_input, WebDefineRhs::ExprEnd(exprs.pop().unwrap())))
        } else if let Ok((next_input, ss)) = identifier(input) {
            Ok((
                next_input,
                WebDefineRhs::ExprIdent(exprs.pop().unwrap(), ss),
            ))
        } else {
            Ok((input, WebDefineRhs::Expr(exprs.pop().unwrap())))
        }
    } else {
        Ok((input, WebDefineRhs::CommaExprs(exprs)))
    }
}
