//! A WEB expression.

use nom::{
    branch::alt,
    combinator::map,
    multi::{many1, separated_list0},
    sequence::tuple,
};
use nom_recursive::recursive_parser;

use super::base::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebExpr<'a> {
    /// A binary expression.
    Binary(WebBinaryExpr<'a>),

    /// A prefix unary expression.
    PrefixUnary(WebPrefixUnaryExpr<'a>),

    /// A postfix unary expression.
    PostfixUnary(WebPostfixUnaryExpr<'a>),

    /// Some kind of token that is a valid expression on its own.
    Token(PascalToken<'a>),

    /// Consecutive string literals. The way that we parse these and their
    /// escaping works, these should be treated as one expression.
    Strings(Vec<PascalToken<'a>>),

    /// A function or procedure call.
    Call(WebCallExpr<'a>),

    /// Indexing an array.
    Index(WebIndexExpr<'a>),

    /// A width specifier in a call like `write_ln`
    Format(WebFormatExpr<'a>),

    /// A parenthesized subexpression.
    Paren(Box<WebExpr<'a>>),
}

pub fn parse_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    alt((
        parse_binary_expr,
        parse_prefix_unary_expr,
        parse_paren_expr,
        parse_call_expr,
        parse_index_expr,
        parse_strings,
        parse_format_expr,
        parse_postfix_unary_expr,
        parse_token_expr,
    ))(input)
}

pub fn parse_lhs_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    alt((parse_index_expr, parse_token_expr))(input)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebBinaryExpr<'a> {
    lhs: Box<WebExpr<'a>>,

    op: PascalToken<'a>,

    rhs: Box<WebExpr<'a>>,
}

#[recursive_parser]
fn parse_binary_expr<'a>(s: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    let (s, items) = tuple((parse_expr, binary_expr_op, parse_expr))(s)?;

    let lhs = Box::new(items.0);
    let op = items.1;
    let rhs = Box::new(items.2);

    Ok((s, WebExpr::Binary(WebBinaryExpr { lhs, op, rhs })))
}

fn binary_expr_op<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(pt) = wt {
        match pt {
            PascalToken::Plus
            | PascalToken::Minus
            | PascalToken::Times
            | PascalToken::Divide
            | PascalToken::Greater
            | PascalToken::GreaterEquals
            | PascalToken::Less
            | PascalToken::LessEquals
            | PascalToken::Equals
            | PascalToken::NotEquals
            | PascalToken::ReservedWord(SpanValue {
                value: PascalReservedWord::Mod,
                ..
            }) => return Ok((input, pt)),

            _ => {}
        }
    }

    return new_parse_err(input, WebErrorKind::Eof);
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebPrefixUnaryExpr<'a> {
    op: PascalToken<'a>,

    inner: Box<WebExpr<'a>>,
}

fn parse_prefix_unary_expr<'a>(s: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    let (s, items) = tuple((prefix_unary_expr_op, parse_expr))(s)?;

    let op = items.0;
    let inner = Box::new(items.1);

    Ok((s, WebExpr::PrefixUnary(WebPrefixUnaryExpr { op, inner })))
}

fn prefix_unary_expr_op<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(pt) = wt {
        match pt {
            PascalToken::ReservedWord(SpanValue {
                value: PascalReservedWord::Not,
                ..
            }) => return Ok((input, pt)),

            _ => {}
        }
    }

    return new_parse_err(input, WebErrorKind::Eof);
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebPostfixUnaryExpr<'a> {
    op: PascalToken<'a>,

    inner: Box<WebExpr<'a>>,
}

#[recursive_parser]
fn parse_postfix_unary_expr<'a>(s: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    let (s, items) = tuple((parse_expr, postfix_unary_expr_op))(s)?;

    let inner = Box::new(items.0);
    let op = items.1;

    Ok((s, WebExpr::PostfixUnary(WebPostfixUnaryExpr { op, inner })))
}

fn postfix_unary_expr_op<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(pt) = wt {
        match pt {
            PascalToken::Caret => return Ok((input, pt)),
            _ => {}
        }
    }

    return new_parse_err(input, WebErrorKind::Eof);
}

fn parse_paren_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    map(
        tuple((
            pascal_token(PascalToken::OpenDelimiter(DelimiterKind::Paren)),
            parse_expr,
            pascal_token(PascalToken::CloseDelimiter(DelimiterKind::Paren)),
        )),
        |t| t.1,
    )(input)
}

fn parse_token_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(pt) = wt {
        match pt {
            PascalToken::Identifier(..)
            | PascalToken::Hash(..)
            | PascalToken::IntLiteral(..)
            | PascalToken::StringPoolChecksum => return Ok((input, WebExpr::Token(pt))),

            _ => {}
        }
    }

    return new_parse_err(input, WebErrorKind::Eof);
}

fn parse_strings<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    map(many1(string_literal), |v| WebExpr::Strings(v))(input)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebCallExpr<'a> {
    target: Box<WebExpr<'a>>,

    args: Vec<Box<WebExpr<'a>>>,
}

#[recursive_parser]
fn parse_call_expr<'a>(s: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    let (s, items) = tuple((
        parse_expr,
        open_delimiter(DelimiterKind::Paren),
        separated_list0(
            pascal_token(PascalToken::Comma),
            map(parse_expr, |e| Box::new(e)),
        ),
        close_delimiter(DelimiterKind::Paren),
    ))(s)?;

    let target = Box::new(items.0);
    let args = items.2;

    Ok((s, WebExpr::Call(WebCallExpr { target, args })))
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebIndexExpr<'a> {
    target: Box<WebExpr<'a>>,

    args: Vec<Box<WebExpr<'a>>>,
}

#[recursive_parser]
fn parse_index_expr<'a>(s: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    let (s, items) = tuple((
        parse_expr,
        open_delimiter(DelimiterKind::SquareBracket),
        separated_list0(
            pascal_token(PascalToken::Comma),
            map(parse_expr, |e| Box::new(e)),
        ),
        close_delimiter(DelimiterKind::SquareBracket),
    ))(s)?;

    let target = Box::new(items.0);
    let args = items.2;

    Ok((s, WebExpr::Index(WebIndexExpr { target, args })))
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebFormatExpr<'a> {
    inner: Box<WebExpr<'a>>,
    width: PascalToken<'a>,
}

#[recursive_parser]
fn parse_format_expr<'a>(s: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    let (s, items) = tuple((parse_expr, pascal_token(PascalToken::Colon), int_literal))(s)?;

    let inner = Box::new(items.0);
    let width = items.2;

    Ok((s, WebExpr::Format(WebFormatExpr { inner, width })))
}
