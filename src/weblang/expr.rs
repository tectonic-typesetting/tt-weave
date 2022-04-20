//! A WEB expression.

use nom::{branch::alt, combinator::map, sequence::tuple};
use nom_recursive::recursive_parser;

use super::base::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebExpr<'a> {
    /// A binary expression.
    Binary(WebBinaryExpr<'a>),

    /// Some kind of token that is a valid expression on its own.
    Token(PascalToken<'a>),
}

pub fn parse_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    alt((parse_binary_expr, parse_token_expr))(input)
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
            | PascalToken::NotEquals => return Ok((input, pt)),

            _ => {}
        }
    }

    return new_parse_err(input, WebErrorKind::Eof);
}

fn parse_token_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(pt) = wt {
        match pt {
            PascalToken::Identifier(..)
            | PascalToken::Hash(..)
            | PascalToken::IntLiteral(..)
            | PascalToken::StringLiteral(..)
            | PascalToken::StringPoolChecksum => return Ok((input, WebExpr::Token(pt))),

            _ => {}
        }
    }

    return new_parse_err(input, WebErrorKind::Eof);
}
