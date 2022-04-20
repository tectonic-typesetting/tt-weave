//! A WEB expression.

use nom::{branch::alt, combinator::map, sequence::tuple};

use super::base::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebExpr<'a> {
    /// A binary expression.
    Binary(WebBinaryExpr<'a>),

    /// An identifier.
    Ident(StringSpan<'a>),
}

pub fn parse_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    eprintln!("EXPR: {:?}", input);
    alt((parse_binary_expr, map(identifier, |i| WebExpr::Ident(i))))(input)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebBinaryExpr<'a> {
    lhs: Box<WebExpr<'a>>,

    op: PascalToken<'a>,

    rhs: Box<WebExpr<'a>>,
}

fn parse_binary_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    let (input, items) = tuple((parse_expr, binary_expr_op, parse_expr))(input)?;

    let lhs = Box::new(items.0);
    let op = items.1;
    let rhs = Box::new(items.2);

    Ok((input, WebExpr::Binary(WebBinaryExpr { lhs, op, rhs })))
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
