//! A WEB expression.

use nom::{branch::alt, combinator::map, multi::separated_list0, sequence::tuple};

use crate::prettify::{self, Prettifier, RenderInline};

use super::{base::*, module_reference::parse_module_reference};

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

    /// A function or procedure call.
    Call(WebCallExpr<'a>),

    /// Indexing an array.
    Index(WebIndexExpr<'a>),

    /// Field access.
    Field(WebFieldAccessExpr<'a>),

    /// A width specifier in a call like `write_ln`
    Format(WebFormatExpr<'a>),

    /// A parenthesized subexpression.
    Paren(Box<WebExpr<'a>>),

    /// A module reference as an expression, needed for XeTeX(2022.0):59.
    ModuleReference(WebModuleReference<'a>),
}

pub fn parse_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    // First try the "advancing" forms, which may recurse with an advanced input,
    // and the "atom" forms, which won't recurse:

    let result = alt((
        parse_prefix_unary_expr,
        parse_paren_expr,
        map(merged_string_literals, |t| WebExpr::Token(t)),
        parse_token_expr,
        map(parse_module_reference, |mr| WebExpr::ModuleReference(mr)),
    ))(input);

    let (mut input, mut expr) = match result {
        Ok(t) => t,
        _ => {
            return result;
        }
    };

    // If that worked, now gobble up as many left-recursive forms as we can.
    // These may recurse, but with an advanced input since we've eaten the
    // "head" subexpression.

    loop {
        let result = alt((
            binary_tail,
            call_tail,
            index_tail,
            field_tail,
            format_tail,
            postfix_unary_tail,
        ))(input);

        if let Ok((new_input, tail)) = result {
            input = new_input;
            expr = tail.finalize(Box::new(expr));
        } else {
            return Ok((input, expr));
        }
    }
}

/// This is like `parse_expr`, but limiting to things that can appear on the
/// left-hand side of an assignment ... pretty much.
///
/// Due to WEB's macros, things that look like function calls can appear
/// as LHSes.
pub fn parse_lhs_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    // LHS-valid advancing/atom forms:

    let result = parse_token_expr(input);

    let (mut input, mut expr) = match result {
        Ok(t) => t,
        _ => {
            return result;
        }
    };

    // LHS-valid left-recursive forms:

    loop {
        let result = alt((call_tail, index_tail, field_tail))(input);

        if let Ok((new_input, tail)) = result {
            input = new_input;
            expr = tail.finalize(Box::new(expr));
        } else {
            return Ok((input, expr));
        }
    }
}

/// Another specialized expr parser for matches in case statements. These are
/// really all integers, but due to WEB's macros may look like integer literals,
/// double-quoted string literals, identifiers, or function calls (WEB macros).
pub fn parse_case_match_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    let result = alt((
        map(merged_string_literals, |t| WebExpr::Token(t)),
        parse_token_expr,
    ))(input);

    let (mut input, mut expr) = match result {
        Ok(t) => t,
        _ => {
            return result;
        }
    };

    // Check for call() form.

    let result = call_tail(input);

    if let Ok((new_input, tail)) = result {
        input = new_input;
        expr = tail.finalize(Box::new(expr));
    }

    Ok((input, expr))
}

// "Atom" forms that do not include sub-expressions

fn parse_token_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(pt) = wt {
        match pt {
            PascalToken::Identifier(..)
            | PascalToken::FormattedIdentifier(_, PascalReservedWord::Nil)
            | PascalToken::Hash(..)
            | PascalToken::IntLiteral(..)
            | PascalToken::StringPoolChecksum => return Ok((input, WebExpr::Token(pt))),

            _ => {}
        }
    }

    return new_parse_err(input, WebErrorKind::Eof);
}

// "Advancing" forms that include sub-expressions, but also require leading
// non-expression tokens, so that they're not left-recursive.

fn parse_paren_expr<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebExpr<'a>> {
    map(
        tuple((
            pascal_token(PascalToken::OpenDelimiter(DelimiterKind::Paren)),
            parse_expr,
            pascal_token(PascalToken::CloseDelimiter(DelimiterKind::Paren)),
        )),
        |t| WebExpr::Paren(Box::new(t.1)),
    )(input)
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
            PascalToken::Plus
            | PascalToken::Minus
            | PascalToken::ReservedWord(SpanValue {
                value: PascalReservedWord::Not,
                ..
            }) => return Ok((input, pt)),

            _ => {}
        }
    }

    return new_parse_err(input, WebErrorKind::Eof);
}

// "Left-recursive" forms that start with a subexpression. We have to
// handle these specially because a naive left-recursion in nom will
// lead to an infinite call stack.

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LeftRecursiveTail<'a> {
    Binary(PascalToken<'a>, Box<WebExpr<'a>>),
    PostfixUnary(PascalToken<'a>),
    Call(Vec<Box<WebExpr<'a>>>),
    Index(Vec<WebIndexTerm<'a>>),
    Field(StringSpan<'a>),
    Format(PascalToken<'a>),
}

impl<'a> LeftRecursiveTail<'a> {
    fn finalize(self, head: Box<WebExpr<'a>>) -> WebExpr<'a> {
        match self {
            LeftRecursiveTail::Binary(op, rhs) => {
                WebExpr::Binary(WebBinaryExpr { lhs: head, op, rhs })
            }
            LeftRecursiveTail::PostfixUnary(op) => {
                WebExpr::PostfixUnary(WebPostfixUnaryExpr { inner: head, op })
            }
            LeftRecursiveTail::Call(args) => WebExpr::Call(WebCallExpr { target: head, args }),
            LeftRecursiveTail::Index(args) => WebExpr::Index(WebIndexExpr { target: head, args }),
            LeftRecursiveTail::Field(field) => {
                WebExpr::Field(WebFieldAccessExpr { item: head, field })
            }
            LeftRecursiveTail::Format(width) => {
                WebExpr::Format(WebFormatExpr { inner: head, width })
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebBinaryExpr<'a> {
    lhs: Box<WebExpr<'a>>,

    op: PascalToken<'a>,

    rhs: Box<WebExpr<'a>>,
}

fn binary_tail<'a>(s: ParseInput<'a>) -> ParseResult<'a, LeftRecursiveTail<'a>> {
    map(tuple((binary_expr_op, parse_expr)), |t| {
        LeftRecursiveTail::Binary(t.0, Box::new(t.1))
    })(s)
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
                value: PascalReservedWord::And,
                ..
            })
            | PascalToken::ReservedWord(SpanValue {
                value: PascalReservedWord::Div,
                ..
            })
            | PascalToken::ReservedWord(SpanValue {
                value: PascalReservedWord::Mod,
                ..
            })
            | PascalToken::ReservedWord(SpanValue {
                value: PascalReservedWord::Or,
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

fn postfix_unary_tail<'a>(s: ParseInput<'a>) -> ParseResult<'a, LeftRecursiveTail<'a>> {
    map(postfix_unary_expr_op, |o| {
        LeftRecursiveTail::PostfixUnary(o)
    })(s)
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebCallExpr<'a> {
    target: Box<WebExpr<'a>>,

    args: Vec<Box<WebExpr<'a>>>,
}

fn call_tail<'a>(s: ParseInput<'a>) -> ParseResult<'a, LeftRecursiveTail<'a>> {
    map(
        tuple((
            open_delimiter(DelimiterKind::Paren),
            separated_list0(
                pascal_token(PascalToken::Comma),
                map(parse_expr, |e| Box::new(e)),
            ),
            close_delimiter(DelimiterKind::Paren),
        )),
        |t| LeftRecursiveTail::Call(t.1),
    )(s)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebIndexExpr<'a> {
    target: Box<WebExpr<'a>>,

    args: Vec<WebIndexTerm<'a>>,
}

/// The `Range` option is needed for some inline Pascal such as in WEAVE#65.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebIndexTerm<'a> {
    Expr(Box<WebExpr<'a>>),
    Range(Box<WebExpr<'a>>, Box<WebExpr<'a>>),
}

fn index_tail<'a>(s: ParseInput<'a>) -> ParseResult<'a, LeftRecursiveTail<'a>> {
    map(
        tuple((
            open_delimiter(DelimiterKind::SquareBracket),
            separated_list0(pascal_token(PascalToken::Comma), index_term),
            close_delimiter(DelimiterKind::SquareBracket),
        )),
        |t| LeftRecursiveTail::Index(t.1),
    )(s)
}

fn index_term<'a>(s: ParseInput<'a>) -> ParseResult<'a, WebIndexTerm<'a>> {
    alt((
        range_index_term,
        map(parse_expr, |e| WebIndexTerm::Expr(Box::new(e))),
    ))(s)
}

fn range_index_term<'a>(s: ParseInput<'a>) -> ParseResult<'a, WebIndexTerm<'a>> {
    map(
        tuple((
            parse_token_expr,
            pascal_token(PascalToken::DoubleDot),
            parse_expr,
        )),
        |t| WebIndexTerm::Range(Box::new(t.0), Box::new(t.2)),
    )(s)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebFormatExpr<'a> {
    inner: Box<WebExpr<'a>>,
    width: PascalToken<'a>,
}

fn format_tail<'a>(s: ParseInput<'a>) -> ParseResult<'a, LeftRecursiveTail<'a>> {
    map(
        tuple((pascal_token(PascalToken::Colon), int_literal)),
        |t| LeftRecursiveTail::Format(t.1),
    )(s)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebFieldAccessExpr<'a> {
    item: Box<WebExpr<'a>>,
    field: StringSpan<'a>,
}

fn field_tail<'a>(s: ParseInput<'a>) -> ParseResult<'a, LeftRecursiveTail<'a>> {
    map(
        tuple((pascal_token(PascalToken::Period), identifier)),
        |t| LeftRecursiveTail::Field(t.1),
    )(s)
}

// Prettification

impl<'a> RenderInline for WebExpr<'a> {
    fn measure_inline(&self) -> usize {
        match self {
            WebExpr::Token(tok) => tok.measure_inline(),

            WebExpr::Binary(bin) => {
                bin.lhs.measure_inline() + bin.rhs.measure_inline() + bin.op.measure_inline() + 2
            }

            WebExpr::PrefixUnary(pu) => pu.op.measure_inline() + pu.inner.measure_inline(),

            WebExpr::PostfixUnary(pu) => pu.op.measure_inline() + pu.inner.measure_inline(),

            WebExpr::Call(call) => {
                call.target.measure_inline() + prettify::measure_inline_seq(&call.args, 2) + 2
            }

            WebExpr::Index(idx) => {
                idx.target.measure_inline() + prettify::measure_inline_seq(&idx.args, 2) + 2
            }

            WebExpr::Field(f) => f.item.measure_inline() + 1 + f.field.len(),

            WebExpr::Format(f) => f.inner.measure_inline() + 1 + f.width.measure_inline(),

            WebExpr::Paren(p) => p.measure_inline() + 2,

            WebExpr::ModuleReference(mr) => mr.measure_inline(),
        }
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        match self {
            WebExpr::Token(tok) => tok.render_inline(dest),

            WebExpr::Binary(bin) => {
                bin.lhs.render_inline(dest);
                dest.space();
                bin.op.render_inline(dest);
                dest.space();
                bin.rhs.render_inline(dest);
            }

            WebExpr::PrefixUnary(pu) => {
                pu.op.render_inline(dest);
                pu.inner.render_inline(dest);
            }

            WebExpr::PostfixUnary(pu) => {
                pu.inner.render_inline(dest);
                pu.op.render_inline(dest);
            }

            WebExpr::Call(call) => {
                call.target.render_inline(dest);
                dest.noscope_push('(');
                prettify::render_inline_seq(&call.args, ", ", dest);
                dest.noscope_push(')');
            }

            WebExpr::Index(idx) => {
                idx.target.render_inline(dest);
                dest.noscope_push('[');
                prettify::render_inline_seq(&idx.args, ", ", dest);
                dest.noscope_push(']');
            }

            WebExpr::Field(f) => {
                f.item.render_inline(dest);
                dest.noscope_push('.');
                dest.noscope_push(&f.field);
            }

            WebExpr::Format(f) => {
                f.inner.render_inline(dest);
                dest.noscope_push(':');
                f.width.render_inline(dest);
            }

            WebExpr::Paren(p) => {
                dest.noscope_push('(');
                p.render_inline(dest);
                dest.noscope_push(')');
            }

            WebExpr::ModuleReference(mr) => mr.render_inline(dest),
        }
    }
}

impl<'a> WebExpr<'a> {
    pub fn render_flex(&self, dest: &mut Prettifier) {
        match self {
            WebExpr::Token(tok) => {
                let w = tok.measure_inline();

                // If it doesn't fit, it's possible that moving to a new line
                // wouldn't actually help any (cf the string literal in
                // WEAVE:106.
                if dest.fits(w) || !dest.would_fit_on_new_line(w + 2) {
                    tok.render_inline(dest);
                } else {
                    dest.noscope_push('(');
                    dest.indent_small();
                    dest.newline_indent();
                    tok.render_inline(dest);
                    dest.dedent_small();
                    dest.newline_indent();
                    dest.noscope_push(')');
                }
            }

            WebExpr::PrefixUnary(pu) => {
                pu.op.render_inline(dest);
                pu.inner.render_flex(dest);
            }

            WebExpr::PostfixUnary(pu) => {
                pu.inner.render_flex(dest);
                pu.op.render_inline(dest);
            }

            WebExpr::Binary(be) => {
                let wl = be.lhs.measure_inline();
                let wr = be.rhs.measure_inline();
                let wo = be.op.measure_inline();

                if dest.fits(wl + wr + wo + 2) {
                    be.lhs.render_inline(dest);
                    dest.space();
                    be.op.render_inline(dest);
                    dest.space();
                    be.rhs.render_inline(dest);
                } else {
                    dest.indent_block();
                    dest.newline_indent();
                    be.lhs.render_flex(dest);
                    dest.newline_indent();
                    be.op.render_inline(dest);
                    dest.space();
                    be.rhs.render_flex_maybe_continue_binop(dest);
                    dest.dedent_block();
                    dest.newline_needed();
                }
            }

            WebExpr::Call(call) => {
                let wa = prettify::measure_inline_seq(&call.args, 2) + 2;

                call.target.render_flex(dest);
                dest.noscope_push('(');

                if dest.fits(wa) {
                    prettify::render_inline_seq(&call.args, ", ", dest);
                } else {
                    dest.indent_small();

                    for arg in &call.args {
                        dest.newline_indent();
                        arg.render_flex(dest);
                        dest.noscope_push(",");
                    }

                    dest.dedent_small();
                    dest.newline_indent();
                }

                dest.noscope_push(')');
            }

            WebExpr::Index(idx) => {
                let wa = prettify::measure_inline_seq(&idx.args, 2) + 2;

                idx.target.render_flex(dest);
                dest.noscope_push('[');

                if dest.fits(wa) {
                    prettify::render_inline_seq(&idx.args, ", ", dest);
                } else {
                    dest.indent_small();

                    for arg in &idx.args {
                        dest.newline_indent();
                        arg.render_flex(dest);
                        dest.noscope_push(",");
                    }

                    dest.dedent_small();
                    dest.newline_indent();
                }

                dest.noscope_push(']');
            }

            WebExpr::Field(f) => {
                let wf = f.field.len() + 1;

                f.item.render_flex(dest);

                if dest.fits(wf) {
                    dest.noscope_push('.');
                    dest.noscope_push(f.field.value.as_ref());
                } else {
                    dest.indent_small();
                    dest.newline_indent();
                    dest.noscope_push('.');
                    dest.noscope_push(f.field.value.as_ref());
                    dest.dedent_small();
                }
            }

            WebExpr::Format(f) => {
                let ww = f.width.measure_inline() + 1;

                f.inner.render_flex(dest);

                if dest.fits(ww) {
                    dest.noscope_push(':');
                    f.width.render_inline(dest);
                } else {
                    // This would look bad, but should also ~never come up
                    dest.indent_small();
                    dest.newline_indent();
                    dest.noscope_push(':');
                    f.width.render_inline(dest);
                    dest.dedent_small();
                }
            }

            WebExpr::Paren(p) => {
                let w = p.measure_inline() + 2;

                if dest.fits(w) {
                    dest.noscope_push('(');
                    p.render_inline(dest);
                    dest.noscope_push(')');
                } else {
                    dest.noscope_push('(');
                    p.render_flex(dest);
                    dest.noscope_push(')');
                }
            }

            WebExpr::ModuleReference(mr) => {
                // Same approach as Token:
                let w = mr.measure_inline();

                if dest.fits(w) || !dest.would_fit_on_new_line(w + 2) {
                    mr.render_inline(dest);
                } else {
                    dest.noscope_push('(');
                    dest.indent_small();
                    dest.newline_indent();
                    mr.render_inline(dest);
                    dest.dedent_small();
                    dest.newline_indent();
                    dest.noscope_push(')');
                }
            }
        }
    }

    /// Called when rendering a binary operator as a continauation of a vertical
    /// rendering of another binary operator. We can chain them to avoid another
    /// indent.
    ///
    /// This will be called with `dest` filled out ready to display `self` as the
    /// RHS at the pipe symbol here:
    ///
    /// ```
    /// <LHS>
    /// <op> |<RHS>
    /// ```
    fn render_flex_maybe_continue_binop(&self, dest: &mut Prettifier) {
        match self {
            WebExpr::Binary(be) => {
                let wl = be.lhs.measure_inline();
                let wr = be.rhs.measure_inline();
                let wo = be.op.measure_inline();

                if dest.fits(wl + wr + wo + 2) {
                    be.lhs.render_inline(dest);
                    dest.space();
                    be.op.render_inline(dest);
                    dest.space();
                    be.rhs.render_inline(dest);
                } else if dest.fits(wl) {
                    // This bit is the novelty compared to plain `render_flex()`
                    be.lhs.render_flex(dest);
                    dest.newline_indent();
                    be.op.render_inline(dest);
                    dest.space();
                    be.rhs.render_flex_maybe_continue_binop(dest);
                } else {
                    be.lhs.render_flex(dest);
                    dest.newline_indent();
                    be.op.render_inline(dest);
                    dest.space();
                    be.rhs.render_flex_maybe_continue_binop(dest);
                    dest.newline_needed();
                }
            }

            _ => {
                self.render_flex(dest);
            }
        }
    }
}

impl<'a> RenderInline for WebIndexTerm<'a> {
    fn measure_inline(&self) -> usize {
        match self {
            WebIndexTerm::Expr(e) => e.measure_inline(),
            WebIndexTerm::Range(lo, hi) => lo.measure_inline() + 4 + hi.measure_inline(),
        }
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        match self {
            WebIndexTerm::Expr(e) => e.render_inline(dest),
            WebIndexTerm::Range(lo, hi) => {
                lo.render_inline(dest);
                dest.noscope_push(" .. ");
                hi.render_inline(dest);
            }
        }
    }
}

impl<'a> WebIndexTerm<'a> {
    pub fn render_flex(&self, dest: &mut Prettifier) {
        match self {
            WebIndexTerm::Expr(e) => e.render_flex(dest),
            WebIndexTerm::Range(lo, hi) => {
                lo.render_flex(dest);
                dest.noscope_push(" .. ");
                hi.render_flex(dest);
            }
        }
    }
}
