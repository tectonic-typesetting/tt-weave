//! A WEB statement.

use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many0, many1, separated_list1},
    sequence::tuple,
};
use std::borrow::Cow;

use crate::prettify::{self, Prettifier};

use super::{
    base::*,
    expr::{parse_case_match_expr, parse_expr, parse_lhs_expr, WebExpr},
    preprocessor_directive, WebToplevel,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebStatement<'a> {
    /// A reference to a module.
    ModuleReference(StringSpan<'a>),

    /// A block of statements.
    Block(WebBlock<'a>),

    /// An assignment.
    Assignment(WebAssignment<'a>),

    /// A preprocessor directive.
    PreprocessorDirective(preprocessor_directive::WebPreprocessorDirective<'a>),

    /// A goto
    Goto(WebGoto<'a>),

    /// An `if` statement.
    If(WebIf<'a>),

    /// A `while` loop.
    While(WebWhile<'a>),

    /// A `for` loop.
    For(WebFor<'a>),

    /// A `repeat`/`until` loop.
    Repeat(WebRepeat<'a>),

    /// A `loop` loop, implemented with a @define formatted like `Xclause`
    Loop(WebLoop<'a>),

    /// A label.
    Label(StringSpan<'a>),

    /// A case statement.
    Case(WebCase<'a>),

    /// A statement that's just an expression.
    Expr(WebExpr<'a>, Option<WebComment<'a>>),

    /// A free-floating case statement, needed for WEAVE#88.
    SpecialFreeCase(SpecialFreeCase<'a>),
}

pub fn parse_statement_base<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    alt((
        parse_mod_ref_statement,
        parse_block,
        map(
            preprocessor_directive::parse_preprocessor_directive_base,
            |d| WebStatement::PreprocessorDirective(d),
        ),
        parse_goto,
        parse_if,
        parse_while,
        parse_for,
        parse_case,
        parse_repeat,
        parse_assignment,
        parse_label,
        parse_loop,
        parse_special_free_case,
        parse_expr_statement,
    ))(input)
}

pub fn parse_statement<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    map(tuple((parse_statement_base, opt(comment))), |t| {
        WebToplevel::Statement(t.0, t.1)
    })(input)
}

fn parse_mod_ref_statement<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    map(
        tuple((module_reference, opt(pascal_token(PascalToken::Semicolon)))),
        |t| WebStatement::ModuleReference(t.0),
    )(input)
}

fn parse_expr_statement<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    map(
        tuple((
            parse_expr,
            opt(pascal_token(PascalToken::Semicolon)),
            opt(comment),
        )),
        |t| WebStatement::Expr(t.0, t.2),
    )(input)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebBlock<'a> {
    /// The token that opens the block.
    opener: PascalToken<'a>,

    /// Inner statements.
    stmts: Vec<Box<WebStatement<'a>>>,

    /// The token that closes the block.
    closer: PascalToken<'a>,

    /// Optional comment after
    post_comment: Option<WebComment<'a>>,
}

fn parse_block<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    let (input, items) = tuple((
        block_opener,
        many0(map(parse_statement_base, |s| Box::new(s))),
        block_closer,
        opt(pascal_token(PascalToken::Semicolon)),
        opt(pascal_token(PascalToken::Period)), // for the very end of program
        opt(comment),
    ))(input)?;

    let opener = items.0;
    let stmts = items.1;
    let closer = items.2;
    let post_comment = items.5;

    Ok((
        input,
        WebStatement::Block(WebBlock {
            opener,
            stmts,
            closer,
            post_comment,
        }),
    ))
}

/// Match a token that opens a block: either `begin`, or a formatted identifier
/// that behaves like it.
fn block_opener<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(ptok) = wt {
        if let PascalToken::ReservedWord(SpanValue {
            value: PascalReservedWord::Begin,
            ..
        }) = ptok
        {
            return Ok((input, ptok));
        } else if let PascalToken::FormattedIdentifier(_, PascalReservedWord::Begin) = ptok {
            return Ok((input, ptok));
        }
    }

    return new_parse_err(input, WebErrorKind::Eof);
}

/// Match a token that closes a block: either `end`, or a formatted identifier
/// that behaves like it.
fn block_closer<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(ptok) = wt {
        if let PascalToken::ReservedWord(SpanValue {
            value: PascalReservedWord::End,
            ..
        }) = ptok
        {
            return Ok((input, ptok));
        } else if let PascalToken::FormattedIdentifier(_, PascalReservedWord::End) = ptok {
            return Ok((input, ptok));
        }
    }

    return new_parse_err(input, WebErrorKind::Eof);
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebAssignment<'a> {
    /// The left-hand side.
    lhs: Box<WebExpr<'a>>,

    /// The right-hand side.
    rhs: Box<WebExpr<'a>>,

    /// Optional comment.
    comment: Option<WebComment<'a>>,
}

fn parse_assignment<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    let (input, items) = tuple((
        parse_lhs_expr,
        pascal_token(PascalToken::Gets),
        parse_expr,
        opt(pascal_token(PascalToken::Semicolon)),
        opt(comment),
    ))(input)?;

    let lhs = Box::new(items.0);
    let rhs = Box::new(items.2);
    let comment = items.4;

    Ok((
        input,
        WebStatement::Assignment(WebAssignment { lhs, rhs, comment }),
    ))
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebGoto<'a> {
    /// The label.
    label: StringSpan<'a>,

    /// Optional comment.
    comment: Option<WebComment<'a>>,
}

fn parse_goto<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::Goto),
        identifier,
        opt(pascal_token(PascalToken::Semicolon)),
        opt(comment),
    ))(input)?;

    let label = items.1;
    let comment = items.3;

    Ok((input, WebStatement::Goto(WebGoto { label, comment })))
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebIf<'a> {
    /// The test expression
    test: Box<WebExpr<'a>>,

    /// Optional comment after the test
    test_comment: Option<WebComment<'a>>,

    /// The `then` statement, which may be a block.
    then: Box<WebStatement<'a>>,

    /// The optional `else` statement, which may be a block, or may be another
    /// `if` statement.
    else_: Option<Box<WebStatement<'a>>>,
}

fn parse_if<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::If),
        parse_expr,
        reserved_word(PascalReservedWord::Then),
        opt(comment),
        parse_statement_base,
        opt(tuple((
            reserved_word(PascalReservedWord::Else),
            parse_statement_base,
        ))),
    ))(input)?;

    let test = Box::new(items.1);
    let test_comment = items.3;
    let then = Box::new(items.4);
    let else_ = items.5.map(|t| Box::new(t.1));

    Ok((
        input,
        WebStatement::If(WebIf {
            test,
            test_comment,
            then,
            else_,
        }),
    ))
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebWhile<'a> {
    /// The loop test expression
    test: Box<WebExpr<'a>>,

    /// The `do` statement, which may be a block.
    do_: Box<WebStatement<'a>>,
}

fn parse_while<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::While),
        parse_expr,
        reserved_word(PascalReservedWord::Do),
        parse_statement_base,
    ))(input)?;

    let test = Box::new(items.1);
    let do_ = Box::new(items.3);

    Ok((input, WebStatement::While(WebWhile { test, do_ })))
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebFor<'a> {
    /// The loop variable
    var: StringSpan<'a>,

    /// The start expression.
    start: Box<WebExpr<'a>>,

    /// Whether this is a "downto" (decreasing) loop, rather than increasing.
    is_down: bool,

    /// The end expression.
    end: Box<WebExpr<'a>>,

    /// The `do` statement, which may be a block.
    do_: Box<WebStatement<'a>>,
}

fn parse_for<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::For),
        identifier,
        pascal_token(PascalToken::Gets),
        parse_expr,
        parse_for_direction_word,
        parse_expr,
        reserved_word(PascalReservedWord::Do),
        parse_statement_base,
    ))(input)?;

    let var = items.1;
    let start = Box::new(items.3);
    let is_down = items.4;
    let end = Box::new(items.5);
    let do_ = Box::new(items.7);

    Ok((
        input,
        WebStatement::For(WebFor {
            var,
            start,
            is_down,
            end,
            do_,
        }),
    ))
}

fn parse_for_direction_word<'a>(input: ParseInput<'a>) -> ParseResult<'a, bool> {
    alt((
        map(reserved_word(PascalReservedWord::To), |_| false),
        map(reserved_word(PascalReservedWord::Downto), |_| true),
    ))(input)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebRepeat<'a> {
    /// The loop test expression
    test: Box<WebExpr<'a>>,

    /// The statements comprising the loop. Unlike most other compound
    /// statements, these come in a sequence without being encased in a
    /// begin/end block.
    stmts: Vec<Box<WebStatement<'a>>>,
}

fn parse_repeat<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    map(
        tuple((
            reserved_word(PascalReservedWord::Repeat),
            many1(map(parse_statement_base, |s| Box::new(s))),
            reserved_word(PascalReservedWord::Until),
            parse_expr,
            pascal_token(PascalToken::Semicolon),
        )),
        |t| {
            WebStatement::Repeat(WebRepeat {
                test: Box::new(t.3),
                stmts: t.1,
            })
        },
    )(input)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebLoop<'a> {
    /// The identifier used in the loop definition
    keyword: StringSpan<'a>,

    /// The `do` statement, which may be a block.
    do_: Box<WebStatement<'a>>,
}

fn parse_loop<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    map(tuple((loop_like_identifier, parse_statement_base)), |t| {
        WebStatement::Loop(WebLoop {
            keyword: t.0,
            do_: Box::new(t.1),
        })
    })(input)
}

pub fn loop_like_identifier<'a>(input: ParseInput<'a>) -> ParseResult<'a, StringSpan<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(PascalToken::FormattedIdentifier(ss, PascalReservedWord::Xclause)) = wt
    {
        Ok((input, ss))
    } else {
        new_parse_err(input, WebErrorKind::Eof)
    }
}

fn parse_label<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    map(tuple((identifier, pascal_token(PascalToken::Colon))), |t| {
        WebStatement::Label(t.0)
    })(input)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebCase<'a> {
    /// The input to the case statement.
    var: Box<WebExpr<'a>>,

    /// Items within the case statement.
    items: Vec<WebCaseItem<'a>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebCaseItem<'a> {
    ModuleReference(StringSpan<'a>),
    Standard(WebStandardCaseItem<'a>),
    OtherCases(WebOtherCasesItem<'a>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebStandardCaseItem<'a> {
    /// The- matched cases. These may be identifiers, string literals,
    /// integer literals, or WEB macros that look like function calls.
    matches: Vec<Box<WebExpr<'a>>>,

    /// The associated statement.
    stmt: Box<WebStatement<'a>>,

    /// Optional comment.
    comment: Option<WebComment<'a>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebOtherCasesItem<'a> {
    /// The formatted identifier used to tag this item.
    tag: StringSpan<'a>,

    /// The associated statement.
    stmt: Box<WebStatement<'a>>,

    /// Optional comment.
    comment: Option<WebComment<'a>>,
}

fn parse_case<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    map(
        tuple((
            reserved_word(PascalReservedWord::Case),
            map(parse_expr, Box::new),
            reserved_word(PascalReservedWord::Of),
            many1(alt((
                parse_mod_ref_case_item,
                parse_other_cases_item,
                parse_standard_case_item,
            ))),
            parse_case_terminator,
            opt(pascal_token(PascalToken::Semicolon)),
        )),
        |t| {
            WebStatement::Case(WebCase {
                var: t.1,
                items: t.3,
            })
        },
    )(input)
}

/// `endcases` is a formatted identifier formatted like `End`
///
/// WEAVE#192 ends the case state with an actual End keyword.
fn parse_case_terminator<'a>(input: ParseInput<'a>) -> ParseResult<'a, StringSpan<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(PascalToken::FormattedIdentifier(ss, PascalReservedWord::End)) = wt {
        Ok((input, ss))
    } else if let WebToken::Pascal(PascalToken::ReservedWord(SpanValue {
        value: PascalReservedWord::End,
        start,
        end,
    })) = wt
    {
        let ss = StringSpan {
            start,
            end,
            value: Cow::Owned("end".to_owned()),
        };
        Ok((input, ss))
    } else {
        new_parse_err(input, WebErrorKind::Eof)
    }
}

fn parse_mod_ref_case_item<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebCaseItem<'a>> {
    map(
        tuple((module_reference, opt(pascal_token(PascalToken::Semicolon)))),
        |t| WebCaseItem::ModuleReference(t.0),
    )(input)
}

fn parse_other_cases_item<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebCaseItem<'a>> {
    map(
        tuple((
            parse_other_cases_tag,
            parse_statement_base,
            opt(pascal_token(PascalToken::Semicolon)),
            opt(comment),
        )),
        |t| {
            WebCaseItem::OtherCases(WebOtherCasesItem {
                tag: t.0,
                stmt: Box::new(t.1),
                comment: t.3,
            })
        },
    )(input)
}

/// `endcases` is a formatted identifier formatted like `Else`
fn parse_other_cases_tag<'a>(input: ParseInput<'a>) -> ParseResult<'a, StringSpan<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(PascalToken::FormattedIdentifier(ss, PascalReservedWord::Else)) = wt {
        Ok((input, ss))
    } else {
        new_parse_err(input, WebErrorKind::Eof)
    }
}

fn parse_standard_case_item<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebCaseItem<'a>> {
    map(
        tuple((
            separated_list1(
                pascal_token(PascalToken::Comma),
                map(parse_case_match_expr, Box::new),
            ),
            pascal_token(PascalToken::Colon),
            parse_statement_base,
            opt(pascal_token(PascalToken::Semicolon)),
            opt(comment),
        )),
        |t| {
            WebCaseItem::Standard(WebStandardCaseItem {
                matches: t.0,
                stmt: Box::new(t.2),
                comment: t.4,
            })
        },
    )(input)
}

// "Special" statements that we need to have for funky WEB structures

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SpecialFreeCase<'a> {
    /// The matched cases.
    matches: Vec<PascalToken<'a>>,

    /// The associated statement.
    stmt: Box<WebStatement<'a>>,
}

fn parse_special_free_case<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    map(
        tuple((
            separated_list1(pascal_token(PascalToken::Comma), merged_string_literals),
            pascal_token(PascalToken::Colon),
            parse_statement_base,
            opt(pascal_token(PascalToken::Semicolon)),
        )),
        |t| {
            WebStatement::SpecialFreeCase(SpecialFreeCase {
                matches: t.0,
                stmt: Box::new(t.2),
            })
        },
    )(input)
}

// Prettification

impl<'a> WebStatement<'a> {
    pub fn measure_horz(&self) -> usize {
        match self {
            WebStatement::Expr(expr, comment) => {
                expr.measure_inline()
                    + comment
                        .as_ref()
                        .map(|c| 1 + c.measure_inline())
                        .unwrap_or(0)
            }

            WebStatement::ModuleReference(name) => prettify::module_reference_measure_inline(name),

            WebStatement::Block(block) => {
                // NOTE: hardcoding block indent size:
                let ws = block
                    .stmts
                    .iter()
                    .map(|s| s.measure_horz() + 4)
                    .max()
                    .unwrap_or(0);
                let wc = block
                    .post_comment
                    .as_ref()
                    .map(|c| c.measure_inline())
                    .unwrap_or(0);
                usize::max(ws, wc)
            }

            WebStatement::Assignment(a) => {
                let wl = a.lhs.measure_inline();
                let wr = a.rhs.measure_inline();
                let wc = a
                    .comment
                    .as_ref()
                    .map(|c| 1 + c.measure_inline())
                    .unwrap_or(0);
                wl + wr + wc + 3 // " = "
            }

            _ => {
                eprintln!("SMH: {:?}", self);
                1
            }
        }
    }

    pub fn render_horz(&self, dest: &mut Prettifier) {
        match self {
            WebStatement::Expr(expr, comment) => {
                expr.render_inline(dest);

                // todo: only if this expr statement isn't a TeX inline
                //dest.noscope_push(";");

                if let Some(c) = comment.as_ref() {
                    dest.space();
                    c.render_inline(dest);
                }
            }

            WebStatement::ModuleReference(name) => {
                prettify::module_reference_render(name, dest);
            }

            WebStatement::PreprocessorDirective(pd) => {
                pd.prettify(dest);
            }

            WebStatement::Block(block) => {
                // NOTE: in many cases, some kind of outer construct (e.g. `if`
                // statement) should special-case standard blocks and avoid this
                // codepath.
                if !block.opener.is_reserved_word(PascalReservedWord::Begin) {
                    block.opener.render_inline(dest);
                    dest.noscope_push("!");
                }

                dest.noscope_push("{");
                dest.indent_block();
                dest.newline_indent();

                for s in &block.stmts {
                    s.render_horz(dest);
                    dest.newline_needed();
                }

                if let Some(c) = block.post_comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                dest.dedent_block();
                dest.noscope_push("}");
                dest.newline_needed();
            }

            WebStatement::Assignment(a) => {
                a.lhs.render_inline(dest);
                dest.noscope_push(" = ");
                a.rhs.render_inline(dest);
                dest.noscope_push(";");

                if let Some(c) = a.comment.as_ref() {
                    dest.space();
                    c.render_inline(dest);
                    dest.newline_needed();
                }
            }

            _ => {
                eprintln!("SRH: {:?}", self);
            }
        }
    }

    pub fn render_flex(&self, dest: &mut Prettifier) {
        self.render_horz(dest)
    }
}
