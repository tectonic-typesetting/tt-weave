//! A WEB statement.

use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many0, many1, separated_list1},
    sequence::tuple,
};
use std::{borrow::Cow, ops::Deref};

use crate::prettify::{self, Prettifier, RenderInline};

use super::{
    base::*,
    expr::{parse_case_match_expr, parse_expr, parse_lhs_expr, WebExpr},
    module_reference::parse_module_reference,
    preprocessor_directive, WebToplevel,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebStatement<'a> {
    /// A reference to a module.
    ModuleReference(WebModuleReference<'a>, Option<WebComment<'a>>),

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
        debug("BB", parse_block),
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
        tuple((
            parse_module_reference,
            opt(pascal_token(PascalToken::Semicolon)),
            opt(comment),
        )),
        |t| WebStatement::ModuleReference(t.0, t.2),
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

    /// Optional comment before
    pre_comment: Option<WebComment<'a>>,

    /// Inner statements.
    stmts: Vec<Box<WebStatement<'a>>>,

    /// The token that closes the block.
    closer: PascalToken<'a>,

    /// Optional comment after
    post_comment: Option<WebComment<'a>>,
}

/// The early optional semicolon is for XeTeX(2022.0):571, near the
/// `wlog("entering extended mode")`.
fn parse_block<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    let (input, items) = tuple((
        block_opener,
        opt(pascal_token(PascalToken::Semicolon)),
        opt(comment),
        many0(map(parse_statement_base, |s| Box::new(s))),
        block_closer,
        opt(pascal_token(PascalToken::Semicolon)),
        opt(pascal_token(PascalToken::Period)), // for the very end of program
        opt(comment),
    ))(input)?;

    let opener = items.0;
    let pre_comment = items.2;
    let stmts = items.3;
    let closer = items.4;
    let post_comment = items.7;

    Ok((
        input,
        WebStatement::Block(WebBlock {
            opener,
            pre_comment,
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
    /// Optional comment before the `if`
    opening_comment: Option<WebComment<'a>>,

    /// The test expression
    test: Box<WebExpr<'a>>,

    /// Optional comment after the test
    test_comment: Option<WebComment<'a>>,

    /// The `then` statement, which may be a block.
    then: Box<WebStatement<'a>>,

    /// The optional `else` statement, which may be a block, or may be another
    /// `if` statement.
    else_: Option<Box<WebStatement<'a>>>,

    /// Optional comment associated with the else block.
    else_comment: Option<WebComment<'a>>,
}

fn parse_if<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    let (input, items) = tuple((
        opt(comment),
        reserved_word(PascalReservedWord::If),
        parse_expr,
        reserved_word(PascalReservedWord::Then),
        opt(comment),
        parse_statement_base,
        opt(tuple((
            reserved_word(PascalReservedWord::Else),
            parse_statement_base,
        ))),
        opt(comment),
    ))(input)?;

    let opening_comment = items.0;
    let test = Box::new(items.2);
    let test_comment = items.4;
    let then = Box::new(items.5);
    let else_ = items.6.map(|t| Box::new(t.1));
    let else_comment = items.7;

    Ok((
        input,
        WebStatement::If(WebIf {
            opening_comment,
            test,
            test_comment,
            then,
            else_,
            else_comment,
        }),
    ))
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebWhile<'a> {
    /// The loop test expression
    test: Box<WebExpr<'a>>,

    /// Optional comment after the test
    test_comment: Option<WebComment<'a>>,

    /// The `do` statement, which may be a block.
    do_: Box<WebStatement<'a>>,
}

fn parse_while<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::While),
        parse_expr,
        reserved_word(PascalReservedWord::Do),
        opt(comment),
        parse_statement_base,
    ))(input)?;

    let test = Box::new(items.1);
    let test_comment = items.3;
    let do_ = Box::new(items.4);

    Ok((
        input,
        WebStatement::While(WebWhile {
            test,
            test_comment,
            do_,
        }),
    ))
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

    /// An optional comment for the loop top.
    top_comment: Option<WebComment<'a>>,

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
        opt(comment),
        parse_statement_base,
    ))(input)?;

    let var = items.1;
    let start = Box::new(items.3);
    let is_down = items.4;
    let end = Box::new(items.5);
    let top_comment = items.7;
    let do_ = Box::new(items.8);

    Ok((
        input,
        WebStatement::For(WebFor {
            var,
            start,
            is_down,
            end,
            top_comment,
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

    /// Optional comment at end of loop.
    closing_comment: Option<WebComment<'a>>,
}

fn parse_repeat<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    map(
        tuple((
            reserved_word(PascalReservedWord::Repeat),
            many1(map(parse_statement_base, |s| Box::new(s))),
            reserved_word(PascalReservedWord::Until),
            parse_expr,
            opt(pascal_token(PascalToken::Semicolon)),
            opt(comment),
        )),
        |t| {
            WebStatement::Repeat(WebRepeat {
                test: Box::new(t.3),
                stmts: t.1,
                closing_comment: t.5,
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

    /// Optional final comment.
    comment: Option<WebComment<'a>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebCaseItem<'a> {
    ModuleReference(WebModuleReference<'a>),
    Standard(WebStandardCaseItem<'a>),
    OtherCases(WebOtherCasesItem<'a>),

    /// A standard-looking item encased in an ifdef-like construct, needed for
    /// XeTeX(2022.0):88.
    IfdefStandard(PascalToken<'a>, WebStandardCaseItem<'a>, PascalToken<'a>),

    /// A standard-ish item where the match is a WEB module. Needed for
    /// XeTeX(2022.0):374.
    ModMatch(WebModMatchCaseItem<'a>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebStandardCaseItem<'a> {
    /// The matched cases. These may be identifiers, string literals,
    /// integer literals, or WEB macros that look like function calls.
    matches: Vec<Box<WebExpr<'a>>>,

    /// The associated statement.
    stmt: Box<WebStatement<'a>>,

    /// Optional comment.
    comment: Option<WebComment<'a>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebModMatchCaseItem<'a> {
    match_: WebModuleReference<'a>,

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
                parse_mod_match_case_item,
                parse_mod_ref_case_item,
                parse_other_cases_item,
                parse_standard_case_item,
                parse_ifdef_case_item,
            ))),
            parse_case_terminator,
            opt(pascal_token(PascalToken::Semicolon)),
            opt(comment),
        )),
        |t| {
            WebStatement::Case(WebCase {
                var: t.1,
                items: t.3,
                comment: t.6,
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
        tuple((
            parse_module_reference,
            opt(pascal_token(PascalToken::Semicolon)),
        )),
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

/// Note that if both comments are present, we'll lose one.
fn parse_standard_case_item_base<'a>(
    input: ParseInput<'a>,
) -> ParseResult<'a, WebStandardCaseItem<'a>> {
    map(
        tuple((
            separated_list1(
                pascal_token(PascalToken::Comma),
                map(parse_case_match_expr, Box::new),
            ),
            pascal_token(PascalToken::Colon),
            opt(comment),
            parse_statement_base,
            opt(pascal_token(PascalToken::Semicolon)),
            opt(comment),
        )),
        |t| WebStandardCaseItem {
            matches: t.0,
            stmt: Box::new(t.3),
            comment: t.2.or(t.5),
        },
    )(input)
}

fn parse_standard_case_item<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebCaseItem<'a>> {
    map(parse_standard_case_item_base, WebCaseItem::Standard)(input)
}

pub fn parse_ifdef_case_item<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebCaseItem<'a>> {
    map(
        tuple((
            formatted_identifier_like(PascalReservedWord::Begin),
            parse_standard_case_item_base,
            formatted_identifier_like(PascalReservedWord::End),
        )),
        |t| WebCaseItem::IfdefStandard(t.0, t.1, t.2),
    )(input)
}

fn parse_mod_match_case_item<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebCaseItem<'a>> {
    map(
        tuple((
            parse_module_reference,
            pascal_token(PascalToken::Colon),
            parse_statement_base,
            opt(pascal_token(PascalToken::Semicolon)),
            opt(comment),
        )),
        |t| {
            WebCaseItem::ModMatch(WebModMatchCaseItem {
                match_: t.0,
                stmt: Box::new(t.2),
                comment: t.4,
            })
        },
    )(input)
}

/// "Special" statements that we need to have for funky WEB structures.
///
/// Note that if the case is an identifier, we can't distinguish between this
/// and a "goto" label. This happens in WEAVE:188.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SpecialFreeCase<'a> {
    /// The matched cases.
    matches: Vec<Box<WebExpr<'a>>>,

    /// The associated statement.
    stmt: Box<WebStatement<'a>>,
}

fn parse_special_free_case<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    map(
        tuple((
            separated_list1(
                pascal_token(PascalToken::Comma),
                map(parse_case_match_expr, Box::new),
            ),
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

impl<'a> RenderInline for WebStatement<'a> {
    fn measure_inline(&self) -> usize {
        match self {
            WebStatement::Block(_)
            | WebStatement::If(_)
            | WebStatement::Case(_)
            | WebStatement::SpecialFreeCase(_)
            | WebStatement::While(_)
            | WebStatement::For(_)
            | WebStatement::Repeat(_)
            | WebStatement::Loop(_)
            | WebStatement::PreprocessorDirective(_) => prettify::NOT_INLINE,

            WebStatement::Expr(expr, comment) => {
                expr.measure_inline()
                    + comment
                        .as_ref()
                        .map(|c| c.measure_inline() + 1)
                        .unwrap_or(0)
            }

            WebStatement::ModuleReference(mr, comment) => {
                mr.measure_inline()
                    + comment
                        .as_ref()
                        .map(|c| c.measure_inline() + 1)
                        .unwrap_or(0)
            }

            WebStatement::Assignment(a) => {
                a.lhs.measure_inline()
                    + 3
                    + a.rhs.measure_inline()
                    + a.comment
                        .as_ref()
                        .map(|c| c.measure_inline() + 1)
                        .unwrap_or(0)
            }

            WebStatement::Goto(g) => {
                g.label.len() + 5 + // "goto "
                     g.comment
                        .as_ref()
                        .map(|c| c.measure_inline() + 1)
                        .unwrap_or(0)
            }

            WebStatement::Label(l) => l.len() + 1,
        }
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        match self {
            WebStatement::Block(_)
            | WebStatement::If(_)
            | WebStatement::Case(_)
            | WebStatement::SpecialFreeCase(_)
            | WebStatement::While(_)
            | WebStatement::For(_)
            | WebStatement::Repeat(_)
            | WebStatement::Loop(_)
            | WebStatement::PreprocessorDirective(_) => dest.noscope_push("XXX-stmt-inline"),

            WebStatement::Expr(expr, comment) => {
                expr.render_inline(dest);

                if let Some(c) = comment {
                    dest.space();
                    c.render_inline(dest);
                }
            }

            WebStatement::ModuleReference(mr, comment) => {
                mr.render_inline(dest);

                if let Some(c) = comment {
                    dest.space();
                    c.render_inline(dest);
                }
            }

            WebStatement::Assignment(a) => {
                a.lhs.render_inline(dest);
                dest.noscope_push(" = ");
                a.rhs.render_inline(dest);

                if let Some(c) = a.comment.as_ref() {
                    dest.space();
                    c.render_inline(dest);
                }
            }

            WebStatement::Goto(g) => {
                dest.keyword("goto");
                dest.space();
                dest.scope_push(*prettify::LABEL_NAME_SCOPE, &g.label);

                if let Some(c) = g.comment.as_ref() {
                    dest.space();
                    c.render_inline(dest);
                }
            }

            WebStatement::Label(l) => {
                dest.scope_push(*prettify::LABEL_NAME_SCOPE, l);
                dest.noscope_push(':');
            }
        }
    }
}

impl<'a> WebStatement<'a> {
    fn wants_semicolon(&self) -> bool {
        match self {
            WebStatement::Block(_)
            | WebStatement::ModuleReference(..)
            | WebStatement::Label(_)
            | WebStatement::If(_)
            | WebStatement::Case(_)
            | WebStatement::SpecialFreeCase(_)
            | WebStatement::While(_)
            | WebStatement::For(_)
            | WebStatement::Loop(_) => false,

            WebStatement::PreprocessorDirective(_)
            | WebStatement::Expr(..)
            | WebStatement::Assignment(_)
            | WebStatement::Repeat(_)
            | WebStatement::Goto(_) => true,
        }
    }

    pub fn maybe_semicolon(&self, dest: &mut Prettifier) {
        if self.wants_semicolon() {
            dest.noscope_push(';')
        }
    }

    /// Render the statement given the knowledge that it is already wrapped in a
    /// block structure. All statements are rendered normally except for Blocks
    /// that don't have an unusual guard.
    pub fn render_in_block(&self, dest: &mut Prettifier) {
        if let WebStatement::Block(block) = self {
            if block.opener.is_reserved_word(PascalReservedWord::Begin) {
                if let Some(c) = block.pre_comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                for s in &block.stmts {
                    s.render_flex(dest);
                    s.maybe_semicolon(dest);
                    dest.newline_needed();
                }

                if let Some(c) = block.post_comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                return;
            }
        }

        self.render_flex(dest);
        self.maybe_semicolon(dest);
    }

    pub fn render_flex(&self, dest: &mut Prettifier) {
        match self {
            WebStatement::Expr(expr, comment) => {
                if let Some(c) = comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                expr.render_flex(dest);
            }

            WebStatement::ModuleReference(mr, comment) => {
                if let Some(c) = comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                mr.render_inline(dest);
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

                if let Some(c) = block.pre_comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                for s in &block.stmts {
                    s.render_flex(dest);
                    s.maybe_semicolon(dest);
                    dest.newline_needed();
                }

                if let Some(c) = block.post_comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                dest.dedent_block();
                dest.noscope_push("}");
            }

            WebStatement::Assignment(a) => {
                if let Some(c) = a.comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                a.lhs.render_flex(dest);
                dest.noscope_push(" = ");
                a.rhs.render_flex(dest);
            }

            WebStatement::Goto(g) => {
                if let Some(c) = g.comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                dest.keyword("goto");
                dest.space();
                dest.noscope_push(&g.label);
            }

            WebStatement::Label(l) => {
                let dented = dest.dedent_small();
                dest.newline_needed();
                dest.noscope_push(&l);
                dest.noscope_push(':');

                if dented {
                    dest.indent_small();
                }

                dest.newline_needed();
            }

            WebStatement::If(i) => {
                let did_opening = if let Some(c) = i.opening_comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                    true
                } else {
                    if let Some(c) = i.test_comment.as_ref() {
                        c.render_inline(dest);
                        dest.newline_needed();
                    }

                    false
                };

                dest.keyword("if");
                dest.noscope_push(" (");
                i.test.render_flex(dest);
                dest.noscope_push(") {");
                dest.indent_block();

                if did_opening {
                    if let Some(c) = i.test_comment.as_ref() {
                        c.render_inline(dest);
                        dest.newline_needed();
                    }
                }

                dest.newline_needed();
                i.then.render_in_block(dest);
                dest.dedent_block();
                dest.newline_needed();
                dest.noscope_push("}");

                if let Some(e) = &i.else_ {
                    // Make `else if` inline for prettiness
                    if let WebStatement::If(_) = e.deref() {
                        dest.space();
                        dest.keyword("else");
                        dest.space();
                        e.render_flex(dest);
                    } else {
                        dest.space();
                        dest.keyword("else");
                        dest.noscope_push(" {");
                        dest.indent_block();
                        dest.newline_needed();

                        if let Some(c) = i.else_comment.as_ref() {
                            c.render_inline(dest);
                            dest.newline_needed();
                        }

                        e.render_in_block(dest);
                        dest.dedent_block();
                        dest.newline_needed();
                        dest.noscope_push("}");
                    }
                }
            }

            WebStatement::While(w) => {
                if let Some(c) = w.test_comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                dest.keyword("while");
                dest.noscope_push(" (");
                w.test.render_flex(dest);
                dest.noscope_push(") {");
                dest.indent_block();
                dest.newline_needed();
                w.do_.render_in_block(dest);
                dest.dedent_block();
                dest.newline_needed();
                dest.noscope_push("}");
            }

            WebStatement::For(f) => {
                if let Some(c) = f.top_comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                dest.keyword("for");
                dest.noscope_push(" (");
                dest.noscope_push(&f.var);
                dest.space();
                dest.keyword("in");
                dest.space();
                f.start.render_flex(dest);

                if f.is_down {
                    dest.space();
                    dest.keyword("downto");
                    dest.space();
                } else {
                    dest.space();
                    dest.keyword("to");
                    dest.space();
                }

                f.end.render_flex(dest);
                dest.noscope_push(") {");
                dest.indent_block();
                dest.newline_needed();
                f.do_.render_in_block(dest);
                dest.dedent_block();
                dest.newline_needed();
                dest.noscope_push("}");
            }

            WebStatement::Repeat(r) => {
                dest.keyword("repeat");
                dest.noscope_push(" {");
                dest.indent_block();

                for s in &r.stmts {
                    dest.newline_needed();
                    s.render_flex(dest);
                    s.maybe_semicolon(dest);
                }

                if let Some(c) = r.closing_comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                dest.dedent_block();
                dest.newline_needed();
                dest.noscope_push("} ");
                dest.keyword("until");
                dest.noscope_push(" (");
                r.test.render_flex(dest);
                dest.noscope_push(')');
            }

            WebStatement::Loop(l) => {
                dest.keyword(&l.keyword);
                dest.noscope_push(" {");
                dest.indent_block();
                dest.newline_needed();
                l.do_.render_in_block(dest);
                dest.dedent_block();
                dest.newline_needed();
                dest.noscope_push("}");
            }

            WebStatement::Case(c) => {
                dest.keyword("case");
                dest.noscope_push(" ");
                c.var.render_flex(dest);
                dest.noscope_push(" {");
                dest.indent_small();

                for item in &c.items {
                    dest.newline_needed();
                    item.render_flex(dest);
                }

                if let Some(c) = c.comment.as_ref() {
                    c.render_inline(dest);
                    dest.newline_needed();
                }

                dest.dedent_small();
                dest.newline_needed();
                dest.noscope_push("}");
            }

            WebStatement::SpecialFreeCase(sfc) => {
                let wm = prettify::measure_inline_seq(&sfc.matches, 2) + 1;

                if dest.fits(wm) {
                    prettify::render_inline_seq(&sfc.matches, ", ", dest);
                } else {
                    let i_last = sfc.matches.len() - 1;

                    for (i, tok) in sfc.matches.iter().enumerate() {
                        dest.newline_needed();
                        tok.render_inline(dest);

                        if i != i_last {
                            dest.noscope_push(',');
                        }
                    }
                }

                dest.noscope_push(':');
                dest.indent_small();
                dest.newline_indent();
                sfc.stmt.render_in_block(dest);
                sfc.stmt.maybe_semicolon(dest);
                dest.dedent_small();
            }
        }
    }
}

impl<'a> WebCaseItem<'a> {
    fn render_flex(&self, dest: &mut Prettifier) {
        match self {
            WebCaseItem::ModuleReference(mr) => mr.render_inline(dest),

            WebCaseItem::Standard(sc) => sc.render_flex(dest),

            WebCaseItem::OtherCases(oc) => {
                if let Some(c) = oc.comment.as_ref() {
                    // align with the full indent
                    dest.noscope_push("  ");
                    c.render_inline(dest);
                    dest.newline_indent();
                }

                dest.noscope_push(&oc.tag);
                dest.noscope_push(':');
                dest.indent_small();
                dest.newline_indent();
                oc.stmt.render_in_block(dest);
                dest.dedent_small();
            }

            WebCaseItem::IfdefStandard(beg, sci, _end) => {
                beg.render_inline(dest);
                dest.noscope_push("!{");
                dest.indent_block();
                dest.newline_indent();
                sci.render_flex(dest);
                dest.dedent_block();
                dest.newline_indent();
                dest.noscope_push('}');
            }

            WebCaseItem::ModMatch(mmc) => mmc.render_flex(dest),
        }
    }
}

impl<'a> WebStandardCaseItem<'a> {
    fn render_flex(&self, dest: &mut Prettifier) {
        if let Some(c) = self.comment.as_ref() {
            // align with the full indent
            dest.noscope_push("  ");
            c.render_inline(dest);
            dest.newline_indent();
        }

        let wm = prettify::measure_inline_seq(&self.matches, 2) + 1;

        if dest.fits(wm) {
            prettify::render_inline_seq(&self.matches, ", ", dest);
        } else {
            let i_last = self.matches.len() - 1;

            for (i, expr) in self.matches.iter().enumerate() {
                dest.newline_needed();
                expr.render_flex(dest);

                if i != i_last {
                    dest.noscope_push(',');
                }
            }
        }

        dest.noscope_push(':');
        dest.indent_small();
        dest.newline_indent();
        self.stmt.render_in_block(dest);
        dest.dedent_small();
    }
}

impl<'a> WebModMatchCaseItem<'a> {
    fn render_flex(&self, dest: &mut Prettifier) {
        if let Some(c) = self.comment.as_ref() {
            // align with the full indent
            dest.noscope_push("  ");
            c.render_inline(dest);
            dest.newline_indent();
        }

        self.match_.render_inline(dest);
        dest.noscope_push(':');
        dest.indent_small();
        dest.newline_indent();
        self.stmt.render_in_block(dest);
        dest.dedent_small();
    }
}
