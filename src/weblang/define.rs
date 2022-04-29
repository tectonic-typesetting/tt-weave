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

use crate::prettify::{self, Prettifier, RenderInline};

use super::{
    base::*,
    expr::{parse_expr, WebExpr},
    statement, WebToplevel,
};

/// A `@d` definition
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebDefine<'a> {
    /// The LHS of the define. This may be a sequence of tokens like `blah(#)`.
    lhs: Vec<PascalToken<'a>>,

    /// The right hand side.
    rhs: WebDefineRhs<'a>,

    /// Optional trailing comment.
    comment: Option<WebComment<'a>>,
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
    let mut rhs = items.3 .0;
    let mut comment = items.3 .1;

    // Tidying up for a common case:

    if let WebDefineRhs::Statements(ref mut stmts) = &mut rhs {
        if stmts.len() == 1 && comment.is_none() {
            if let statement::WebStatement::Expr(_, ref mut sc) = &mut stmts[0] {
                comment = sc.take();
            }
        }
    }

    Ok((input, WebToplevel::Define(WebDefine { lhs, rhs, comment })))
}

/// The right-hand-side of a `@d` definition
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebDefineRhs<'a> {
    ReservedWord(SpanValue<'a, PascalReservedWord>),

    /// The boolean specifies whether it's an opener (true) or closer
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

    /// A series of statements, then an imbalanced `end` keyword. Needed for
    /// WEAVE#125, WEAVE#148.
    StatementsThenEnd(Vec<statement::WebStatement<'a>>),

    /// An imbalanced `begin` keyword, then a series of statements. Needed
    /// for WEAVE#148.
    BeginThenStatements(Vec<statement::WebStatement<'a>>),

    /// A synthesized identifier, needed for XeTeX(2022.0)#4
    SynthesizedIdentifier(Vec<StringSpan<'a>>),
}

fn parse_define_rhs<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    alt((
        parse_ifdef_like,
        parse_loop_definition,
        map(peek_end_of_define, |_| WebDefineRhs::EmptyDefinition),
        parse_othercases,
        parse_statement_series,
        parse_begin_then_statements,
        parse_comma_exprs,
        map(any_reserved_word, |rw| WebDefineRhs::ReservedWord(rw)),
        parse_synthesized_identifier,
    ))(input)
}

/// There are some constructs rendered as `@d foo = { comment } begin ... end`.
/// We handle fiddle with these to treat them homogeneously with other statements
fn parse_inverted_statement<'a>(
    input: ParseInput<'a>,
) -> ParseResult<'a, (WebDefineRhs<'a>, Option<WebComment<'a>>)> {
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

/// Parse a sequence of statements, potentially followed by an imbalanced `end`
/// keyword.
fn parse_statement_series<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    let (input, tup) = tuple((
        many1(statement::parse_statement_base),
        opt(reserved_word(PascalReservedWord::End)),
        peek_end_of_define,
    ))(input)?;

    if tup.1.is_some() {
        Ok((input, WebDefineRhs::StatementsThenEnd(tup.0)))
    } else {
        Ok((input, WebDefineRhs::Statements(tup.0)))
    }
}

/// Parse an imbalanced `begin` keyword, followed by a series of statements.
fn parse_begin_then_statements<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    map(
        tuple((
            reserved_word(PascalReservedWord::Begin),
            many1(statement::parse_statement_base),
            peek_end_of_define,
        )),
        |t| WebDefineRhs::BeginThenStatements(t.1),
    )(input)
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

fn parse_comma_exprs<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    let (input, exprs) =
        separated_list1(pascal_token(PascalToken::Comma), map(parse_expr, Box::new))(input)?;

    if exprs.len() == 1 {
        return new_parse_err(input, WebErrorKind::Eof);
    } else {
        Ok((input, WebDefineRhs::CommaExprs(exprs)))
    }
}

fn parse_synthesized_identifier<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    let (input, idents) = separated_list1(pascal_token(PascalToken::PasteText), identifier)(input)?;

    if idents.len() == 1 {
        return new_parse_err(input, WebErrorKind::Eof);
    } else {
        Ok((input, WebDefineRhs::SynthesizedIdentifier(idents)))
    }
}

// Prettification

impl<'a> WebDefine<'a> {
    pub fn prettify(&self, dest: &mut Prettifier) {
        let lhs_width: usize = self.lhs.iter().map(|t| t.measure_inline()).sum();
        let rhs_width = self.rhs.measure_inline();
        let c_width = self
            .comment
            .as_ref()
            .map(|c| 1 + c.measure_inline())
            .unwrap_or(0);

        if dest.fits(11 + lhs_width + rhs_width + c_width) {
            dest.noscope_push("@define ");

            for t in &self.lhs {
                dest.noscope_push(t);
            }

            dest.noscope_push(" => ");
            self.rhs.render_inline(dest);

            if let Some(c) = self.comment.as_ref() {
                dest.space();
                c.render_inline(dest);
            }
        } else if dest.fits(11 + lhs_width + rhs_width) {
            // We can't get here without the comment being Some, but ...
            if let Some(c) = self.comment.as_ref() {
                c.render_inline(dest);
                dest.newline_indent();
            }

            dest.noscope_push("@define ");

            for t in &self.lhs {
                dest.noscope_push(t);
            }

            dest.noscope_push(" => ");
            self.rhs.render_inline(dest);
        } else {
            if let Some(c) = self.comment.as_ref() {
                c.render_inline(dest);
                dest.newline_indent();
            }

            dest.noscope_push("@define ");

            for t in &self.lhs {
                dest.noscope_push(t);
            }

            dest.noscope_push(" =>");
            dest.indent_block();
            dest.newline_indent();
            render_rhs_flex(&self.rhs, dest);
        }

        dest.newline_needed();
    }
}

impl<'a> RenderInline for WebDefineRhs<'a> {
    fn measure_inline(&self) -> usize {
        match self {
            WebDefineRhs::ReservedWord(s) => s.value.to_string().len(),

            WebDefineRhs::IfdefLike(_) => 13,
            WebDefineRhs::LoopDefinition(t) => t.len() + 8, // "while  {"
            WebDefineRhs::EmptyDefinition => 12,
            WebDefineRhs::OthercasesDefinition(t) => t.len() + 1,

            WebDefineRhs::Statements(stmts) => {
                if stmts.len() > 1 {
                    // If multi-line, never try to render as inline.
                    prettify::NOT_INLINE
                } else {
                    stmts[0].measure_horz()
                }
            }

            WebDefineRhs::CommaExprs(exprs) => prettify::measure_inline_seq(exprs, 2),
            WebDefineRhs::StatementsThenEnd(_stmts) => prettify::NOT_INLINE,
            WebDefineRhs::BeginThenStatements(_stmts) => prettify::NOT_INLINE,
            WebDefineRhs::SynthesizedIdentifier(pieces) => {
                pieces.iter().map(|p| p.len()).sum::<usize>() + 20
            }
        }
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        match self {
            WebDefineRhs::ReservedWord(s) => dest.noscope_push(s),

            WebDefineRhs::IfdefLike(is_opener) => dest.noscope_push(if *is_opener {
                "begin_ignore!"
            } else {
                "end_ignore!"
            }),

            WebDefineRhs::LoopDefinition(t) => {
                dest.noscope_push("while ");
                dest.noscope_push(t.value.as_ref());
                dest.noscope_push(" {");
            }

            WebDefineRhs::EmptyDefinition => dest.noscope_push("/*nothing*/"),

            WebDefineRhs::OthercasesDefinition(t) => {
                dest.noscope_push(t);
                dest.noscope_push(":");
            }

            WebDefineRhs::Statements(stmts) => {
                // This should only be called if we consist of a single statement
                for s in stmts {
                    s.render_horz(dest);
                }
            }

            WebDefineRhs::CommaExprs(exprs) => prettify::render_inline_seq(exprs, ", ", dest),

            // Should not be rendered inline:
            WebDefineRhs::StatementsThenEnd(_stmts) => dest.noscope_push("XXXstmts-end"),
            WebDefineRhs::BeginThenStatements(_stmts) => dest.noscope_push("XXXbegin-stmts"),

            WebDefineRhs::SynthesizedIdentifier(pieces) => {
                dest.noscope_push("synthesized_ident!(");
                for p in pieces {
                    dest.noscope_push(p);
                }
                dest.noscope_push(")")
            }
        }
    }
}

fn render_rhs_flex<'a>(rhs: &WebDefineRhs<'a>, dest: &mut Prettifier) {
    match rhs {
        WebDefineRhs::ReservedWord(_)
        | WebDefineRhs::IfdefLike(_)
        | WebDefineRhs::LoopDefinition(_)
        | WebDefineRhs::EmptyDefinition
        | WebDefineRhs::OthercasesDefinition(_)
        | WebDefineRhs::SynthesizedIdentifier(_) => {
            rhs.render_inline(dest);
        }

        WebDefineRhs::Statements(stmts) => {
            for s in stmts {
                s.render_flex(dest);
                dest.newline_needed();
            }
        }

        WebDefineRhs::CommaExprs(exprs) => {
            let i_last = exprs.len() - 1;

            for (i, e) in exprs.iter().enumerate() {
                e.render_inline(dest);

                if i != i_last {
                    dest.noscope_push(",");
                }

                dest.newline_needed();
            }
        }

        WebDefineRhs::StatementsThenEnd(stmts) => {
            dest.noscope_push("/*... previous opener ...*/");
            dest.indent_block();

            for s in stmts {
                dest.newline_indent();
                s.render_flex(dest);
            }

            dest.dedent_block();
            dest.newline_indent();
            dest.noscope_push("}");
        }

        WebDefineRhs::BeginThenStatements(stmts) => {
            dest.noscope_push("{");
            dest.indent_block();

            for s in stmts {
                dest.newline_indent();
                s.render_flex(dest);
            }

            dest.dedent_block();
            dest.newline_indent();
            dest.noscope_push("/* ... closed later ... */");
        }
    }
}
