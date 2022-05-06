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

use crate::prettify::{self, Prettifier, RenderInline, COMMENT_SCOPE};

use super::{
    base::*,
    expr::{parse_expr, WebExpr},
    statement::{self, WebStatement},
    webtype::{parse_type, WebType},
    WebToplevel,
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
            if let WebStatement::Expr(_, ref mut sc) = &mut stmts[0] {
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

    Statements(Vec<WebStatement<'a>>),

    /// A comma-separated group of exprs, needed for WEAVE#95.
    CommaExprs(Vec<Box<WebExpr<'a>>>),

    /// A series of statements, then an imbalanced `end` keyword. Needed for
    /// WEAVE#125, WEAVE#148.
    StatementsThenEnd(Vec<WebStatement<'a>>),

    /// An imbalanced `begin` keyword, then a series of statements. Needed
    /// for WEAVE#148.
    BeginThenStatements(Vec<WebStatement<'a>>),

    /// A synthesized identifier, needed for XeTeX(2022.0)#4
    SynthesizedIdentifier(Vec<StringSpan<'a>>),

    /// An ifdef-like construct and an incomplete `if`, needed for
    /// XeTeX(2022.0)#8. The token is the ifdef-like and the string span is the
    /// identifier in the `if` statement.
    IfdefAndIf(PascalToken<'a>, StringSpan<'a>),

    /// The closing dual for IfdefAndIf — an `end` and a closing ifdef-like.
    EndAndEndif(PascalToken<'a>),

    /// A statement ending with `.0` because it involves floating point
    /// literals. Big old hack for a couple of forms appearing in
    /// XeTeX(2022.0):113. The token is the trailing int-literal token that is
    /// really the fractional part of a float literal.
    FloatyStatement(WebStatement<'a>, PascalToken<'a>),

    /// Super-specialized for XeTeX(2022.0):589: there are a variety of forms
    /// like `$expr [ $expr {$verbatim{(} $ident,$ident,$ident}?`.
    XetexCharInfoHead(SpecialXetexCharInfoHead<'a>),

    /// Super-specialized for XeTeX(2022.0):589: there are a variety of forms
    /// like `$expr $verbatim{)}? ] . $expr`.
    XetexCharInfoTail(SpecialXetexCharInfoTail<'a>),

    /// Super-specialized for XeTeX(2022.0):742: the head portion of a math
    /// font accessor macro.
    XetexMathAccessorHead(SpecialXetexMathAccessorHead<'a>),

    /// Super-specialized for XeTeX(2022.0):742: the "body" portion of a math
    /// font accessor macro.
    XetexMathAccessorBody(SpecialXetexMathAccessorBody<'a>),
}

fn parse_define_rhs<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    alt((
        parse_ifdef_like,
        parse_loop_definition,
        map(peek_end_of_define, |_| WebDefineRhs::EmptyDefinition),
        parse_othercases,
        parse_floaty_statement,
        parse_statement_series,
        parse_ifdef_and_if,
        parse_end_and_endif,
        parse_begin_then_statements,
        parse_comma_exprs,
        parse_synthesized_identifier,
        parse_xetex_char_info_head,
        parse_xetex_char_info_tail,
        parse_xetex_math_accessor_head,
        parse_xetex_math_accessor_body,
        map(any_reserved_word, |rw| WebDefineRhs::ReservedWord(rw)),
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
pub fn peek_end_of_define<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
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

/// Parse a "floaty" statement (XeTeX(2022.0):113).
fn parse_floaty_statement<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    map(
        tuple((
            statement::parse_statement_base,
            pascal_token(PascalToken::Period),
            int_literal,
        )),
        |t| WebDefineRhs::FloatyStatement(t.0, t.2),
    )(input)
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

fn parse_ifdef_and_if<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    map(
        tuple((
            formatted_identifier_like(PascalReservedWord::Begin),
            reserved_word(PascalReservedWord::If),
            identifier,
            reserved_word(PascalReservedWord::Then),
            reserved_word(PascalReservedWord::Begin),
            peek_end_of_define,
        )),
        |t| WebDefineRhs::IfdefAndIf(t.0, t.2),
    )(input)
}

fn parse_end_and_endif<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    map(
        tuple((
            reserved_word(PascalReservedWord::End),
            pascal_token(PascalToken::Semicolon),
            formatted_identifier_like(PascalReservedWord::End),
        )),
        |t| WebDefineRhs::EndAndEndif(t.1),
    )(input)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SpecialXetexCharInfoHead<'a> {
    start: Box<WebExpr<'a>>,
    middle: Box<WebExpr<'a>>,
    tail_args: Vec<StringSpan<'a>>,
}

fn parse_xetex_char_info_head<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    map(
        tuple((
            parse_expr,
            pascal_token(PascalToken::OpenDelimiter(DelimiterKind::SquareBracket)),
            parse_expr,
            opt(tuple((
                verbatim_open_paren,
                separated_list1(pascal_token(PascalToken::Comma), identifier),
            ))),
        )),
        |t| {
            WebDefineRhs::XetexCharInfoHead(SpecialXetexCharInfoHead {
                start: Box::new(t.0),
                middle: Box::new(t.2),
                tail_args: t.3.map(|tt| tt.1).unwrap_or_default(),
            })
        },
    )(input)
}

fn verbatim_open_paren<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    let (input, tok) = verbatim_pascal(input)?;

    if let PascalToken::VerbatimPascal(ss) = tok {
        if ss.value.as_ref() == "(" {
            return Ok((input, ()));
        }
    }

    new_parse_err(input, WebErrorKind::Eof)
}

impl<'a> RenderInline for SpecialXetexCharInfoHead<'a> {
    fn measure_inline(&self) -> usize {
        let wp = if self.tail_args.is_empty() {
            0
        } else {
            // measure "item, item, item, ", then lose two chars for the
            // excessive final element, then gain one for the verbatim
            // parenthesis.
            self.tail_args.iter().map(|ss| ss.len() + 2).sum::<usize>() - 1
        };

        self.start.measure_inline() + 1 + self.middle.measure_inline() + wp
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        self.start.render_inline(dest);
        dest.noscope_push("[");
        self.middle.render_inline(dest);

        if !self.tail_args.is_empty() {
            dest.noscope_push('(');

            let mut first = true;

            for arg in &self.tail_args {
                if first {
                    first = false;
                } else {
                    dest.noscope_push(", ");
                }

                dest.noscope_push(arg);
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SpecialXetexCharInfoTail<'a> {
    start: Box<WebExpr<'a>>,
    has_right_paren: bool,
    end: Box<WebExpr<'a>>,
}

fn parse_xetex_char_info_tail<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    let (input, t) = tuple((
        parse_expr,
        opt(verbatim_pascal),
        pascal_token(PascalToken::CloseDelimiter(DelimiterKind::SquareBracket)),
        pascal_token(PascalToken::Period),
        parse_expr,
        peek_end_of_define,
    ))(input)?;

    if let Some(PascalToken::VerbatimPascal(ss)) = t.1.as_ref() {
        if ss.value.as_ref() != ")" {
            return new_parse_err(input, WebErrorKind::Eof);
        }
    }

    Ok((
        input,
        WebDefineRhs::XetexCharInfoTail(SpecialXetexCharInfoTail {
            start: Box::new(t.0),
            has_right_paren: t.1.is_some(),
            end: Box::new(t.4),
        }),
    ))
}

impl<'a> RenderInline for SpecialXetexCharInfoTail<'a> {
    fn measure_inline(&self) -> usize {
        let wp = if self.has_right_paren { 1 } else { 0 };
        self.start.measure_inline() + wp + 2 + self.end.measure_inline()
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        self.start.render_inline(dest);

        if self.has_right_paren {
            dest.noscope_push(')');
        }

        dest.noscope_push("].");
        self.end.render_inline(dest);
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SpecialXetexMathAccessorHead<'a> {
    arg: Option<(StringSpan<'a>, StringSpan<'a>)>,
    ret_type: WebType<'a>,
    body: StringSpan<'a>,
}

fn parse_xetex_math_accessor_head<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    map(
        tuple((
            reserved_word(PascalReservedWord::Function),
            hash_token,
            opt(map(
                tuple((
                    pascal_token(PascalToken::OpenDelimiter(DelimiterKind::Paren)),
                    identifier,
                    pascal_token(PascalToken::Colon),
                    identifier,
                    pascal_token(PascalToken::CloseDelimiter(DelimiterKind::Paren)),
                )),
                |t| (t.1, t.3),
            )),
            pascal_token(PascalToken::Colon),
            parse_type,
            pascal_token(PascalToken::Semicolon),
            identifier,
            peek_end_of_define,
        )),
        |t| {
            WebDefineRhs::XetexMathAccessorHead(SpecialXetexMathAccessorHead {
                arg: t.2,
                ret_type: t.4,
                body: t.6,
            })
        },
    )(input)
}

fn hash_token<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    let (input, tok) = next_token(input)?;

    if let WebToken::Pascal(PascalToken::Hash(_)) = tok {
        Ok((input, ()))
    } else {
        new_parse_err(input, WebErrorKind::Eof)
    }
}

impl<'a> SpecialXetexMathAccessorHead<'a> {
    fn prettify(&self, dest: &mut Prettifier) {
        dest.keyword("function");
        dest.space();
        dest.noscope_push("#(");

        if let Some((name, ty)) = self.arg.as_ref() {
            dest.noscope_push(name);
            dest.noscope_push(": ");
            dest.noscope_push(ty);
        }

        dest.noscope_push("): ");
        self.ret_type.render_inline(dest);
        dest.noscope_push(" {");
        dest.indent_block();
        dest.newline_needed();
        dest.noscope_push(&self.body);
        dest.dedent_block();
        dest.newline_needed();
        dest.scope_push(*COMMENT_SCOPE, "/* ... continued later ... */");
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SpecialXetexMathAccessorBody<'a> {
    args: Vec<super::var_declaration::WebVarDeclaration<'a>>,
    body: Vec<WebStatement<'a>>,
}

fn parse_xetex_math_accessor_body<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebDefineRhs<'a>> {
    map(
        tuple((
            reserved_word(PascalReservedWord::Var),
            many1(debug(
                "MAB",
                super::var_declaration::parse_var_declaration_base,
            )),
            reserved_word(PascalReservedWord::Begin),
            many1(statement::parse_statement_base),
            peek_end_of_define,
        )),
        |t| {
            WebDefineRhs::XetexMathAccessorBody(SpecialXetexMathAccessorBody {
                args: t.1,
                body: t.3,
            })
        },
    )(input)
}

impl<'a> SpecialXetexMathAccessorBody<'a> {
    fn prettify(&self, dest: &mut Prettifier) {
        dest.keyword("var");
        dest.indent_small();

        for vd in &self.args {
            dest.newline_needed();
            vd.prettify(dest);
        }

        dest.newline_indent();

        for stmt in &self.body {
            dest.newline_needed();
            stmt.render_flex(dest);
            stmt.maybe_semicolon(dest);
        }
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
            dest.keyword("@define");
            dest.space();

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

            dest.keyword("@define");
            dest.space();

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

            dest.keyword("@define");
            dest.space();

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
            WebDefineRhs::StatementsThenEnd(_)
            | WebDefineRhs::BeginThenStatements(_)
            | WebDefineRhs::IfdefAndIf(..)
            | WebDefineRhs::EndAndEndif(_)
            | WebDefineRhs::XetexMathAccessorHead(_)
            | WebDefineRhs::XetexMathAccessorBody(_) => prettify::NOT_INLINE,

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
                    stmts[0].measure_inline()
                }
            }

            WebDefineRhs::CommaExprs(exprs) => prettify::measure_inline_seq(exprs, 2),
            WebDefineRhs::SynthesizedIdentifier(pieces) => {
                pieces.iter().map(|p| p.len()).sum::<usize>() + 20
            }
            WebDefineRhs::FloatyStatement(stmt, n) => {
                stmt.measure_inline() + 1 + n.measure_inline()
            }
            WebDefineRhs::XetexCharInfoHead(cih) => cih.measure_inline(),
            WebDefineRhs::XetexCharInfoTail(cit) => cit.measure_inline(),
        }
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        match self {
            WebDefineRhs::StatementsThenEnd(_)
            | WebDefineRhs::BeginThenStatements(_)
            | WebDefineRhs::IfdefAndIf(..)
            | WebDefineRhs::EndAndEndif(_)
            | WebDefineRhs::XetexMathAccessorHead(_)
            | WebDefineRhs::XetexMathAccessorBody(_) => dest.noscope_push("XXXrhs"),

            WebDefineRhs::ReservedWord(s) => dest.noscope_push(s),

            WebDefineRhs::IfdefLike(is_opener) => dest.noscope_push(if *is_opener {
                "begin_ignore!"
            } else {
                "end_ignore!"
            }),

            WebDefineRhs::LoopDefinition(t) => {
                dest.keyword("while");
                dest.space();
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
                    s.render_inline(dest);
                }
            }

            WebDefineRhs::CommaExprs(exprs) => prettify::render_inline_seq(exprs, ", ", dest),

            WebDefineRhs::SynthesizedIdentifier(pieces) => {
                dest.noscope_push("synthesized_ident!(");
                for p in pieces {
                    dest.noscope_push(p);
                }
                dest.noscope_push(")")
            }

            WebDefineRhs::FloatyStatement(stmt, n) => {
                stmt.render_inline(dest);
                dest.noscope_push(".");
                n.render_inline(dest);
            }

            WebDefineRhs::XetexCharInfoHead(cih) => cih.render_inline(dest),
            WebDefineRhs::XetexCharInfoTail(cit) => cit.render_inline(dest),
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
        | WebDefineRhs::SynthesizedIdentifier(_)
        | WebDefineRhs::FloatyStatement(..)
        | WebDefineRhs::XetexCharInfoHead(_)
        | WebDefineRhs::XetexCharInfoTail(_) => {
            rhs.render_inline(dest);
        }

        WebDefineRhs::Statements(stmts) => {
            let i_last = stmts.len() - 1;

            for (i, s) in stmts.iter().enumerate() {
                s.render_flex(dest);
                if i != i_last {
                    s.maybe_semicolon(dest);
                }
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
            dest.noscope_push("/*... opened earlier ...*/");
            dest.indent_block();

            for s in stmts {
                dest.newline_indent();
                s.render_flex(dest);
                s.maybe_semicolon(dest);
            }

            dest.dedent_block();
            dest.newline_indent();
            dest.noscope_push("}");
        }

        WebDefineRhs::BeginThenStatements(stmts) => {
            dest.noscope_push("{");
            dest.indent_block();

            let i_last = stmts.len() - 1;

            for (i, s) in stmts.iter().enumerate() {
                dest.newline_indent();
                s.render_flex(dest);

                if i != i_last {
                    s.maybe_semicolon(dest);
                }
            }

            dest.dedent_block();
            dest.newline_indent();
            dest.noscope_push("/* ... closed later ... */");
        }

        WebDefineRhs::IfdefAndIf(beg, ident) => {
            beg.render_inline(dest);
            dest.noscope_push("!{");
            dest.indent_block();
            dest.newline_indent();
            dest.keyword("if");
            dest.space();
            dest.noscope_push(ident);
            dest.space();
            dest.noscope_push("{");
        }

        WebDefineRhs::EndAndEndif(end) => {
            dest.indent_block();
            dest.noscope_push("}");
            dest.dedent_block();
            dest.newline_indent();
            end.render_inline(dest);
        }

        WebDefineRhs::XetexMathAccessorHead(mah) => mah.prettify(dest),
        WebDefineRhs::XetexMathAccessorBody(mab) => mab.prettify(dest),
    }
}
