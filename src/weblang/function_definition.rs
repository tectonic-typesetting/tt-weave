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

use crate::prettify::{self, Prettifier, RenderInline};

use super::{
    base::*,
    expr::{parse_expr, WebExpr},
    module_reference::parse_module_reference,
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
    opening_comment: Option<WebComment<'a>>,

    /// Labels
    labels: Vec<WebLabel<'a>>,

    /// Constants (needed for XeTeX(2022.0):744)
    consts: Vec<WebConstant<'a>>,

    /// Records in the function's `var` block.
    vars: Vec<WebVarBlockItem<'a>>,

    /// The statement(s) that comprise the function — almost always a block.
    stmt: WebStatement<'a>,

    /// The comment associated with end of the function.
    closing_comment: Option<WebComment<'a>>,
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
    ModuleReference(WebModuleReference<'a>),

    /// Actual in-place definitions
    InPlace(WebInPlaceVariables<'a>),

    /// In-place definitions guarded by an ifdef-like block
    IfdefInPlace(PascalToken<'a>, WebInPlaceVariables<'a>, PascalToken<'a>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebInPlaceVariables<'a> {
    vars: WebVariables<'a>,
    comment: Option<WebComment<'a>>,
}

fn parse_var_block_item<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebVarBlockItem<'a>> {
    alt((
        map(parse_module_reference, |t| {
            WebVarBlockItem::ModuleReference(t)
        }),
        map(parse_in_place_vars, |v| WebVarBlockItem::InPlace(v)),
        map(
            tuple((
                formatted_identifier_like(PascalReservedWord::Begin),
                parse_in_place_vars,
                formatted_identifier_like(PascalReservedWord::End),
            )),
            |t| WebVarBlockItem::IfdefInPlace(t.0, t.1, t.2),
        ),
    ))(input)
}

fn parse_in_place_vars<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebInPlaceVariables<'a>> {
    map(
        tuple((
            separated_list0(pascal_token(PascalToken::Comma), identifier),
            pascal_token(PascalToken::Colon),
            parse_type,
            pascal_token(PascalToken::Semicolon),
            opt(comment),
        )),
        |tup| WebInPlaceVariables {
            vars: WebVariables {
                is_var: false,
                names: tup.0,
                ty: tup.2,
            },
            comment: tup.4,
        },
    )(input)
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

/// This machinery is mainly needed for XeTeX(2022.0):371, where each label gets
/// its own associated comment. The "name" can be a binary expression, as in
/// XeTeX(2022.0):1084, since WEB preprocesses basic arithmetic on numerical
/// constants.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebLabel<'a> {
    name: Box<WebExpr<'a>>,
    comment: Option<WebComment<'a>>,
}

/// This is a little tricky since the comments are optional and *after* the
/// separator between items.
fn parse_label_section<'a>(input: ParseInput<'a>) -> ParseResult<'a, Vec<WebLabel<'a>>> {
    let (mut input, _) = reserved_word(PascalReservedWord::Label)(input)?;

    let mut items = Vec::new();

    loop {
        let item;
        (input, item) = tuple((
            map(parse_expr, Box::new),
            alt((
                pascal_token(PascalToken::Comma),
                pascal_token(PascalToken::Semicolon),
            )),
            opt(comment),
        ))(input)?;

        items.push(WebLabel {
            name: item.0,
            comment: item.2,
        });

        if let PascalToken::Semicolon = item.1 {
            break;
        }
    }

    Ok((input, items))
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebConstant<'a> {
    name: StringSpan<'a>,
    value: PascalToken<'a>,
}

fn parse_const_item<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebConstant<'a>> {
    map(
        tuple((
            identifier,
            pascal_token(PascalToken::Equals),
            int_literal,
            pascal_token(PascalToken::Semicolon),
        )),
        |t| WebConstant {
            name: t.0,
            value: t.2,
        },
    )(input)
}

impl<'a> RenderInline for WebConstant<'a> {
    fn measure_inline(&self) -> usize {
        self.name.len() + 4 + self.value.measure_inline()
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        dest.noscope_push(&self.name);
        dest.noscope_push(" = ");
        self.value.render_inline(dest);
        dest.noscope_push(';');
    }
}

// Tying it all together

pub fn parse_function_definition_base<'a>(
    input: ParseInput<'a>,
) -> ParseResult<'a, WebFunctionDefinition<'a>> {
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
        opt(parse_label_section),
        opt(tuple((
            reserved_word(PascalReservedWord::Const),
            many1(parse_const_item),
        ))),
        opt(tuple((
            reserved_word(PascalReservedWord::Var),
            many1(parse_var_block_item),
        ))),
        alt((
            parse_statement_base,
            // XeTex(2022.0):638 has a procedure definition with an outer
            // `begin` that is missing its `end`. It contains just one statement
            // so we can handle it as follows:
            map(
                tuple((
                    reserved_word(PascalReservedWord::Begin),
                    parse_statement_base,
                )),
                |t| t.1,
            ),
        )),
        opt(comment),
    ))(input)?;

    let name = items.1;
    let args = items.2.map(|t| t.1).unwrap_or_default();
    let return_type = items.3.map(|t| t.1);
    let opening_comment = items.5;
    let labels = items.6.unwrap_or_default();
    let consts = items.7.map(|t| t.1).unwrap_or_default();
    let vars = items.8.map(|t| t.1).unwrap_or_default();
    let stmt = items.9;
    let closing_comment = items.10;

    Ok((
        input,
        WebFunctionDefinition {
            name,
            args,
            return_type,
            opening_comment,
            labels,
            consts,
            vars,
            stmt,
            closing_comment,
        },
    ))
}

pub fn parse_function_definition<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    map(parse_function_definition_base, |d| {
        WebToplevel::FunctionDefinition(d)
    })(input)
}

// Prettifying

impl<'a> WebFunctionDefinition<'a> {
    pub fn prettify(&self, dest: &mut Prettifier) {
        // Opening comment

        if let Some(c) = self.opening_comment.as_ref() {
            c.render_inline(dest);
            dest.newline_needed();
        }

        // Prototype: name, args, retval

        let wname = self.name.value.as_ref().len();
        let wargs: usize =
            self.args.iter().map(|a| a.measure_inline()).sum::<usize>() + 2 * (self.args.len() - 1);
        let wret = self
            .return_type
            .as_ref()
            .map(|r| r.measure_inline() + 2) // type + ": "
            .unwrap_or(0);

        if dest.fits(wname + wargs + wret + 13) {
            // Single-line prototype: "function () {"
            dest.keyword("function");
            dest.space();
            dest.noscope_push(self.name.value.as_ref());
            dest.noscope_push('(');
            prettify::render_inline_seq(&self.args, ", ", dest);
        } else {
            // Multi-line function prototype
            dest.keyword("function");
            dest.space();
            dest.noscope_push(self.name.value.as_ref());
            dest.noscope_push('(');
            dest.indent_small();
            dest.newline_needed();

            for arg in &self.args {
                arg.render_inline(dest);
                dest.noscope_push(',');
                dest.newline_needed();
            }

            dest.dedent_small();
        }

        dest.noscope_push(')');

        if let Some(r) = self.return_type.as_ref() {
            dest.noscope_push(": ");
            r.render_inline(dest);
        }

        dest.noscope_push(" {");
        dest.newline_needed();
        dest.indent_block();

        // Labels

        if !self.labels.is_empty() {
            // This measurement will include all labels and the associated
            // comment inline, if there is no more than one comment.
            let wl = self.labels.measure_inline();

            if dest.fits(wl + 6) {
                dest.keyword("label");
                dest.space();
                self.labels.render_inline(dest);
            } else {
                // If that didn't work, but there is only one comment, maybe we can
                // render that comment on one line and the labels inline?

                let mut comment = None;
                let mut n_comments = 0;
                let mut wl = 0;

                for label in &self.labels {
                    if label.comment.is_some() {
                        n_comments += 1;
                        comment = label.comment.as_ref();
                    }

                    wl += label.name.measure_inline() + 2;
                }

                // 5 = len("label ;") - len(", ")
                if n_comments == 1 && dest.fits(wl + 5) {
                    // Yes, we can do that. This `if let` should never fail,
                    // because otherwise the simplest case would have worked.
                    if let Some(c) = comment {
                        c.render_inline(dest);
                        dest.newline_needed();
                    }

                    dest.keyword("label");
                    dest.space();

                    let mut first = true;

                    for label in &self.labels {
                        if first {
                            first = false;
                        } else {
                            dest.noscope_push(", ");
                        }

                        label.name.render_inline(dest);
                    }

                    dest.noscope_push(";");
                } else {
                    // If that didn't work, we'll need to render in a vertical layout.

                    dest.keyword("label");
                    dest.indent_block();

                    let i_last = self.labels.len() - 1;

                    for (i, label) in self.labels.iter().enumerate() {
                        dest.newline_needed();

                        let sep = if i == i_last { ';' } else { ',' };

                        label.name.render_inline(dest);
                        dest.noscope_push(sep);

                        if let Some(c) = label.comment.as_ref() {
                            dest.space();
                            c.render_inline(dest);
                        }
                    }

                    dest.dedent_block();
                    dest.newline_needed();
                }
            }
        }

        // Consts

        if !self.consts.is_empty() {
            let wc: usize = self.consts.iter().map(|s| s.measure_inline()).sum();

            if self.consts.len() == 1 && dest.fits(wc + 6) {
                // "const "
                dest.keyword("const");
                dest.space();
                self.consts[0].render_inline(dest);
            } else {
                // Multi-line form
                dest.keyword("const");
                dest.indent_small();

                for c in &self.consts {
                    dest.newline_needed();
                    c.render_inline(dest);
                }

                dest.dedent_small();
            }

            dest.newline_needed();
        }

        // Vars

        if !self.vars.is_empty() {
            let mut wv: usize = self.vars.iter().map(|s| s.measure_inline()).sum();
            wv += 2 * (self.vars.len() - 1);

            if dest.fits(wv + 5) {
                // "var ;"
                dest.keyword("var");
                dest.space();
                prettify::render_inline_seq(&self.vars, ", ", dest);
                dest.noscope_push(";");
            } else {
                // Multi-line var declarations
                dest.keyword("var");
                dest.indent_small();
                dest.newline_needed();

                let n_last = self.vars.len() - 1;

                for (i, v) in self.vars.iter().enumerate() {
                    let term = if i == n_last { ';' } else { ',' };

                    v.prettify(dest, term);
                    dest.newline_needed();
                }

                dest.dedent_small();
            }

            dest.newline_needed();
        }

        // Statements

        if !self.labels.is_empty() || !self.vars.is_empty() {
            dest.newline_indent();
            dest.newline_needed();
        }

        self.stmt.render_in_block(dest);
        dest.newline_needed();

        // Closing comment

        if let Some(c) = self.closing_comment.as_ref() {
            c.render_inline(dest);
            dest.newline_needed();
        }

        // Done

        dest.newline_needed();
        dest.dedent_block();
        dest.noscope_push("}");
        dest.newline_needed();
    }
}

impl<'a> RenderInline for WebVariables<'a> {
    fn measure_inline(&self) -> usize {
        let mut w = 0;

        if self.is_var {
            w += 4;
        }

        for n in &self.names {
            w += n.value.as_ref().len();
        }

        w += 2 * (self.names.len() - 1); // ", " between names
        w += 2; // ": "
        w += self.ty.measure_inline();
        w
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        if self.is_var {
            dest.keyword("var");
            dest.space();
        }

        let mut first = true;

        for n in &self.names {
            if first {
                first = false;
            } else {
                dest.noscope_push(", ");
            }

            dest.noscope_push(n.value.as_ref());
        }

        dest.noscope_push(": ");
        self.ty.render_inline(dest);
    }
}

impl<'a> RenderInline for WebVarBlockItem<'a> {
    fn measure_inline(&self) -> usize {
        match self {
            WebVarBlockItem::ModuleReference(mr) => mr.measure_inline(),
            WebVarBlockItem::InPlace(ip) => ip.measure_inline(),
            WebVarBlockItem::IfdefInPlace(..) => prettify::NOT_INLINE,
        }
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        match self {
            WebVarBlockItem::ModuleReference(mr) => mr.render_inline(dest),
            WebVarBlockItem::InPlace(ip) => ip.render_inline(dest),
            WebVarBlockItem::IfdefInPlace(..) => dest.noscope_push("XXXifdefvbiinline"),
        }
    }
}

impl<'a> WebVarBlockItem<'a> {
    pub fn prettify(&self, dest: &mut Prettifier, term: char) {
        match self {
            WebVarBlockItem::ModuleReference(mr) => {
                mr.render_inline(dest);
                dest.noscope_push(term);
            }

            WebVarBlockItem::InPlace(ip) => ip.prettify(dest, term),
            WebVarBlockItem::IfdefInPlace(beg, vars, _end) => {
                beg.render_inline(dest);
                dest.noscope_push("!{");
                dest.indent_block();
                dest.newline_indent();
                vars.prettify(dest, ';');
                dest.dedent_block();
                dest.newline_indent();
                dest.noscope_push('}');
            }
        }
    }
}

impl<'a> RenderInline for WebInPlaceVariables<'a> {
    fn measure_inline(&self) -> usize {
        if self.comment.is_some() {
            crate::prettify::NOT_INLINE
        } else {
            self.vars.measure_inline()
        }
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        // By definition, no comment
        self.vars.render_inline(dest);
    }
}

impl<'a> WebInPlaceVariables<'a> {
    pub fn prettify(&self, dest: &mut Prettifier, term: char) {
        self.vars.render_inline(dest);
        dest.noscope_push(term);

        if let Some(c) = self.comment.as_ref() {
            dest.space();
            c.render_inline(dest);
        }
    }
}

impl<'a> RenderInline for Vec<WebLabel<'a>> {
    fn measure_inline(&self) -> usize {
        let mut n = 0;
        let mut n_comments = 0;

        for label in self {
            if let Some(c) = label.comment.as_ref() {
                n += c.measure_inline() + 1;
                n_comments += 1;

                if n_comments > 1 {
                    return prettify::NOT_INLINE;
                }
            }

            n += label.name.measure_inline() + 2;
        }

        // We accounted for an excessive ", " before, but need to add a trailing
        // semicolon, because we need to render our own comment.
        n - 1
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        let mut comment = None;
        let mut first = true;

        for label in self {
            if first {
                first = false;
            } else {
                dest.noscope_push(", ");
            }

            if label.comment.is_some() {
                comment = label.comment.as_ref();
            }

            label.name.render_inline(dest);
        }

        dest.noscope_push(";");

        if let Some(c) = comment {
            dest.space();
            c.render_inline(dest);
        }
    }
}
