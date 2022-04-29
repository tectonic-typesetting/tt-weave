//! A WEB/Pascal function definition.
//!
//! Here we group both functions and procedures together, the difference being
//! whether there's a return value or not.

use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many1, separated_list0, separated_list1},
    sequence::tuple,
};

use crate::prettify::{self, Prettifier, RenderInline};

use super::{
    base::*,
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
    labels: Vec<StringSpan<'a>>,

    /// Records in the function's `var` block.
    vars: Vec<WebVarBlockItem<'a>>,

    /// The statements that comprise the function.
    stmts: Vec<WebStatement<'a>>,
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
    ModuleReference(StringSpan<'a>),

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
        map(module_reference, |t| WebVarBlockItem::ModuleReference(t)),
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
        opt(tuple((
            reserved_word(PascalReservedWord::Label),
            separated_list1(pascal_token(PascalToken::Comma), identifier),
            pascal_token(PascalToken::Semicolon),
        ))),
        opt(tuple((
            reserved_word(PascalReservedWord::Var),
            many1(parse_var_block_item),
        ))),
        reserved_word(PascalReservedWord::Begin),
        many1(parse_statement_base),
        reserved_word(PascalReservedWord::End),
        pascal_token(PascalToken::Semicolon),
    ))(input)?;

    let name = items.1;
    let args = items.2.map(|t| t.1).unwrap_or_default();
    let return_type = items.3.map(|t| t.1);
    let opening_comment = items.5;
    let labels = items.6.map(|t| t.1).unwrap_or_default();
    let vars = items.7.map(|t| t.1).unwrap_or_default();
    let stmts = items.9;

    Ok((
        input,
        WebFunctionDefinition {
            name,
            args,
            return_type,
            opening_comment,
            labels,
            vars,
            stmts,
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
            dest.noscope_push("function ");
            dest.noscope_push(self.name.value.as_ref());
            dest.noscope_push('(');
            prettify::render_inline_seq(&self.args, ", ", dest);
        } else {
            // Multi-line function prototype
            dest.noscope_push("function ");
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
            let mut wl: usize = self.labels.iter().map(|s| s.value.as_ref().len()).sum();
            wl += 2 * (self.labels.len() - 1);

            if dest.fits(wl + 7) {
                // "label ;"
                dest.noscope_push("label ");

                let mut first = true;

                for l in &self.labels {
                    if first {
                        first = false;
                    } else {
                        dest.noscope_push(", ");
                    }

                    dest.noscope_push(l.value.as_ref());
                }
            } else {
                // Multi-line label declarations
                dest.noscope_push("label");
                dest.indent_small();
                dest.newline_needed();

                let mut first = true;

                for l in &self.labels {
                    if first {
                        first = false;
                    } else {
                        dest.noscope_push(",");
                        dest.newline_indent();
                    }

                    dest.noscope_push(l.value.as_ref());
                }

                dest.dedent_small();
            }

            dest.noscope_push(";");
            dest.newline_needed();
        }

        // Vars

        if !self.vars.is_empty() {
            let mut wv: usize = self.vars.iter().map(|s| s.measure_inline()).sum();
            wv += 2 * (self.vars.len() - 1);

            if dest.fits(wv + 5) {
                // "var ;"
                dest.noscope_push("var ");
                prettify::render_inline_seq(&self.vars, ", ", dest);
                dest.noscope_push(";");
            } else {
                // Multi-line var declarations
                dest.noscope_push("var");
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

        for s in &self.stmts {
            s.render_flex(dest);
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
            dest.noscope_push("var ");
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
            WebVarBlockItem::ModuleReference(mr) => prettify::module_reference_measure_inline(mr),
            WebVarBlockItem::InPlace(ip) => ip.measure_inline(),
            WebVarBlockItem::IfdefInPlace(..) => 9999, // never inline
        }
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        match self {
            WebVarBlockItem::ModuleReference(mr) => prettify::module_reference_render(mr, dest),
            WebVarBlockItem::InPlace(ip) => ip.render_inline(dest),
            WebVarBlockItem::IfdefInPlace(..) => dest.noscope_push("XXXifdefvbiinline"),
        }
    }
}

impl<'a> WebVarBlockItem<'a> {
    pub fn prettify(&self, dest: &mut Prettifier, term: char) {
        match self {
            WebVarBlockItem::ModuleReference(mr) => {
                prettify::module_reference_render(mr, dest);
                dest.noscope_push(term);
            }

            WebVarBlockItem::InPlace(ip) => ip.prettify(dest, term),
            WebVarBlockItem::IfdefInPlace(..) => dest.noscope_push("XXXifdefvbiREAL"),
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
