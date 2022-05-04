//! Higher-level WEB language processing.
//!
//! This is *mostly* Pascal, but with a few additions. We implement parsing with
//! `nom` where the underlying datatype is a sequence of tokens.

use nom::{
    branch::alt,
    bytes::complete::take_while,
    combinator::opt,
    multi::{many1, separated_list1},
    Finish, InputLength,
};

pub mod base;
mod comment;
mod const_declaration;
mod define;
mod expr;
mod format;
mod forward_declaration;
mod function_definition;
mod label_declaration;
pub mod module_reference;
mod modulified_declaration;
mod preprocessor_directive;
mod program_definition;
mod standalone;
mod statement;
mod type_declaration;
mod var_declaration;
mod webtype;

use crate::prettify::{self, Prettifier, RenderInline, COMMENT_SCOPE};

use self::{
    base::*,
    expr::{parse_expr, WebExpr},
    statement::WebStatement,
};

pub use self::base::{WebSyntax, WebToken};

/// A top-level WEB production.
///
/// A "top-level" is whatever it takes to make it true that any WEB Pascal block
/// can be expressed as a series of toplevels, including `@define` and `@format`
/// statements. Because we're not actually compiling the WEB language in any
/// meaningful way, we're not very intellectually rigorous.
///
/// Toplevel module references are captured as Statements.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebToplevel<'a> {
    /// A `@d` definition.
    Define(define::WebDefine<'a>),

    /// A `@f` format definition.
    Format(format::WebFormat<'a>),

    /// A single Pascal token (with optional comment).
    Standalone(standalone::WebStandalone<'a>),

    /// The program definition.
    ProgramDefinition(program_definition::WebProgramDefinition<'a>),

    /// A label declaration.
    LabelDeclaration(label_declaration::WebLabelDeclaration<'a>),

    /// Declarations that are done by referencing a module.
    ModulifiedDeclaration(modulified_declaration::WebModulifiedDeclaration<'a>),

    /// Definition of a procedure or function
    FunctionDefinition(function_definition::WebFunctionDefinition<'a>),

    /// Declaration of a constant.
    ConstDeclaration(const_declaration::WebConstantDeclaration<'a>),

    /// Declaration of a variable.
    VarDeclaration(var_declaration::WebVarDeclaration<'a>),

    /// Declaration of a type.
    TypeDeclaration(type_declaration::WebTypeDeclaration<'a>),

    /// Forward declaration of a function or procedure.
    ForwardDeclaration(forward_declaration::WebForwardDeclaration<'a>),

    /// A Pascal statement.
    Statement(WebStatement<'a>, Option<WebComment<'a>>),

    /// No code at all, needed for XeTeX(2022.0):23.
    Empty,

    /// `( $ident $ident )`, needed for WEAVE:143
    SpecialParenTwoIdent(StringSpan<'a>, StringSpan<'a>),

    /// `[]`, needed for WEAVE:143
    SpecialEmptyBrackets,

    /// `$relational_op $expr`, needed for WEAVE:144
    SpecialRelationalExpr(PascalToken<'a>, WebExpr<'a>),

    /// `$expr .. $expr`, needed for WEAVE:144, XeTeX(2022.0):83
    SpecialRange(WebExpr<'a>, WebExpr<'a>),

    /// `$begin_like $function $end_like`, needed for WEAVE:260
    SpecialIfdefFunction(
        PascalToken<'a>,
        function_definition::WebFunctionDefinition<'a>,
        PascalToken<'a>,
    ),

    /// `$begin_like $forward_declaration $end_like`, needed for WEAVE:30 and others
    SpecialIfdefForward(
        PascalToken<'a>,
        forward_declaration::WebForwardDeclaration<'a>,
        PascalToken<'a>,
    ),

    /// `$begin_like $var_declaration $end_like`, needed for WEAVE:244
    SpecialIfdefVarDeclaration(
        Option<WebComment<'a>>,
        PascalToken<'a>,
        Vec<var_declaration::WebVarDeclaration<'a>>,
        PascalToken<'a>,
        Option<WebComment<'a>>,
    ),

    /// `$start_meta_comment $statement $end_meta_comment`, needed for XeTeX(2022.0):31.
    SpecialCommentedOut(WebStatement<'a>),

    /// `$[$int0, $int1a .. $int1b, ...]`, needed for XeTeX(2022.0):49.
    SpecialIntList(Vec<SpecialIntListTerm<'a>>),

    /// `$ident in [$int0, $int1a .. $int1b, ...]`, needed for XeTeX(2022.0):49.
    SpecialIdentInIntList(StringSpan<'a>, Vec<SpecialIntListTerm<'a>>),

    /// `$expr == $expr`, needed for XeTeX(2022.0):134.
    SpecialInlineDefine(WebExpr<'a>, WebExpr<'a>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SpecialIntListTerm<'a> {
    Single(PascalToken<'a>),
    Range(PascalToken<'a>, PascalToken<'a>),
}

impl<'a> RenderInline for SpecialIntListTerm<'a> {
    fn measure_inline(&self) -> usize {
        match self {
            SpecialIntListTerm::Single(t) => t.measure_inline(),
            SpecialIntListTerm::Range(t1, t2) => t1.measure_inline() + 4 + t2.measure_inline(),
        }
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        match self {
            SpecialIntListTerm::Single(t) => t.render_inline(dest),
            SpecialIntListTerm::Range(t1, t2) => {
                t1.render_inline(dest);
                dest.noscope_push(" .. ");
                t2.render_inline(dest);
            }
        }
    }
}

/// A block of WEB code: a sequence of parsed-out WEB toplevels
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebCode<'a>(pub Vec<WebToplevel<'a>>);

impl<'a> WebCode<'a> {
    /// Parse a sequence of WEB tokens into sequence of toplevels.
    pub fn parse(syntax: &'a WebSyntax<'a>) -> Option<WebCode<'a>> {
        let input = ParseInput(&syntax.0[..]);

        if input.input_len() == 0 {
            return Some(WebCode(vec![WebToplevel::Empty]));
        }

        match many1(parse_toplevel)(input).finish() {
            Ok((remainder, value)) => {
                if remainder.input_len() > 0 {
                    eprintln!("\nincomplete parse");
                    return None;
                } else {
                    return Some(WebCode(value));
                }
            }

            Err((_remainder, e)) => {
                eprintln!("parse error: {:?}", e);
                return None;
            }
        }
    }
}

fn is_ignored_token(t: WebToken) -> bool {
    match t {
        WebToken::Pascal(PascalToken::Formatting)
        | WebToken::Pascal(PascalToken::ForcedEol)
        | WebToken::Pascal(PascalToken::TexString(..)) => true,
        _ => false,
    }
}

fn parse_toplevel<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, _) = take_while(is_ignored_token)(input)?;

    // We have so many possibilities that we need to use multiple alt() calls to
    // avoid the limit of 20-item tuples!
    let result = alt((
        // Define comes first since its tail is a toplevel in and of itself.
        define::parse_define,
        format::parse_format,
        program_definition::parse_program_definition,
        label_declaration::parse_label_declaration,
        modulified_declaration::parse_modulified_declaration,
        forward_declaration::parse_forward_declaration,
        function_definition::parse_function_definition,
        const_declaration::parse_constant_declaration,
        var_declaration::parse_var_declaration,
        type_declaration::parse_type_declaration,
        alt((
            tl_specials::parse_special_ifdef_forward,
            tl_specials::parse_special_ifdef_function,
            tl_specials::parse_special_ifdef_var_decl,
            tl_specials::parse_special_paren_two_ident,
            tl_specials::parse_special_empty_brackets,
            tl_specials::parse_special_relational_expr,
            tl_specials::parse_special_range,
            tl_specials::parse_special_commented_out,
            tl_specials::parse_special_int_list,
            tl_specials::parse_special_ident_in_int_list,
            tl_specials::parse_special_inline_define,
        )),
        statement::parse_statement,
        standalone::parse_standalone,
    ))(input);

    match &result {
        Ok((input, v)) => {
            eprintln!("TL OK: {:?}", v);
            let n = usize::min(input.input_len(), 8);
            for tok in &input.0[..n] {
                eprintln!("- {:?}", tok);
            }
        }

        Err(nom::Err::Error((input, kind))) => {
            if kind != &WebErrorKind::Eof {
                eprintln!("TL error {:?}", kind);
                let n = usize::min(input.input_len(), 20);
                for tok in &input.0[..n] {
                    eprintln!("- {:?}", tok);
                }
            }
        }

        _ => {
            eprintln!("TL other failure???");
        }
    }

    result
}

mod tl_specials {
    use nom::{combinator::map, sequence::tuple};

    use super::*;

    pub fn parse_special_paren_two_ident<'a>(
        input: ParseInput<'a>,
    ) -> ParseResult<'a, WebToplevel<'a>> {
        map(
            tuple((
                open_delimiter(DelimiterKind::Paren),
                identifier,
                identifier,
                close_delimiter(DelimiterKind::Paren),
            )),
            |t| WebToplevel::SpecialParenTwoIdent(t.1, t.2),
        )(input)
    }

    pub fn parse_special_empty_brackets<'a>(
        input: ParseInput<'a>,
    ) -> ParseResult<'a, WebToplevel<'a>> {
        map(
            tuple((
                open_delimiter(DelimiterKind::SquareBracket),
                close_delimiter(DelimiterKind::SquareBracket),
            )),
            |_| WebToplevel::SpecialEmptyBrackets,
        )(input)
    }

    pub fn parse_special_relational_expr<'a>(
        input: ParseInput<'a>,
    ) -> ParseResult<'a, WebToplevel<'a>> {
        map(tuple((relational_ident_op, parse_expr)), |t| {
            WebToplevel::SpecialRelationalExpr(t.0, t.1)
        })(input)
    }

    fn relational_ident_op<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
        let (input, wt) = next_token(input)?;

        if let WebToken::Pascal(pt) = wt {
            match pt {
                PascalToken::Greater
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

    pub fn parse_special_range<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
        map(
            tuple((parse_expr, pascal_token(PascalToken::DoubleDot), parse_expr)),
            |t| WebToplevel::SpecialRange(t.0, t.2),
        )(input)
    }

    pub fn parse_special_ifdef_function<'a>(
        input: ParseInput<'a>,
    ) -> ParseResult<'a, WebToplevel<'a>> {
        map(
            tuple((
                formatted_identifier_like(PascalReservedWord::Begin),
                function_definition::parse_function_definition_base,
                formatted_identifier_like(PascalReservedWord::End),
            )),
            |t| WebToplevel::SpecialIfdefFunction(t.0, t.1, t.2),
        )(input)
    }

    pub fn parse_special_ifdef_forward<'a>(
        input: ParseInput<'a>,
    ) -> ParseResult<'a, WebToplevel<'a>> {
        map(
            tuple((
                formatted_identifier_like(PascalReservedWord::Begin),
                forward_declaration::parse_forward_declaration_base,
                formatted_identifier_like(PascalReservedWord::End),
            )),
            |t| WebToplevel::SpecialIfdefForward(t.0, t.1, t.2),
        )(input)
    }

    pub fn parse_special_ifdef_var_decl<'a>(
        input: ParseInput<'a>,
    ) -> ParseResult<'a, WebToplevel<'a>> {
        map(
            tuple((
                opt(comment),
                formatted_identifier_like(PascalReservedWord::Begin),
                many1(var_declaration::parse_var_declaration_base),
                formatted_identifier_like(PascalReservedWord::End),
                opt(comment),
            )),
            |t| WebToplevel::SpecialIfdefVarDeclaration(t.0, t.1, t.2, t.3, t.4),
        )(input)
    }

    pub fn parse_special_commented_out<'a>(
        input: ParseInput<'a>,
    ) -> ParseResult<'a, WebToplevel<'a>> {
        map(
            tuple((
                pascal_token(PascalToken::OpenDelimiter(DelimiterKind::MetaComment)),
                statement::parse_statement_base,
                pascal_token(PascalToken::CloseDelimiter(DelimiterKind::MetaComment)),
            )),
            |t| WebToplevel::SpecialCommentedOut(t.1),
        )(input)
    }

    pub fn parse_special_int_list<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
        map(
            tuple((
                pascal_token(PascalToken::OpenDelimiter(DelimiterKind::SquareBracket)),
                separated_list1(pascal_token(PascalToken::Comma), int_list_term),
                pascal_token(PascalToken::CloseDelimiter(DelimiterKind::SquareBracket)),
            )),
            |t| WebToplevel::SpecialIntList(t.1),
        )(input)
    }

    pub fn parse_special_ident_in_int_list<'a>(
        input: ParseInput<'a>,
    ) -> ParseResult<'a, WebToplevel<'a>> {
        map(
            tuple((
                identifier,
                reserved_word(PascalReservedWord::In),
                pascal_token(PascalToken::OpenDelimiter(DelimiterKind::SquareBracket)),
                separated_list1(pascal_token(PascalToken::Comma), int_list_term),
                pascal_token(PascalToken::CloseDelimiter(DelimiterKind::SquareBracket)),
            )),
            |t| WebToplevel::SpecialIdentInIntList(t.0, t.3),
        )(input)
    }

    fn int_list_term<'a>(input: ParseInput<'a>) -> ParseResult<'a, SpecialIntListTerm<'a>> {
        alt((
            map(
                tuple((
                    int_literal,
                    pascal_token(PascalToken::DoubleDot),
                    int_literal,
                )),
                |t| SpecialIntListTerm::Range(t.0, t.2),
            ),
            map(int_literal, |i| SpecialIntListTerm::Single(i)),
        ))(input)
    }

    pub fn parse_special_inline_define<'a>(
        input: ParseInput<'a>,
    ) -> ParseResult<'a, WebToplevel<'a>> {
        map(
            tuple((
                parse_expr,
                pascal_token(PascalToken::Equivalence),
                parse_expr,
            )),
            |t| WebToplevel::SpecialInlineDefine(t.0, t.2),
        )(input)
    }
}

impl<'a> WebToplevel<'a> {
    pub fn prettify(&self, dest: &mut Prettifier) {
        match self {
            WebToplevel::Statement(stmt, comment) => tl_prettify::statement(stmt, comment, dest),
            WebToplevel::Standalone(s) => s.render_inline(dest),
            WebToplevel::Define(d) => d.prettify(dest),
            WebToplevel::Format(f) => f.prettify(dest),
            WebToplevel::LabelDeclaration(ld) => ld.prettify(dest),
            WebToplevel::ProgramDefinition(pd) => pd.prettify(dest),
            WebToplevel::ModulifiedDeclaration(md) => md.prettify(dest),
            WebToplevel::FunctionDefinition(fd) => fd.prettify(dest),
            WebToplevel::ConstDeclaration(cd) => cd.prettify(dest),
            WebToplevel::VarDeclaration(vd) => vd.prettify(dest),
            WebToplevel::TypeDeclaration(td) => td.prettify(dest),
            WebToplevel::ForwardDeclaration(fd) => fd.prettify(dest),
            WebToplevel::Empty => dest.scope_push(*COMMENT_SCOPE, "/*nothing*/"),

            WebToplevel::SpecialParenTwoIdent(id1, id2) => {
                tl_prettify::special_paren_two_ident(id1, id2, dest)
            }
            WebToplevel::SpecialEmptyBrackets => tl_prettify::special_empty_brackets(dest),
            WebToplevel::SpecialRelationalExpr(op, expr) => {
                tl_prettify::special_relational_expr(op, expr, dest)
            }
            WebToplevel::SpecialRange(e1, e2) => tl_prettify::special_range(e1, e2, dest),
            WebToplevel::SpecialIfdefFunction(beg, fd, end) => {
                tl_prettify::special_ifdef_function(beg, fd, end, dest)
            }
            WebToplevel::SpecialIfdefForward(beg, fd, end) => {
                tl_prettify::special_ifdef_forward(beg, fd, end, dest)
            }
            WebToplevel::SpecialIfdefVarDeclaration(c1, beg, vd, end, c2) => {
                tl_prettify::special_ifdef_var_declaration(c1, beg, vd, end, c2, dest)
            }
            WebToplevel::SpecialCommentedOut(stmt) => {
                tl_prettify::special_commented_out(stmt, dest)
            }
            WebToplevel::SpecialIdentInIntList(id, vals) => {
                tl_prettify::special_ident_in_int_list(id, vals, dest)
            }
            WebToplevel::SpecialIntList(vals) => tl_prettify::special_int_list(vals, dest),
            WebToplevel::SpecialInlineDefine(lhs, rhs) => {
                tl_prettify::special_inline_define(lhs, rhs, dest)
            }
        }
    }
}

mod tl_prettify {
    use super::*;

    pub fn statement<'a>(
        stmt: &WebStatement<'a>,
        comment: &Option<WebComment<'a>>,
        dest: &mut Prettifier,
    ) {
        // Most statements won't be able to be rendered inline, but a few can.
        let clen = comment.as_ref().map(|c| c.measure_inline()).unwrap_or(0);
        let slen = stmt.measure_inline();

        if dest.fits(clen + slen + 1) {
            stmt.render_inline(dest);

            if let Some(c) = comment.as_ref() {
                dest.space();
                c.render_inline(dest);
            }
        } else if dest.fits(slen) {
            if let Some(c) = comment.as_ref() {
                c.render_inline(dest);
                dest.newline_needed();
            }

            stmt.render_inline(dest);
        } else {
            if let Some(c) = comment.as_ref() {
                c.render_inline(dest);
                dest.newline_needed();
            }

            stmt.render_flex(dest);
        }

        dest.newline_needed();
    }

    pub fn special_paren_two_ident<'a>(
        id1: &StringSpan<'a>,
        id2: &StringSpan<'a>,
        dest: &mut Prettifier,
    ) {
        dest.noscope_push('(');
        dest.noscope_push(id1);
        dest.noscope_push(id2);
        dest.noscope_push(')');
    }

    pub fn special_empty_brackets<'a>(dest: &mut Prettifier) {
        dest.noscope_push("[]");
    }

    pub fn special_relational_expr<'a>(
        op: &PascalToken<'a>,
        expr: &WebExpr<'a>,
        dest: &mut Prettifier,
    ) {
        op.render_inline(dest);
        expr.render_inline(dest);
    }

    pub fn special_range<'a>(e1: &WebExpr<'a>, e2: &WebExpr<'a>, dest: &mut Prettifier) {
        e1.render_inline(dest);
        dest.noscope_push(" .. ");
        e2.render_inline(dest);
    }

    pub fn special_ifdef_function<'a>(
        beg: &PascalToken<'a>,
        fd: &function_definition::WebFunctionDefinition<'a>,
        _end: &PascalToken<'a>,
        dest: &mut Prettifier,
    ) {
        beg.render_inline(dest);
        dest.noscope_push("!{");
        dest.indent_block();
        dest.newline_indent();
        fd.prettify(dest);
        dest.dedent_block();
        dest.newline_indent();
        dest.noscope_push('}');
    }

    pub fn special_ifdef_forward<'a>(
        beg: &PascalToken<'a>,
        fd: &forward_declaration::WebForwardDeclaration<'a>,
        _end: &PascalToken<'a>,
        dest: &mut Prettifier,
    ) {
        beg.render_inline(dest);
        dest.noscope_push("!{");
        dest.indent_block();
        dest.newline_indent();
        fd.prettify(dest);
        dest.dedent_block();
        dest.newline_indent();
        dest.noscope_push('}');
    }

    pub fn special_ifdef_var_declaration<'a>(
        c1: &Option<WebComment<'a>>,
        beg: &PascalToken<'a>,
        vds: &Vec<var_declaration::WebVarDeclaration<'a>>,
        _end: &PascalToken<'a>,
        c2: &Option<WebComment<'a>>,
        dest: &mut Prettifier,
    ) {
        if let Some(c) = c1.as_ref() {
            c.render_inline(dest);
            dest.newline_needed();
        }

        if let Some(c) = c2.as_ref() {
            c.render_inline(dest);
            dest.newline_needed();
        }

        beg.render_inline(dest);
        dest.noscope_push("!{");
        dest.indent_block();
        dest.newline_indent();

        for vd in vds {
            vd.prettify(dest);
            dest.newline_needed();
        }

        dest.dedent_block();
        dest.newline_indent();
        dest.noscope_push('}');
    }

    pub fn special_commented_out<'a>(stmt: &WebStatement<'a>, dest: &mut Prettifier) {
        dest.with_scope(*COMMENT_SCOPE, |d| {
            d.noscope_push("/*");
            d.indent_block();
            d.newline_needed();
            stmt.render_flex(d);
            stmt.maybe_semicolon(d);
            d.dedent_block();
            d.newline_needed();
            d.noscope_push("*/");
        });
    }

    pub fn special_int_list<'a>(vals: &Vec<SpecialIntListTerm<'a>>, dest: &mut Prettifier) {
        dest.noscope_push("[");
        prettify::render_inline_seq(vals, ", ", dest);
        dest.noscope_push("]");
    }

    pub fn special_ident_in_int_list<'a>(
        id: &StringSpan<'a>,
        vals: &Vec<SpecialIntListTerm<'a>>,
        dest: &mut Prettifier,
    ) {
        dest.noscope_push(id);
        dest.space();
        dest.keyword("in");
        dest.space();
        dest.noscope_push("[");
        prettify::render_inline_seq(vals, ", ", dest);
        dest.noscope_push("]");
    }

    pub fn special_inline_define<'a>(lhs: &WebExpr<'a>, rhs: &WebExpr<'a>, dest: &mut Prettifier) {
        lhs.render_inline(dest);
        dest.noscope_push(" => ");
        rhs.render_inline(dest);
    }
}
