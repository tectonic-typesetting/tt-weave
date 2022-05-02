//! Higher-level WEB language processing.
//!
//! This is *mostly* Pascal, but with a few additions. We implement parsing with
//! `nom` where the underlying datatype is a sequence of tokens.

use nom::{
    branch::alt, bytes::complete::take_while, combinator::opt, multi::many1, Finish, InputLength,
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
mod modulified_declaration;
mod preprocessor_directive;
mod program_definition;
mod standalone;
mod statement;
mod type_declaration;
mod var_declaration;
mod webtype;

use crate::prettify::{Prettifier, RenderInline};

use self::{base::*, statement::WebStatement};

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

    /// `( $ident $ident )`, needed for WEAVE:143
    SpecialParenTwoIdent(StringSpan<'a>, StringSpan<'a>),

    /// `[]`, needed for WEAVE:143
    SpecialEmptyBrackets,

    /// `$relational_op $ident`, needed for WEAVE:144
    SpecialRelationalIdent(PascalToken<'a>, StringSpan<'a>),

    /// `$int .. $int`, needed for WEAVE:144
    SpecialIntRange(PascalToken<'a>, PascalToken<'a>),

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
        PascalToken<'a>,
        var_declaration::WebVarDeclaration<'a>,
        PascalToken<'a>,
        Option<WebComment<'a>>,
    ),
}

/// A block of WEB code: a sequence of parsed-out WEB toplevels
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebCode<'a>(pub Vec<WebToplevel<'a>>);

impl<'a> WebCode<'a> {
    /// Parse a sequence of WEB tokens into sequence of toplevels.
    pub fn parse(syntax: &'a WebSyntax<'a>) -> Option<WebCode<'a>> {
        let input = ParseInput(&syntax.0[..]);

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
        tl_specials::parse_special_ifdef_forward,
        tl_specials::parse_special_ifdef_function,
        tl_specials::parse_special_ifdef_var_decl,
        tl_specials::parse_special_paren_two_ident,
        tl_specials::parse_special_empty_brackets,
        tl_specials::parse_special_relational_ident,
        tl_specials::parse_special_int_range,
        statement::parse_statement,
        standalone::parse_standalone,
    ))(input);

    //match &result {
    //    Ok((input, v)) => {
    //        eprintln!("TL OK: {:?}", v);
    //        let n = usize::min(input.input_len(), 8);
    //        for tok in &input.0[..n] {
    //            eprintln!("- {:?}", tok);
    //        }
    //    }
    //
    //    Err(nom::Err::Error((input, kind))) => {
    //        if kind != &WebErrorKind::Eof {
    //            eprintln!("TL error {:?}", kind);
    //            let n = usize::min(input.input_len(), 20);
    //            for tok in &input.0[..n] {
    //                eprintln!("- {:?}", tok);
    //            }
    //        }
    //    }
    //
    //    _ => {
    //        eprintln!("TL other failure???");
    //    }
    //}

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

    pub fn parse_special_relational_ident<'a>(
        input: ParseInput<'a>,
    ) -> ParseResult<'a, WebToplevel<'a>> {
        map(tuple((relational_ident_op, identifier)), |t| {
            WebToplevel::SpecialRelationalIdent(t.0, t.1)
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

    pub fn parse_special_int_range<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
        map(
            tuple((
                int_literal,
                pascal_token(PascalToken::DoubleDot),
                int_literal,
            )),
            |t| WebToplevel::SpecialIntRange(t.0, t.2),
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
                formatted_identifier_like(PascalReservedWord::Begin),
                var_declaration::parse_var_declaration_base,
                formatted_identifier_like(PascalReservedWord::End),
                opt(comment),
            )),
            |t| WebToplevel::SpecialIfdefVarDeclaration(t.0, t.1, t.2, t.3),
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

            WebToplevel::SpecialParenTwoIdent(id1, id2) => {
                tl_prettify::special_paren_two_ident(id1, id2, dest)
            }
            WebToplevel::SpecialEmptyBrackets => tl_prettify::special_empty_brackets(dest),
            WebToplevel::SpecialRelationalIdent(op, id) => {
                tl_prettify::special_relational_ident(op, id, dest)
            }
            WebToplevel::SpecialIntRange(n1, n2) => tl_prettify::special_int_range(n1, n2, dest),
            WebToplevel::SpecialIfdefFunction(beg, fd, end) => {
                tl_prettify::special_ifdef_function(beg, fd, end, dest)
            }
            WebToplevel::SpecialIfdefForward(beg, fd, end) => {
                tl_prettify::special_ifdef_forward(beg, fd, end, dest)
            }
            WebToplevel::SpecialIfdefVarDeclaration(beg, vd, end, comment) => {
                tl_prettify::special_ifdef_var_declaration(beg, vd, end, comment, dest)
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

    pub fn special_relational_ident<'a>(
        op: &PascalToken<'a>,
        id: &StringSpan<'a>,
        dest: &mut Prettifier,
    ) {
        op.render_inline(dest);
        dest.noscope_push(id);
    }

    pub fn special_int_range<'a>(
        n1: &PascalToken<'a>,
        n2: &PascalToken<'a>,
        dest: &mut Prettifier,
    ) {
        n1.render_inline(dest);
        dest.noscope_push(" .. ");
        n2.render_inline(dest);
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
        beg: &PascalToken<'a>,
        vd: &var_declaration::WebVarDeclaration<'a>,
        _end: &PascalToken<'a>,
        comment: &Option<WebComment<'a>>,
        dest: &mut Prettifier,
    ) {
        if let Some(c) = comment.as_ref() {
            c.render_inline(dest);
            dest.newline_needed();
        }

        beg.render_inline(dest);
        dest.noscope_push("!{");
        dest.indent_block();
        dest.newline_indent();
        vd.prettify(dest);
        dest.dedent_block();
        dest.newline_indent();
        dest.noscope_push('}');
    }
}
