//! Prettify the Pascal source.

use lazy_static::lazy_static;
use std::{
    fmt::{self, Write},
    ops::Deref,
    str::FromStr,
};
use syntect::{
    highlighting::{Color, FontStyle, HighlightIterator, HighlightState, Highlighter, Theme},
    parsing::{Scope, ScopeStack, ScopeStackOp},
};

use crate::weblang::base::SpanValue;

// See https://www.sublimetext.com/docs/scope_naming.html for some scope hints.

const INITIAL_SCOPES: &str = "source.c";

lazy_static! {
    pub static ref KEYWORD_SCOPE: Scope = Scope::new("keyword.control.c").unwrap();
    pub static ref COMMENT_SCOPE: Scope = Scope::new("comment.line.c").unwrap();
    pub static ref STRING_LITERAL_SCOPE: Scope = Scope::new("string.quoted.double").unwrap();
    pub static ref HEX_LITERAL_SCOPE: Scope =
        Scope::new("constant.numeric.integer.hexadecimal").unwrap();
    pub static ref DECIMAL_LITERAL_SCOPE: Scope =
        Scope::new("constant.numeric.integer.decimal").unwrap();
    pub static ref FLOAT_LITERAL_SCOPE: Scope = Scope::new("constant.numeric.float").unwrap();
    pub static ref LABEL_NAME_SCOPE: Scope = Scope::new("entity.name.label").unwrap();
}

const WIDTH: usize = 60;

#[derive(Clone, Debug)]
pub struct Prettifier {
    full_width: usize,
    indent: usize,
    remaining_width: usize,
    newline_needed: bool,
    text: String,
    ops: Vec<(usize, ScopeStackOp)>,
}

impl Prettifier {
    pub fn new() -> Self {
        Prettifier {
            full_width: WIDTH,
            indent: 0,
            remaining_width: WIDTH,
            newline_needed: false,
            text: String::default(),
            ops: Vec::default(),
        }
    }

    #[inline(always)]
    pub fn fits(&self, width: usize) -> bool {
        let eff_width = if self.newline_needed {
            self.full_width - self.indent
        } else {
            self.remaining_width
        };

        width <= eff_width
    }

    pub fn would_fit_on_new_line(&self, width: usize) -> bool {
        width <= self.full_width - self.indent
    }

    pub fn indent_block(&mut self) -> bool {
        if self.full_width - self.indent > 4 {
            self.indent += 4;
            true
        } else {
            false
        }
    }

    pub fn dedent_block(&mut self) -> bool {
        if self.indent > 3 {
            self.indent -= 4;
            true
        } else {
            false
        }
    }

    pub fn indent_small(&mut self) -> bool {
        if self.full_width - self.indent > 2 {
            self.indent += 2;
            true
        } else {
            false
        }
    }

    pub fn dedent_small(&mut self) -> bool {
        if self.indent > 1 {
            self.indent -= 2;
            true
        } else {
            false
        }
    }

    pub fn newline_indent(&mut self) {
        self.text.push('\n');

        for _ in 0..self.indent {
            self.text.push(' ');
        }

        self.newline_needed = false;
        self.remaining_width = self.full_width - self.indent;
    }

    pub fn newline_needed(&mut self) {
        self.newline_needed = true;
    }

    #[inline(always)]
    fn maybe_newline(&mut self) {
        if self.newline_needed {
            self.newline_indent();
        }
    }

    pub fn scope_push<S: fmt::Display>(&mut self, scope: Scope, text: S) {
        self.maybe_newline();

        let n0 = self.text.len();
        self.ops.push((n0, ScopeStackOp::Push(scope)));
        write!(self.text, "{}", text).unwrap();
        let n1 = self.text.len();
        self.ops.push((n1, ScopeStackOp::Pop(1)));
        self.remaining_width = self.remaining_width.saturating_sub(n1 - n0);
    }

    pub fn with_scope<F: FnOnce(&mut Self)>(&mut self, scope: Scope, func: F) {
        let n0 = self.text.len();
        self.ops.push((n0, ScopeStackOp::Push(scope)));
        func(self);
        let n1 = self.text.len();
        self.ops.push((n1, ScopeStackOp::Pop(1)));
    }

    pub fn keyword<S: fmt::Display>(&mut self, text: S) {
        self.scope_push(*KEYWORD_SCOPE, text)
    }

    pub fn noscope_push<S: fmt::Display>(&mut self, text: S) {
        // TODO: never use this? Should always have some kine of scope?
        self.maybe_newline();
        let n0 = self.text.len();
        write!(self.text, "{}", text).unwrap();
        let n1 = self.text.len();
        self.remaining_width = self.remaining_width.saturating_sub(n1 - n0);
    }

    pub fn space(&mut self) {
        self.text.push(' ');
        self.remaining_width = self.remaining_width.saturating_sub(1);
    }

    pub fn toplevel_separator(&mut self) {
        self.text.push('\n');
        self.newline_indent();
    }

    pub fn emit(self, theme: &Theme, inline: bool) {
        let highlighter = Highlighter::new(theme);
        let initial_stack = ScopeStack::from_str(INITIAL_SCOPES).unwrap();
        let mut hs = HighlightState::new(&highlighter, initial_stack);
        let hi = HighlightIterator::new(&mut hs, &self.ops[..], &self.text[..], &highlighter);

        let (env, terminator) = if inline {
            ("WebPrettifiedInline", "")
        } else {
            ("WebPrettifiedDisplay", "%\n")
        };

        println!("\\begin{{{}}}%", env);

        for (style, span) in hi {
            print!(
                "\\S{{{}}}{{{}}}{{",
                ColorHexConvert(style.foreground),
                ColorHexConvert(style.background)
            );

            if style.font_style.intersects(FontStyle::BOLD) {
                print!("\\bf");
            }

            if style.font_style.intersects(FontStyle::ITALIC) {
                print!("\\it");
            }

            if style.font_style.intersects(FontStyle::UNDERLINE) {
                print!("\\ul");
            }

            print!("}}{{");

            for c in span.chars() {
                match c {
                    '$' => print!("\\$"),
                    '%' => print!("\\%"),
                    '^' => print!("\\^"),
                    '_' => print!("\\_"),
                    '{' => print!("\\{{"),
                    '}' => print!("\\}}"),
                    '#' => print!("\\#"),
                    '\\' => print!("{{\\textbackslash}}"),
                    '&' => print!("\\&"),
                    '~' => print!("{{\\textasciitilde}}"),
                    ' ' => print!("\\ "),
                    '\n' => print!("\\WebNL\n"), // XXXXXXXXXXXXx
                    other => print!("{}", other),
                }
            }

            print!("}}");
        }

        println!("%");
        print!("\\end{{{}}}{}", env, terminator);
    }
}

/// A trait for measuring how wide some WEB language items that can be rendered
/// in a fully "inline" format.
pub trait RenderInline {
    /// Get how many characters wide the item would be if render all on one
    /// line. Return `NOT_INLINE` if the item should not be rendered in an
    /// inline mode.
    fn measure_inline(&self) -> usize;

    /// Render the item in its inline format.
    fn render_inline(&self, dest: &mut Prettifier);
}

pub const NOT_INLINE: usize = 9999;

impl<T: RenderInline> RenderInline for Box<T> {
    fn measure_inline(&self) -> usize {
        self.deref().measure_inline()
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        self.deref().render_inline(dest)
    }
}

impl<T: RenderInline> RenderInline for &T {
    fn measure_inline(&self) -> usize {
        self.deref().measure_inline()
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        self.deref().render_inline(dest)
    }
}

impl<'a, T: RenderInline> RenderInline for SpanValue<'a, T> {
    fn measure_inline(&self) -> usize {
        self.value.measure_inline()
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        self.value.render_inline(dest)
    }
}

/// Measure how wide a sequence of items will be if rendered inline.
///
/// The items are assumed to be rendered with a separator of width `sep_width`.
pub fn measure_inline_seq<I: IntoIterator<Item = T>, T: RenderInline>(
    seq: I,
    sep_width: usize,
) -> usize {
    let mut n = 0;

    for item in seq.into_iter() {
        if n != 0 {
            n += sep_width;
        }

        n += item.measure_inline();
    }

    n
}

/// Render a sequence of items inline.
pub fn render_inline_seq<I: IntoIterator<Item = T>, T: RenderInline>(
    seq: I,
    sep: &str,
    dest: &mut Prettifier,
) {
    let mut first = true;

    for item in seq.into_iter() {
        if first {
            first = false;
        } else {
            dest.noscope_push(sep);
        }

        item.render_inline(dest);
    }
}

struct ColorHexConvert(Color);

impl fmt::Display for ColorHexConvert {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "rgba({},{},{},{:.2})",
            self.0.r,
            self.0.g,
            self.0.b,
            self.0.a as f32 / 255.0
        )
    }
}
