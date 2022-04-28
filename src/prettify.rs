//! Prettify the Pascal source.

use lazy_static::lazy_static;
use std::{
    fmt::{self, Write},
    str::FromStr,
};
use syntect::{
    highlighting::{Color, FontStyle, HighlightIterator, HighlightState, Highlighter, Theme},
    parsing::{Scope, ScopeStack, ScopeStackOp},
};

use crate::weblang::{base::StringSpan, TypesetComment};

const INITIAL_SCOPES: &str = "source.c";

lazy_static! {
    pub static ref KEYWORD_SCOPE: Scope = Scope::new("keyword.control.c").unwrap();
}

#[derive(Clone, Debug)]
pub struct Prettifier {
    indent: usize,
    remaining_width: usize,
    is_inline: bool,
    newline_needed: bool,
    text: String,
    ops: Vec<(usize, ScopeStackOp)>,
}

impl Prettifier {
    pub fn new_inline(is_inline: bool) -> Self {
        Prettifier {
            indent: 0,
            remaining_width: 60,
            is_inline,
            newline_needed: false,
            text: String::default(),
            ops: Vec::default(),
        }
    }

    #[inline(always)]
    pub fn fits(&self, width: usize) -> bool {
        width <= self.remaining_width
    }

    pub fn indent_block(&mut self) {
        if self.remaining_width > 4 {
            self.indent += 4;
            self.remaining_width -= 4;
        }
    }

    pub fn dedent_block(&mut self) {
        if self.indent > 0 {
            self.indent -= 4;
            self.remaining_width += 4;
        }
    }

    pub fn newline_indent(&mut self) {
        self.text.push('\n');

        for _ in 0..self.indent {
            self.text.push(' ');
        }

        self.newline_needed = false;
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

    pub fn scope_push<S: fmt::Display>(&mut self, scope: Scope, text: S) -> usize {
        self.maybe_newline();

        let n0 = self.text.len();
        self.ops.push((n0, ScopeStackOp::Push(scope)));
        write!(self.text, "{}", text).unwrap();
        let n1 = self.text.len();
        self.ops.push((n1, ScopeStackOp::Pop(1)));
        n1 - n0
    }

    pub fn noscope_push<S: fmt::Display>(&mut self, text: S) {
        // TODO: never use this? Should always have some kine of scope?
        self.maybe_newline();
        write!(self.text, "{}", text).unwrap();
    }

    pub fn space(&mut self) {
        self.text.push(' ');
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
                    '~' => print!("\\~"),
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

/// Measure how wide a comment will be if we typeset it inline, including a
/// leading `// `.
pub fn comment_measure_inline<'a>(comment: &Vec<TypesetComment<'a>>) -> usize {
    let mut n = 3; // `// `

    n += comment.len() - 1; // spaces between items

    for piece in &comment[..] {
        match piece {
            TypesetComment::Tex(s) => {
                // This isn't quite right since we shuld be measuring the width
                // of the comment as rendered, and TeX control sequences won't map
                // directly to that. But it's the best we can do.
                n += s.len();
            }

            TypesetComment::Pascal(toks) => {
                n += toks.len() - 1;

                for tok in &toks[..] {
                    n += tok.to_string().len();
                }
            }
        }
    }

    n
}

pub fn comment_render_inline<'a>(comment: &Vec<TypesetComment<'a>>, dest: &mut Prettifier) {
    dest.noscope_push("//");

    for piece in &comment[..] {
        dest.noscope_push(' ');

        match piece {
            TypesetComment::Tex(s) => {
                // TODO be mindful of TeX escaping here ... maybe
                let mut first = true;

                for word in s.split_whitespace() {
                    if first {
                        first = false;
                    } else {
                        dest.space();
                    }

                    dest.noscope_push(word);
                }
            }

            TypesetComment::Pascal(toks) => {
                let mut first = true;

                for tok in &toks[..] {
                    if first {
                        first = false;
                    } else {
                        dest.noscope_push(' ');
                    }

                    dest.noscope_push(tok);
                }
            }
        }
    }

    dest.newline_needed();
}

pub fn module_reference_render<'a>(mr: &StringSpan<'a>, dest: &mut Prettifier) {
    dest.noscope_push("< ");
    dest.noscope_push(mr.value.as_ref());
    dest.noscope_push(" >");
    dest.newline_needed();
}

#[derive(Debug, Default)]
pub struct PrettifiedCode {}

impl PrettifiedCode {}

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
