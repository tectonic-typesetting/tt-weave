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

use crate::weblang::TypesetComment;

const INITIAL_SCOPES: &str = "source.c";

lazy_static! {
    static ref KEYWORD_SCOPE: Scope = Scope::new("keyword.control.c").unwrap();
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct FormatContext {
    pub indent: usize,
    pub remaining_width: usize,
    pub is_inline: bool,
}

impl FormatContext {
    pub fn new_inline(is_inline: bool) -> Self {
        FormatContext {
            indent: 0,
            remaining_width: 60,
            is_inline,
        }
    }

    fn indent(&self, amount: usize) -> Option<Self> {
        if amount >= self.remaining_width {
            None
        } else {
            Some(FormatContext {
                indent: self.indent + amount,
                remaining_width: self.remaining_width - amount,
                ..*self
            })
        }
    }

    fn indent_default(&self) -> Option<Self> {
        self.indent(4)
    }

    #[inline(always)]
    pub fn fits(&self, width: usize) -> bool {
        width <= self.remaining_width
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

pub fn comment_render_inline<'a>(
    comment: &Vec<TypesetComment<'a>>,
    _ctxt: &FormatContext,
    dest: &mut PrettifiedCode,
) {
    dest.noscope_push("//");

    for piece in &comment[..] {
        dest.noscope_push(' ');

        match piece {
            TypesetComment::Tex(s) => {
                // TODO TeX escaping???
                dest.noscope_push(s)
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
}

#[derive(Debug, Default)]
pub struct PrettifiedCode {
    text: String,
    ops: Vec<(usize, ScopeStackOp)>,
}

impl PrettifiedCode {
    pub fn scope_push<S: fmt::Display>(&mut self, scope: Scope, text: S) -> usize {
        let n0 = self.text.len();
        self.ops.push((n0, ScopeStackOp::Push(scope)));
        write!(self.text, "{}", text).unwrap();
        let n1 = self.text.len();
        self.ops.push((n1, ScopeStackOp::Pop(1)));
        n1 - n0
    }

    pub fn noscope_push<S: fmt::Display>(&mut self, text: S) {
        // TODO: never use this? Should always have some kine of scope?
        write!(self.text, "{}", text).unwrap();
    }

    pub fn space(&mut self) -> usize {
        self.text.push(' ');
        1
    }

    pub fn newline(&mut self, ctxt: &FormatContext) {
        if !ctxt.is_inline {
            self.text.push('\n');
        }
    }

    pub fn toplevel_separator(&mut self) {
        self.text.push_str("\n\n");
    }

    pub fn new_placeholder<S: ToString>(text: S) -> Self {
        let text = text.to_string();

        // TEMP!
        let n = text.len();
        let tmp = Scope::new("keyword.control.c").unwrap();

        PrettifiedCode {
            text,
            ops: vec![(0, ScopeStackOp::Push(tmp)), (n, ScopeStackOp::Pop(1))],
        }
    }

    pub fn emit(&self, theme: &Theme, inline: bool) {
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
                    '\n' => print!("\n"), // XXXXXXXXXXXXx
                    other => print!("{}", other),
                }
            }

            print!("}}");
        }

        println!("%");
        print!("\\end{{{}}}{}", env, terminator);
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
