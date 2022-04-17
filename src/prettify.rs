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

use crate::{pass2::WebToken, reserved::PascalReservedWord};

lazy_static! {
    static ref KEYWORD_SCOPE: Scope = Scope::new("keyword.control.c").unwrap();
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct FormatContext {
    indent: usize,
    remaining_width: usize,
}

impl FormatContext {
    fn indent(&self, amount: usize) -> Option<Self> {
        if amount >= self.remaining_width {
            None
        } else {
            Some(FormatContext {
                indent: self.indent + amount,
                remaining_width: self.remaining_width - amount,
            })
        }
    }

    fn indent_default(&self) -> Option<Self> {
        self.indent(4)
    }
}

impl FormatContext {
    fn new() -> Self {
        FormatContext {
            indent: 0,
            remaining_width: 60,
        }
    }
}

#[derive(Debug, Default)]
pub struct PrettifiedCode {
    text: String,
    ops: Vec<(usize, ScopeStackOp)>,
}

impl PrettifiedCode {
    fn scope_push<S: fmt::Display>(&mut self, scope: Scope, text: S) -> usize {
        let n0 = self.text.len();
        self.ops.push((n0, ScopeStackOp::Push(scope)));
        write!(self.text, "{}", text).unwrap();
        let n1 = self.text.len();
        self.ops.push((n1, ScopeStackOp::Pop(1)));
        n1 - n0
    }

    fn space(&mut self) -> usize {
        self.text.push(' ');
        1
    }
}

fn prettify(code: &[WebToken], context: &FormatContext) -> Option<PrettifiedCode> {
    // No tokens? No problem!
    if code.is_empty() {
        return Some(PrettifiedCode::default());
    }

    // Top-level constructs

    if code[0].is_reserved_word(PascalReservedWord::Define) {
        return prettify_define_statement(code, context);
    }

    None
}

/// `@define sym(#) === anything ...`
///
/// When called we know that code[0] is the @define token.
fn prettify_define_statement(code: &[WebToken], context: &FormatContext) -> Option<PrettifiedCode> {
    let mut pc = PrettifiedCode::default();

    let mut amount = pc.scope_push(*KEYWORD_SCOPE, code[0].as_pascal().unwrap());
    amount += pc.space();

    Some(pc)
}

const INITIAL_SCOPES: &str = "source.c";

impl PrettifiedCode {
    pub fn new<S: ToString>(text: S) -> Self {
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
                    '\n' => print!("{{\\newline}}"),
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
