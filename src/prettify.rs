//! Prettify the Pascal source.

use std::{fmt, str::FromStr};
use syntect::{
    highlighting::{Color, FontStyle, HighlightIterator, HighlightState, Highlighter, Theme},
    parsing::{Scope, ScopeStack, ScopeStackOp},
};

#[derive(Debug)]
pub struct PrettifiedCode {
    text: String,
    ops: Vec<(usize, ScopeStackOp)>,
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

    pub fn emit(&self, theme: &Theme) {
        let highlighter = Highlighter::new(theme);
        let initial_stack = ScopeStack::from_str(INITIAL_SCOPES).unwrap();
        let mut hs = HighlightState::new(&highlighter, initial_stack);
        let hi = HighlightIterator::new(&mut hs, &self.ops[..], &self.text[..], &highlighter);

        println!("\\begin{{WebPrettified}}%");

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
                    '\n' => print!("^^J"),
                    other => print!("{}", other),
                }
            }

            print!("}}");
        }

        println!("%");
        println!("\\end{{WebPrettified}}%");
    }
}

struct ColorHexConvert(Color);

impl fmt::Display for ColorHexConvert {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "rgba({:02X},{:02X},{:02X},{:.2})",
            self.0.r,
            self.0.g,
            self.0.b,
            self.0.a as f32 / 255.0
        )
    }
}
