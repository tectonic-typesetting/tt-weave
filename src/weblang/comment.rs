//! Comments

use crate::prettify::{Prettifier, RenderInline, COMMENT_SCOPE};

use super::base::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebComment<'a>(pub Vec<TypesetComment<'a>>);

impl<'a> RenderInline for WebComment<'a> {
    fn measure_inline(&self) -> usize {
        let mut n = 3; // `// `

        n += self.0.len() - 1; // spaces between items

        for piece in &self.0[..] {
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

    // Here we cheat and word-wrap even though we should stay all one one line,
    // since we use this function for convenience even when a comment doesn't
    // need to be inline, and we won't need to wrap in contexts where we need to
    // stay inline.
    fn render_inline(&self, dest: &mut Prettifier) {
        dest.with_scope(*COMMENT_SCOPE, |d| {
            d.noscope_push("//");

            for piece in &self.0[..] {
                d.noscope_push(' ');

                match piece {
                    TypesetComment::Tex(s) => {
                        // TODO be mindful of TeX escaping here ... maybe
                        let mut first = true;

                        for word in s.split_whitespace() {
                            if first {
                                first = false;
                            } else {
                                d.space();
                            }

                            if !d.fits(word.len()) {
                                d.newline_needed();
                                d.noscope_push("// ");
                            }

                            d.noscope_push(word);
                        }
                    }

                    TypesetComment::Pascal(toks) => {
                        let mut first = true;

                        for tok in &toks[..] {
                            if first {
                                first = false;
                            } else {
                                d.noscope_push(' ');
                            }

                            if !d.fits(tok.measure_inline()) {
                                d.newline_needed();
                                d.noscope_push("// ");
                            }

                            d.noscope_push(tok);
                        }
                    }
                }
            }
        });
    }
}
