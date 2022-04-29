//! Comments

use crate::prettify::{Prettifier, RenderInline};

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

    fn render_inline(&self, dest: &mut Prettifier) {
        dest.noscope_push("//");

        for piece in &self.0[..] {
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
    }
}
