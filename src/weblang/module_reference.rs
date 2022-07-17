//! A reference to a WEB module

use crate::prettify::{Prettifier, RenderInline};

use super::base::*;

/// A reference to a WEB module.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebModuleReference<'a> {
    pub name: StringSpan<'a>,
    pub id: ModuleId,
}

/// Expect a module reference, returning its value.
pub fn parse_module_reference<'a>(
    input: ParseInput<'a>,
) -> ParseResult<'a, WebModuleReference<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::ModuleReference(mr) = wt {
        Ok((input, mr))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedIdentifier);
    }
}

impl<'a> RenderInline for WebModuleReference<'a> {
    fn measure_inline(&self) -> usize {
        self.name.len() + 3 + self.id.to_string().len()
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        dest.insert(TexInsert::StartModuleReference(self.id), true);
        dest.noscope_push(format!("⟦{} ", self.id));
        dest.noscope_push(self.name.value.as_ref());
        dest.noscope_push("⟧");
        dest.insert(TexInsert::EndMacro, false);
    }
}
