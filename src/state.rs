//! The main app state.

use lexical_sort::{natural_lexical_cmp, StringSort};
use nom::{bytes::complete::take_while, error::ErrorKind};
use nom_locate::position;
use std::{
    collections::{btree_map::Entry, BTreeMap, HashMap},
    convert::TryFrom,
};

use crate::{
    control::ControlKind,
    index::IndexEntryKind,
    parse_base::{new_parse_error, ParseResult, Span, StringSpan},
    pascal_token::{match_pascal_token, FormatOverrides, PascalToken},
    reserved::PascalReservedWord,
    token::{next_token, take_until_terminator, Token},
    weblang::module_reference::WebModuleReference,
};

pub type ModuleId = usize;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Reference {
    pub module: ModuleId,
    pub is_definition: bool,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct IndexState {
    pub kind: IndexEntryKind,
    pub refs: Vec<Reference>,
}

impl IndexState {
    fn new(kind: IndexEntryKind) -> Self {
        IndexState {
            kind,
            refs: Vec::default(),
        }
    }
}

#[derive(Debug, Default)]
pub struct State {
    definition_flag: bool,

    /// Sorted map of module names to their canonical ID numbers. The sorting
    /// allows us to look up partial names. Every entry here also has a record
    /// in the index table, where `is_definition` entries indicate ones that
    /// contribute code to the module. During the first pass the ID numbers are
    /// just zeros: module names might be referenced before they're actually
    /// defined, so we can't know their "real" module-IDs before we've finished
    /// the pass.
    named_modules: BTreeMap<String, ModuleId>,

    index_entries: HashMap<String, IndexState>,

    formatted_identifiers: FormatOverrides,
}

impl State {
    pub fn add_index_entry<S: Into<String>>(
        &mut self,
        text: S,
        kind: IndexEntryKind,
        module: ModuleId,
    ) {
        let text = text.into(); // sigh - rust-lang/rust#51604

        if !self.definition_flag {
            if text.len() == 1 || PascalReservedWord::try_from(&text[..]).is_ok() {
                return;
            }
        }

        let refs = &mut self
            .index_entries
            .entry(text)
            .or_insert(IndexState::new(kind))
            .refs;

        for existing in refs.iter_mut() {
            if existing.module == module {
                existing.is_definition |= self.definition_flag;
                self.definition_flag = false;
                return;
            }
        }

        refs.push(Reference {
            module,
            is_definition: self.definition_flag,
        });
        self.definition_flag = false;
    }

    pub fn set_definition_flag(&mut self, f: bool) {
        self.definition_flag = f;
    }

    pub fn add_formatted_identifier<S: Into<String>>(
        &mut self,
        text: S,
        equiv: PascalReservedWord,
    ) {
        self.formatted_identifiers.insert(text.into(), equiv);
    }

    pub fn match_pascal_token_with_formats<'a>(
        &self,
        span: Span<'a>,
    ) -> ParseResult<'a, PascalToken<'a>> {
        match_pascal_token(span, Some(&self.formatted_identifiers))
    }

    pub fn scan_next<'a>(&self, span: Span<'a>) -> ParseResult<'a, Token> {
        let (span, _) = take_until_terminator(span)?;
        next_token(span)
    }

    pub fn scan_add_next<'a>(
        &mut self,
        kind: IndexEntryKind,
        module: ModuleId,
        span: Span<'a>,
    ) -> ParseResult<'a, Token> {
        let (span, text) = take_until_terminator(span)?;
        self.add_index_entry(text.value.into_owned(), kind, module);
        next_token(span)
    }

    /// Scan the name of a WEB module.
    ///
    /// Module names are terminated by a terminator control token. Leading and
    /// trailing whitespace are eaten, and inner whitespace is collapsed.
    ///
    /// See WEAVE:103-104.
    pub fn scan_module_name<'a>(&self, span: Span<'a>) -> ParseResult<'a, StringSpan<'a>> {
        let (span, start) = position(span)?;
        let (mut span, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;

        let mut value = String::new();
        let mut space_needed = false;
        let mut tok;

        loop {
            (span, tok) = next_token(span)?;

            match tok {
                Token::Char(' ') | Token::Char('\t') | Token::Char('\n') => {
                    space_needed = true;
                }

                Token::Control(ControlKind::Terminator) => {
                    break;
                }

                _ => {
                    if space_needed {
                        value.push(' ');
                        space_needed = false;
                    }

                    tok.push_syntax_into(&mut value);
                }
            }
        }

        let (span, end) = position(span)?;

        // Now (ab)use the module name table to do the prefix match if we need to

        let matched_name = if let Some(body) = value.strip_suffix("...") {
            let mut matched_name = String::new();

            for item in self
                .named_modules
                .range(body.to_owned()..)
                .take_while(|i| i.0.starts_with(body))
            {
                if matched_name.is_empty() {
                    matched_name.push_str(&item.0);
                } else {
                    return new_parse_error(span, ErrorKind::Fail);
                }
            }
            if matched_name.is_empty() {
                return new_parse_error(span, ErrorKind::Fail);
            }

            matched_name
        } else {
            value
        };

        Ok((
            span,
            StringSpan {
                start,
                end,
                value: matched_name.into(),
            },
        ))
    }

    pub fn scan_module_name_and_register<'a>(
        &mut self,
        module: ModuleId,
        span: Span<'a>,
    ) -> ParseResult<'a, StringSpan<'a>> {
        let (span, text) = self.scan_module_name(span)?;

        // This might be a reference to the module name but not its actual
        // definition, so we should just insert a placeholder module ID for now.
        self.named_modules.insert(text.value.to_string(), 0);

        self.add_index_entry(text.value.to_string(), IndexEntryKind::Normal, module);
        Ok((span, text))
    }

    /// For the second pass: scan a module name and resolve it to a full module
    /// reference, looking up the module-id.
    pub fn scan_module_reference<'a>(
        &self,
        span: Span<'a>,
    ) -> ParseResult<'a, WebModuleReference<'a>> {
        let (span, name) = self.scan_module_name(span)?;

        let id = match self.named_modules.get(name.value.as_ref()) {
            Some(i) => *i,
            None => return new_parse_error(span, ErrorKind::Fail),
        };

        Ok((span, WebModuleReference { name, id }))
    }

    pub fn compute_module_ids(&mut self) {
        for (name, info) in &self.index_entries {
            if let Entry::Occupied(mut occ) = self.named_modules.entry(name.clone()) {
                for r in &info.refs {
                    if r.is_definition {
                        occ.insert(r.module);
                        break;
                    }
                }
            }
        }
    }

    /// Emit the index of named modules.
    ///
    /// The structure of the emitted TeX is:
    ///
    /// ```
    /// \begin{WebModuleIndex}
    ///   \WebModuleIndexEntry{$id}{$name}{
    ///     % Modules contributing to the definition of the code:
    ///     \mref{$id1}
    ///     \mref{$id2}
    ///     % etc.
    ///   }{
    ///     % Modules referencing the named module:
    ///     \mref{$id3}
    ///     \mref{$id4}
    ///     % etc
    ///   }
    ///   \WebModuleIndexEntry{$id2}{$name2}{...etc...}
    /// \end{WebModuleIndex}
    /// ```
    ///
    /// So, you should define an environment for the index, and 4-parameter
    /// command for dealing with each index entry. The command should define a
    /// `\mref` helper macro to do whatever makes sense for your implementation.
    ///
    /// Note that the index will be sorted by module name, not module id!
    pub fn emit_module_index(&self) {
        println!();
        println!("\\begin{{WebModuleIndex}}");

        for (name, id) in self.named_modules.iter() {
            println!("  \\WebModuleIndexEntry{{{}}}{{{}}}{{%", id, name);

            if let Some(ixstate) = self.index_entries.get(&**name) {
                for r in &ixstate.refs {
                    if r.is_definition {
                        println!("    \\mref{{{}}}%", r.module);
                    }
                }
            }

            println!("  }}{{%");

            if let Some(ixstate) = self.index_entries.get(&**name) {
                for r in &ixstate.refs {
                    if !r.is_definition {
                        println!("    \\mref{{{}}}%", r.module);
                    }
                }
            }

            println!("  }}%");
        }

        println!("\\end{{WebModuleIndex}}");
    }

    /// Emit the index of non-module symbols.
    ///
    /// This has the same structure as the module index, but with `Symbol`
    /// instead of `Module` for various control-sequence names. The form of the
    /// index entry command is:
    ///
    /// ```tex
    /// \WebSymbolIndexEntry{$text}{$kind}{$definers}{$references}
    /// ```
    ///
    /// where $text is the index entry text and $kind is one of the four
    /// following control sequences: `\code`, `\prose`, `\output`, or `\custom`.
    /// $defines and $references are sequences of `\mref` strings as in the
    /// named module index. *Most* symbols have one definition, but some have
    /// zero (including `\output` strings) and some have multiple (especially
    /// variables with single-character names).
    pub fn emit_symbol_index(&self) {
        println!();
        println!("\\begin{{WebSymbolIndex}}");

        let mut index: Vec<_> = self.index_entries.keys().collect();
        index.string_sort_unstable(natural_lexical_cmp);

        for name in &index {
            // Named modules are dealt with separately.
            if self.named_modules.contains_key(&**name) {
                continue;
            }

            let info = self.index_entries.get(&**name).unwrap();

            let kind = match info.kind {
                IndexEntryKind::Normal => "code",
                IndexEntryKind::Roman => "prose",
                IndexEntryKind::Typewriter => "output",
                IndexEntryKind::Wildcard => "custom",
            };

            println!("  \\WebSymbolIndexEntry{{{}}}{{\\{}}}{{%", name, kind);

            for r in &info.refs {
                if r.is_definition {
                    println!("    \\mref{{{}}}%", r.module);
                }
            }

            println!("  }}{{%");

            for r in &info.refs {
                if !r.is_definition {
                    println!("    \\mref{{{}}}%", r.module);
                }
            }

            println!("  }}");
        }

        println!("\\end{{WebSymbolIndex}}");
    }
}
