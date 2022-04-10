//! The main app state.

use lexical_sort::{natural_lexical_cmp, StringSort};
use nom::{bytes::complete::take_while, error::ErrorKind};
use nom_locate::position;
use std::{
    collections::{BTreeSet, HashMap},
    convert::TryFrom,
    fmt::Write,
};

use crate::{
    control::ControlKind,
    index::IndexEntryKind,
    parse_base::{new_parse_error, ParseResult, Span, StringSpan},
    reserved::PascalReservedWord,
    token::{next_token, take_until_terminator, Token},
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

    /// Map from full-length module name to the module that initially defines
    /// it. Every entry here also has a record in the index table, where
    /// `is_definition` modules indicate ones that contribute code to the
    /// module.
    named_modules: BTreeSet<String>,

    index_entries: HashMap<String, IndexState>,
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

            for name in self
                .named_modules
                .range(body.to_owned()..)
                .take_while(|n| n.starts_with(body))
            {
                if matched_name.is_empty() {
                    matched_name.push_str(&name);
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
        self.named_modules.insert(text.value.to_string());
        self.add_index_entry(text.value.to_string(), IndexEntryKind::Normal, module);
        Ok((span, text))
    }

    pub fn dump_pass1(&self) {
        for name in self.named_modules.iter() {
            eprintln!("{:?}", name);
        }

        eprintln!();

        let mut index: Vec<_> = self.index_entries.keys().collect();
        index.string_sort_unstable(natural_lexical_cmp);

        for name in &index {
            if self.named_modules.contains(&**name) {
                continue;
            }

            let info = self.index_entries.get(&**name).unwrap();

            let mut refs = String::new();
            for r in &info.refs {
                if !refs.is_empty() {
                    refs.push(' ');
                }

                write!(refs, "{}", r.module).unwrap();

                if r.is_definition {
                    refs.push('*');
                }
            }

            eprintln!("{} ({:?}) => {}", name, info.kind, refs);
        }
    }
}
