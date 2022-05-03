//! The first pass -- building up the index.

use nom::{bytes::complete::take_while, character::complete::char, error::ErrorKind, Finish};
use tectonic_errors::prelude::*;

use crate::{
    control::ControlKind,
    index::IndexEntryKind,
    parse_base::{new_parse_error, ParseResult, Span, SpanValue},
    pascal_token::{match_pascal_token, PascalToken},
    reserved::PascalReservedWord,
    state::{ModuleId, State},
    token::{next_token, Token},
};

/// Skip the "limbo" section at the start of the WEB file.
///
/// This method approximately corresponds to WEAVE:89, `skip_limbo`.
fn skip_limbo(mut span: Span) -> ParseResult<Token> {
    // I feel like there must be a better way to do this. The way that peeking
    // seems to work in nom, I think we have to loop char-by-char in order to be
    // able to rewind to the '@' when we find our match.
    let mut tok;

    loop {
        (span, tok) = next_token(span)?;

        match tok {
            Token::Char(_) | Token::Control(ControlKind::AtLiteral) => continue,
            Token::Control(ControlKind::NewMajorModule)
            | Token::Control(ControlKind::NewMinorModule) => return Ok((span, tok)),
            _ => return new_parse_error(span, ErrorKind::Char),
        }
    }
}

/// WEAVE:91, `skip_comment`
///
/// Skip over the TeX portion of a comment inside an outer-Pascal section. This
/// needs to keep track of brace balancing because the parsing is not recursive;
/// it just alternates between Pascal modes and TeX modes.
///
/// This method stops skipping when it hits a `|` (indicating that a nested
/// Pascal section is starting), when it hits a `}` that brings the brace
/// balance down to zero, or an (erroneous) new-module control.
fn first_pass_skip_comment<'a>(mut depth: usize, mut span: Span<'a>) -> ParseResult<'a, usize> {
    let mut tok;

    loop {
        (span, tok) = next_token(span)?;

        match tok {
            Token::Char('|') => return Ok((span, depth)),

            Token::Char('\\') => {
                // WEAVE handles '\@' specially in a way that might give
                // different behavior than this, but it looks like that
                // construction basically doesn't arise in practice.
                (span, _) = next_token(span)?;
            }

            Token::Char('{') => {
                depth += 1;
            }

            Token::Char('}') => {
                depth -= 1;

                if depth == 0 {
                    return Ok((span, depth));
                }
            }

            Token::Control(ControlKind::NewMajorModule)
            | Token::Control(ControlKind::NewMinorModule) => {
                return new_parse_error(span, ErrorKind::Char)
            }

            _ => {}
        }
    }
}

/// WEAVE:90, `skip_tex`
///
/// Skip over TeX code at the beginning of a module. Stop when we get to a
/// control code or a `|`.
fn first_pass_skip_tex<'a>(mut span: Span<'a>) -> ParseResult<'a, Token> {
    let mut tok;

    loop {
        (span, tok) = next_token(span)?;

        match tok {
            Token::Char('|') | Token::Control(_) => return Ok((span, tok)),
            _ => {}
        }
    }
}

/// WEAVE:111, `Pascal_xref`
///
/// Read Pascal tokens and store cross-references to identifiers. Reading
/// continues until one of the following tokens is found: `{`, `|`, `@f`, `@d`,
/// `@p`, `@<`, `@ `, or `@*`. Therefore it will stop at comments rather than
/// nesting into them.
fn first_pass_scan_pascal_only<'a>(
    cur_module: ModuleId,
    state: &mut State,
    mut span: Span<'a>,
) -> ParseResult<'a, Token> {
    let mut tok;
    let mut ptok;

    loop {
        (span, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;

        let prev_span = span.clone();
        (span, tok) = next_token(span)?;

        match tok {
            Token::Char('|')
            | Token::Char('{')
            | Token::Control(ControlKind::MacroDefinition)
            | Token::Control(ControlKind::FormatDefinition)
            | Token::Control(ControlKind::StartUnnamedPascal)
            | Token::Control(ControlKind::ModuleName)
            | Token::Control(ControlKind::NewMinorModule)
            | Token::Control(ControlKind::NewMajorModule) => {
                return Ok((span, tok));
            }
            _ => {}
        }

        // Looks like we still have Pascal. Now parse it as such.

        (span, ptok) = match_pascal_token(prev_span, None)?;

        match ptok {
            PascalToken::ReservedWord(SpanValue {
                value: PascalReservedWord::Function,
                ..
            })
            | PascalToken::ReservedWord(SpanValue {
                value: PascalReservedWord::Procedure,
                ..
            })
            | PascalToken::ReservedWord(SpanValue {
                value: PascalReservedWord::Program,
                ..
            })
            | PascalToken::ReservedWord(SpanValue {
                value: PascalReservedWord::Var,
                ..
            }) => {
                state.set_definition_flag(true);
            }

            PascalToken::Identifier(text) => {
                state.add_index_entry(text.value.into_owned(), IndexEntryKind::Normal, cur_module);
            }

            PascalToken::IndexEntry(kind, text) => {
                state.add_index_entry(text.value.into_owned(), kind, cur_module);
            }

            PascalToken::DefinitionFlag => {
                state.set_definition_flag(true);
            }

            PascalToken::CancelDefinitionFlag => {
                state.set_definition_flag(false);
            }

            _ => {}
        }
    }
}

/// WEAVE:112, `outer_xref`
///
/// Like `first_pass_scan_pascal_only`, but at a higher level: it handles
/// comments, which switch to TeX mode, and which may themselves contain `|`s to
/// switch back to Pascal mode.
fn first_pass_scan_pascal<'a>(
    cur_module: ModuleId,
    state: &mut State,
    mut span: Span<'a>,
) -> ParseResult<'a, Token> {
    let mut tok;

    let mut prev_span = span;
    (span, tok) = next_token(span)?;

    loop {
        match tok {
            Token::Char('{') => {
                // Start a comment. Start alternating between TeX and inner-Pascal
                // until it fully ends.
                let mut depth;
                (span, depth) = first_pass_skip_comment(1, span)?;

                while depth > 0 {
                    (span, tok) = first_pass_scan_pascal_only(cur_module, state, span)?;

                    if let Token::Char('|') = tok {
                        (span, depth) = first_pass_skip_comment(depth, span)?;
                    } else {
                        return new_parse_error(span, ErrorKind::Char);
                    }
                }

                prev_span = span;
                (span, tok) = next_token(span)?;
            }

            Token::Control(ControlKind::MacroDefinition)
            | Token::Control(ControlKind::FormatDefinition)
            | Token::Control(ControlKind::StartUnnamedPascal)
            | Token::Control(ControlKind::ModuleName)
            | Token::Control(ControlKind::NewMinorModule)
            | Token::Control(ControlKind::NewMajorModule) => {
                return Ok((span, tok));
            }

            _ => {
                (span, tok) = first_pass_scan_pascal_only(cur_module, state, prev_span)?;
            }
        }
    }
}

/// See WEAVE:90, WEAVE:113. We basically skip over TeX, but parse Pascal spans
/// (delimited by `|`) and index entries.
fn first_pass_handle_tex<'a>(
    cur_module: ModuleId,
    state: &mut State,
    mut span: Span<'a>,
) -> ParseResult<'a, Token> {
    let mut tok;

    (span, tok) = first_pass_skip_tex(span)?;

    loop {
        match tok {
            Token::Control(ControlKind::NewMajorModule)
            | Token::Control(ControlKind::NewMinorModule)
            | Token::Control(ControlKind::StartUnnamedPascal)
            | Token::Control(ControlKind::ModuleName)
            | Token::Control(ControlKind::MacroDefinition)
            | Token::Control(ControlKind::FormatDefinition) => {
                return Ok((span, tok));
            }

            Token::Control(ControlKind::RomanIndexEntry) => {
                (span, tok) = state.scan_add_next(IndexEntryKind::Roman, cur_module, span)?;
            }
            Token::Control(ControlKind::TypewriterIndexEntry) => {
                (span, tok) = state.scan_add_next(IndexEntryKind::Typewriter, cur_module, span)?;
            }
            Token::Control(ControlKind::WildcardIndexEntry) => {
                (span, tok) = state.scan_add_next(IndexEntryKind::Wildcard, cur_module, span)?;
            }

            Token::Char('|') => {
                (span, _) = first_pass_scan_pascal_only(cur_module, state, span)?;
                (span, tok) = first_pass_skip_tex(span)?;
            }

            _ => {
                (span, tok) = first_pass_skip_tex(span)?;
            }
        }
    }
}

/// See WEAVE:115-116. Definitions are pretty simple structurally.
fn first_pass_handle_definitions<'a>(
    cur_module: ModuleId,
    state: &mut State,
    mut span: Span<'a>,
    mut tok: Token,
) -> ParseResult<'a, Token> {
    loop {
        match tok {
            Token::Control(ControlKind::NewMajorModule)
            | Token::Control(ControlKind::NewMinorModule)
            | Token::Control(ControlKind::StartUnnamedPascal)
            | Token::Control(ControlKind::ModuleName) => {
                return Ok((span, tok));
            }

            Token::Control(ControlKind::MacroDefinition) => {
                state.set_definition_flag(true);
                (span, tok) = first_pass_scan_pascal(cur_module, state, span)?;
            }

            Token::Control(ControlKind::FormatDefinition) => {
                let mut ptok;

                state.set_definition_flag(true);
                (span, ptok) = match_pascal_token(span, None)?;

                if let PascalToken::Identifier(text) = ptok {
                    state.add_index_entry(
                        text.value.to_owned(),
                        IndexEntryKind::Normal,
                        cur_module,
                    );
                    (span, ptok) = match_pascal_token(span, None)?;

                    if let PascalToken::Equivalence = ptok {
                        (span, ptok) = match_pascal_token(span, None)?;

                        if let PascalToken::ReservedWord(sv) = ptok {
                            state.add_formatted_identifier(text.value.into_owned(), sv.value);
                            state.add_index_entry(
                                sv.value.to_string(),
                                IndexEntryKind::Normal,
                                cur_module,
                            );
                        }
                    }
                }

                (span, tok) = first_pass_scan_pascal(cur_module, state, span)?;
            }

            Token::Control(ControlKind::RomanIndexEntry) => {
                (span, tok) = state.scan_add_next(IndexEntryKind::Roman, cur_module, span)?;
            }
            Token::Control(ControlKind::TypewriterIndexEntry) => {
                (span, tok) = state.scan_add_next(IndexEntryKind::Typewriter, cur_module, span)?;
            }
            Token::Control(ControlKind::WildcardIndexEntry) => {
                (span, tok) = state.scan_add_next(IndexEntryKind::Wildcard, cur_module, span)?;
            }

            Token::Char('|') => {
                (span, tok) = first_pass_scan_pascal_only(cur_module, state, span)?;
            }

            _ => {
                (span, tok) = next_token(span)?;
            }
        }
    }
}

/// See WEAVE:117. Full-Pascal sections are also pretty straightforward with our
/// machinery.
fn first_pass_handle_pascal<'a>(
    cur_module: ModuleId,
    state: &mut State,
    mut span: Span<'a>,
) -> ParseResult<'a, Token> {
    let mut tok;

    let mut prev_span = span.clone();
    (span, tok) = next_token(span)?;

    loop {
        match tok {
            Token::Control(ControlKind::NewMajorModule)
            | Token::Control(ControlKind::NewMinorModule) => {
                return Ok((span, tok));
            }

            Token::Control(ControlKind::ModuleName) => {
                state.set_definition_flag(false);
                (span, _) = state.scan_module_name_and_register(cur_module, span)?;
                prev_span = span.clone();
                (span, tok) = next_token(span)?;
            }

            _ => {
                (span, tok) = first_pass_scan_pascal(cur_module, state, prev_span)?;
            }
        }
    }
}

fn first_pass_inner<'a>(state: &mut State, span: Span<'a>) -> ParseResult<'a, ()> {
    let (mut span, mut tok) = skip_limbo(span)?;
    let mut cur_module: ModuleId = 0;

    loop {
        // At the top of this loop, we've just read a new-module boundary token.
        // At the moment we don't really care about major vs minor.

        cur_module += 1;

        match tok {
            Token::Control(ControlKind::NewMajorModule) => {
                eprintln!("- Major module #{}", cur_module);
            }

            Token::Control(ControlKind::NewMinorModule) => {}

            _ => {
                eprintln!("unexpected module end {:?}", tok);
                return new_parse_error(span, ErrorKind::Complete);
            }
        }

        // Handle the TeX chunk (which can be empty), and find out what ended it.

        (span, tok) = first_pass_handle_tex(cur_module, state, span)?;

        // If there are macro/format definitions, handle those

        match tok {
            Token::Control(ControlKind::MacroDefinition)
            | Token::Control(ControlKind::FormatDefinition) => {
                (span, tok) = first_pass_handle_definitions(cur_module, state, span, tok)?;
            }

            _ => {}
        }

        // If there's Pascal, handle that

        match tok {
            Token::Control(ControlKind::StartUnnamedPascal) => {
                (span, tok) = first_pass_handle_pascal(cur_module, state, span)?;
            }

            Token::Control(ControlKind::ModuleName) => {
                state.set_definition_flag(true);
                (span, _) = state.scan_module_name_and_register(cur_module, span)?;

                // there's like one module in XeTeX with a space between module name and equals sign
                (span, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;
                (span, _) = char('=')(span)?;
                (span, tok) = first_pass_handle_pascal(cur_module, state, span)?;
            }

            _ => {}
        }
    }
}

pub fn execute(span: Span) -> Result<State> {
    let mut state = State::default();

    match first_pass_inner(&mut state, span).finish() {
        Ok((_remainder, _value)) => {}
        Err((_remainder, ErrorKind::Eof)) => {}
        Err((_remainder, kind)) => return Err(anyhow!(kind.description().to_owned())),
    }

    state.compute_module_ids();
    Ok(state)
}
