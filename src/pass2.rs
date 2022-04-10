//! The second pass -- emitting TeX

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

/// WEAVE:132, `copy_limbo`, or so.
fn copy_limbo(mut span: Span) -> ParseResult<Token> {
    let mut tok;

    loop {
        (span, tok) = next_token(span)?;

        match tok {
            Token::Char(c) => {
                print!("{}", c);
            }

            Token::Control(ControlKind::AtLiteral) => {
                print!("@");
            }

            Token::Control(ControlKind::NewMajorModule)
            | Token::Control(ControlKind::NewMinorModule) => return Ok((span, tok)),

            _ => return new_parse_error(span, ErrorKind::Char),
        }
    }
}

/// WEAVE:134, `copy_tex`
///
/// TODO: may need to monitor linebreaks as in WEAVE finish_line, etc.,
/// to produce correct output with index entries.
fn copy_tex<'a>(mut span: Span<'a>) -> ParseResult<'a, Token> {
    let mut tok;

    loop {
        (span, tok) = next_token(span)?;

        match tok {
            Token::Char('|') | Token::Control(_) => return Ok((span, tok)),
            Token::Char(c) => {
                print!("{}", c);
            }
        }
    }
}

/// WEAVE:136, `copy_comment`
fn copy_comment<'a>(mut depth: usize, mut span: Span<'a>) -> ParseResult<'a, (String, usize)> {
    let mut text = String::new();
    let mut tok;

    loop {
        (span, tok) = next_token(span)?;

        match tok {
            Token::Char('|') => return Ok((span, (text, depth))),

            Token::Char('\\') => {
                // WEAVE handles '\@' specially in a way that might give
                // different behavior than this, but it looks like that
                // construction basically doesn't arise in practice.
                text.push('\\');

                let mut c;
                (span, c) = next_token(span)?;
                c.push_syntax_into(&mut text);
            }

            Token::Char('{') => {
                text.push('{');
                depth += 1;
            }

            Token::Char('}') => {
                text.push('}');
                depth -= 1;

                if depth == 0 {
                    return Ok((span, (text, depth)));
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

fn scan_pascal_only<'a>(mut span: Span<'a>) -> ParseResult<'a, (Vec<PascalToken<'a>>, Token)> {
    let mut ptoks = Vec::new();
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
                return Ok((span, (ptoks, tok)));
            }
            _ => {}
        }

        // Looks like we still have Pascal. Now parse it as such.

        (span, ptok) = match_pascal_token(prev_span)?;

        match ptok {
            PascalToken::IndexEntry(_, _)
            | PascalToken::DefinitionFlag
            | PascalToken::CancelDefinitionFlag => {}

            other => {
                ptoks.push(other);
            }
        }
    }
}

/// WEAVE:222
fn handle_tex<'a>(
    cur_module: ModuleId,
    state: &State,
    mut span: Span<'a>,
) -> ParseResult<'a, Token> {
    let mut tok;

    (span, tok) = copy_tex(span)?;

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

            Token::Char('|') => {
                (span, _) = scan_pascal_only(span)?;
                (span, tok) = copy_tex(span)?;
            }

            Token::Char(c) => {
                print!("{}", c);
                (span, tok) = copy_tex(span)?;
            }

            Token::Control(ControlKind::AtLiteral) => {
                print!("@");
                (span, tok) = copy_tex(span)?;
            }

            Token::Control(ControlKind::RomanIndexEntry)
            | Token::Control(ControlKind::TypewriterIndexEntry)
            | Token::Control(ControlKind::WildcardIndexEntry) => {
                (span, tok) = state.scan_next(span)?;
            }

            Token::Control(ControlKind::DefinitionFlag) => {
                (span, tok) = copy_tex(span)?;
            }

            other => {
                //(span, tok) = copy_tex(span)?;
                eprintln!("** unhandled TeX control code in pass2: {:?}", other);
                return new_parse_error(span, ErrorKind::Char);
            }
        }
    }
}

/// WEAVE:218, WEAVE:220, etc.
fn second_pass_inner<'a>(state: &State, span: Span<'a>) -> ParseResult<'a, ()> {
    let (mut span, mut tok) = copy_limbo(span)?;
    let mut cur_module: ModuleId = 0;

    loop {
        // At the top of this loop, we've just read a new-module boundary token.
        // At the moment we don't really care about major vs minor.
        cur_module += 1;
        match tok {
            Token::Control(ControlKind::NewMajorModule) => {
                print!("\n\\WebMajorModule{{{}}} ", cur_module);
            }
            Token::Control(ControlKind::NewMinorModule) => {
                print!("\n\\WebMinorModule{{{}}} ", cur_module);
            }
            _ => {
                eprintln!("unexpected module end {:?}", tok);
                return new_parse_error(span, ErrorKind::Complete);
            }
        }
        // Handle the TeX chunk (which can be empty), and find out what ended it.
        (span, tok) = handle_tex(cur_module, state, span)?;

        // If there are macro/format definitions, handle those
        //match tok {
        //    Token::Control(ControlKind::MacroDefinition)
        //    | Token::Control(ControlKind::FormatDefinition) => {
        //        (span, tok) = first_pass_handle_definitions(cur_module, state, span, tok)?;
        //    }
        //    _ => {}
        //}
        //
        //    // If there's Pascal, handle that
        //
        //    match tok {
        //        Token::Control(ControlKind::StartUnnamedPascal) => {
        //            (span, tok) = first_pass_handle_pascal(cur_module, state, span)?;
        //        }
        //
        //        Token::Control(ControlKind::ModuleName) => {
        //            state.set_definition_flag(true);
        //            (span, _) = state.scan_module_name_and_register(cur_module, span)?;
        //
        //            // there's like one module in XeTeX with a space between module name and equals sign
        //            (span, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;
        //            (span, _) = char('=')(span)?;
        //            (span, tok) = first_pass_handle_pascal(cur_module, state, span)?;
        //        }
        //
        //        _ => {}
        //    }
    }

    Ok((span, ()))
}

pub fn execute(state: &State, span: Span) -> Result<()> {
    match second_pass_inner(state, span).finish() {
        Ok((_remainder, _value)) => {}
        Err((_remainder, ErrorKind::Eof)) => {}
        Err((_remainder, kind)) => return Err(anyhow!(kind.description().to_owned())),
    }

    Ok(())
}
