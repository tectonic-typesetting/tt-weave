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

fn second_pass_inner<'a>(state: &State, span: Span<'a>) -> ParseResult<'a, ()> {
    let (mut span, mut tok) = copy_limbo(span)?;
    let mut cur_module: ModuleId = 0;

    //loop {
    //    // At the top of this loop, we've just read a new-module boundary token.
    //    // At the moment we don't really care about major vs minor.
    //
    //    cur_module += 1;
    //
    //    match tok {
    //        Token::Control(ControlKind::NewMajorModule) => {
    //            println!("- Major module #{}", cur_module);
    //        }
    //
    //        Token::Control(ControlKind::NewMinorModule) => {}
    //
    //        _ => {
    //            eprintln!("unexpected module end {:?}", tok);
    //            return new_parse_error(span, ErrorKind::Complete);
    //        }
    //    }
    //
    //    // Handle the TeX chunk (which can be empty), and find out what ended it.
    //
    //    (span, tok) = first_pass_handle_tex(cur_module, state, span)?;
    //
    //    // If there are macro/format definitions, handle those
    //
    //    match tok {
    //        Token::Control(ControlKind::MacroDefinition)
    //        | Token::Control(ControlKind::FormatDefinition) => {
    //            (span, tok) = first_pass_handle_definitions(cur_module, state, span, tok)?;
    //        }
    //
    //        _ => {}
    //    }
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
    //}

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
