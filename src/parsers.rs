//! Parsers for the WEB inputs.

use nom::{
    error::{ErrorKind, ParseError as NomParseError},
    Err, Finish, IResult, InputIter, Slice,
};
use tectonic_errors::prelude::*;

use crate::Span;

type ParseError<'a> = (Span<'a>, ErrorKind);
type ParseResult<'a, T> = IResult<Span<'a>, T, ParseError<'a>>;

fn new_parse_error<'a, T>(i: Span<'a>, k: ErrorKind) -> ParseResult<'a, T> {
    Err(Err::Error(ParseError::from_error_kind(i, k)))
}

/// Skip the "limbo" section at the start of the WEB file.
///
/// This method approximately corresponds to WEAVE:89, `skip_limbo`.
fn skip_limbo(mut input: Span) -> ParseResult<()> {
    // I feel like there must be a better way to do this.
    loop {
        let prev_input = input.clone();

        match input.iter_elements().next() {
            Some(c) => {
                input = input.slice(1..);
                if c != '@' {
                    continue;
                }
            }

            None => return new_parse_error(input, ErrorKind::Eof),
        }

        match input.iter_elements().next() {
            Some(c) => {
                input = input.slice(1..);

                if c == ' ' || c == '\t' || c == '*' {
                    return Ok((prev_input, ()));
                }
            }

            None => return new_parse_error(input, ErrorKind::Eof),
        }
    }
}

pub fn first_pass(input: Span) -> Result<()> {
    match skip_limbo(input).finish() {
        //Ok((_remainder, _value)) => Ok(()),
        Ok((remainder, _value)) => {
            println!("Stopped at: {}", &remainder[..32]);
            Ok(())
        }
        Err((_remainder, kind)) => Err(anyhow!(kind.description().to_owned())),
    }
}
