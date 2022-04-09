//! Low-level tokens out of WEB files.

use nom::{
    error::{ErrorKind, ParseError as NomParseError},
    Err, InputIter, InputTake, Slice,
};
use nom_locate::position;
use std::borrow::Cow;

use crate::{
    control::ControlKind,
    index::IndexEntryKind,
    parse_base::{new_parse_error, ParseError, ParseResult, Span, StringSpan},
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Token {
    Char(char),
    Control(ControlKind),
}

impl Token {
    pub fn n_chars(&self) -> usize {
        match self {
            Token::Char(_) => 1,
            Token::Control(_) => 2,
        }
    }

    pub fn push_syntax_into(&self, s: &mut String) {
        match self {
            Token::Char(c) => {
                s.push(*c);
            }
            Token::Control(k) => {
                s.push('@');
                s.push(k.syntax_char());
            }
        }
    }

    pub fn as_index_kind(&self) -> Option<IndexEntryKind> {
        match self {
            Token::Control(k) => k.as_index_kind(),
            Token::Char(_) => None,
        }
    }
}

pub fn next_token(span: Span) -> ParseResult<Token> {
    let mut it = span.iter_elements();
    let c = it
        .next()
        .ok_or_else(|| Err::Error(ParseError::from_error_kind(span, ErrorKind::Eof)))?;

    if c != '@' {
        return Ok((span.slice(1..), Token::Char(c)));
    }

    let c = it
        .next()
        .ok_or_else(|| Err::Error(ParseError::from_error_kind(span, ErrorKind::Eof)))?;

    let k = match c {
        '>' => ControlKind::Terminator,
        '@' => ControlKind::AtLiteral,
        ' ' => ControlKind::NewMinorModule,
        '\t' => ControlKind::NewMinorModule,
        '\n' => ControlKind::NewMinorModule, // this happens in XeTeX
        '*' => ControlKind::NewMajorModule,
        'd' => ControlKind::MacroDefinition,
        'D' => ControlKind::MacroDefinition,
        'f' => ControlKind::FormatDefinition,
        'F' => ControlKind::FormatDefinition,
        'p' => ControlKind::StartUnnamedPascal,
        'P' => ControlKind::StartUnnamedPascal,
        '<' => ControlKind::ModuleName,
        '\'' => ControlKind::OctalLiteral,
        '\"' => ControlKind::HexLiteral,
        '$' => ControlKind::StringPoolChecksum,
        '{' => ControlKind::BeginMetaComment,
        '}' => ControlKind::EndMetaComment,
        '&' => ControlKind::PasteText,
        '^' => ControlKind::RomanIndexEntry,
        '.' => ControlKind::TypewriterIndexEntry,
        ':' => ControlKind::WildcardIndexEntry,
        't' => ControlKind::TexAnnotation,
        '=' => ControlKind::VerbatimPascal,
        '\\' => ControlKind::PascalForceEol,
        '!' => ControlKind::DefinitionFlag,
        '?' => ControlKind::CancelDefinitionFlag,
        ',' => ControlKind::FormatThinSpace,
        '/' => ControlKind::FormatBreak,
        '|' => ControlKind::FormatOptionalBreak,
        '#' => ControlKind::FormatBigBreak,
        '+' => ControlKind::FormatCancelBreak,
        ';' => ControlKind::FormatLikeSemicolon,
        _ => {
            return Err(Err::Error(ParseError::from_error_kind(
                span,
                ErrorKind::Char,
            )))
        }
    };

    Ok((span.slice(2..), Token::Control(k)))
}

pub fn expect_token<'a>(tok: Token) -> impl Fn(Span<'a>) -> ParseResult<Token> {
    move |span: Span| -> ParseResult<Token> {
        let (new_span, actual_tok) = next_token(span)?;

        if actual_tok == tok {
            Ok((new_span, actual_tok))
        } else {
            new_parse_error(span, ErrorKind::Char)
        }
    }
}

/// WEAVE:106
pub fn take_until_terminator<'a>(span: Span<'a>) -> ParseResult<StringSpan<'a>> {
    let (mut span, start) = position(span)?;
    let text_begin = span.clone();
    let mut n_taken = 0;
    let mut tok;

    loop {
        (span, tok) = next_token(span)?;

        match tok {
            Token::Control(ControlKind::Terminator) => {
                let (span, end) = position(span)?;
                let text = text_begin.take(n_taken);
                return Ok((
                    span,
                    StringSpan {
                        start,
                        end,
                        value: Cow::Borrowed(&text),
                    },
                ));
            }

            Token::Control(_) => {
                return new_parse_error(span, ErrorKind::TakeUntil);
            }

            Token::Char(_) => {
                n_taken += tok.n_chars();
            }
        }
    }
}
