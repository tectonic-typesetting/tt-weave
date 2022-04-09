//! Low-level tokens out of WEB files.

use nom::{
    error::{ErrorKind, ParseError as NomParseError},
    Err, InputIter, Slice,
};

use crate::{
    control::ControlKind,
    parse_base::{new_parse_error, ParseError, ParseResult, Span},
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
