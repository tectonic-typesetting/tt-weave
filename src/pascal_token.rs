//! Tokens in our semi-Pascal language

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, alphanumeric1, char, one_of},
    combinator::{map_res, recognize},
    error::ErrorKind,
    multi::{many0_count, many1},
    sequence::pair,
    InputTake, InputTakeAtPosition,
};
use nom_locate::position;
use std::{borrow::Cow, convert::TryFrom, fmt};

use crate::{
    control::ControlKind,
    index::IndexEntryKind,
    parse_base::{new_parse_error, ParseError, ParseResult, Span, SpanValue, StringSpan},
    reserved::PascalReservedWord,
    token::{expect_token, next_token, take_until_terminator, Token},
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DelimiterKind {
    Paren,

    /// A `@{` or `@}` meta-comment. Note that these need not be balanced.
    MetaComment,

    /// Obtained with the `(.` digraph, or literal square brackets.
    SquareBracket,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IntLiteralKind {
    Decimal,
    Octal,
    Hex,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StringLiteralKind {
    SingleQuote,
    DoubleQuote,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PascalToken<'a> {
    /// The `@t` control code: TeX text for the woven output.
    TexString(StringSpan<'a>),

    /// One of the Pascal reserved words
    ReservedWord(SpanValue<'a, PascalReservedWord>),

    /// An identifier
    Identifier(StringSpan<'a>),

    OpenDelimiter(DelimiterKind),

    CloseDelimiter(DelimiterKind),

    Comma,

    Semicolon,

    /// Formatting control codes that we don't care about: @/, @|, @#, @+
    Formatting,

    /// @&: concatenate adjacent pieces of text
    PasteText,

    /// @\: force EOL in the Pascal output
    ForcedEol,

    /// semi-hack (?) for parsing magic system directives
    DollarSign,

    Plus,

    Minus,

    Times,

    Divide,

    Greater,

    GreaterEquals,

    Less,

    LessEquals,

    Equals,

    NotEquals,

    DoubleDot,

    Gets,

    Equivalence,

    Colon,

    Caret,

    Period,

    /// Needed to parse WEB macros
    Hash,

    StringPoolChecksum,

    DefinitionFlag,

    CancelDefinitionFlag,

    IntLiteral(IntLiteralKind, usize),

    StringLiteral(StringLiteralKind, StringSpan<'a>),

    IndexEntry(IndexEntryKind, StringSpan<'a>),

    VerbatimPascal(StringSpan<'a>),
}

impl<'a> fmt::Display for PascalToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PascalToken::Identifier(s) => write!(f, "{}", s.value),
            PascalToken::IndexEntry(k, s) => write!(f, "IndexEntry({:?}, {:?})", k, s.value),
            PascalToken::ReservedWord(s) => write!(f, "{}", s.value),
            PascalToken::StringLiteral(k, s) => write!(f, "StringLiteral({:?}, {:?})", k, s.value),
            PascalToken::TexString(s) => write!(f, "TexString({:?})", s.value),
            PascalToken::OpenDelimiter(DelimiterKind::MetaComment) => write!(f, "/*"),
            PascalToken::CloseDelimiter(DelimiterKind::MetaComment) => write!(f, "*/"),
            PascalToken::OpenDelimiter(DelimiterKind::SquareBracket) => write!(f, "["),
            PascalToken::CloseDelimiter(DelimiterKind::SquareBracket) => write!(f, "]"),
            PascalToken::OpenDelimiter(DelimiterKind::Paren) => write!(f, "("),
            PascalToken::CloseDelimiter(DelimiterKind::Paren) => write!(f, ")"),
            PascalToken::Comma => write!(f, ":"),
            PascalToken::Semicolon => write!(f, ";"),
            PascalToken::Formatting => Ok(()),
            PascalToken::PasteText => Ok(()),
            PascalToken::ForcedEol => writeln!(f),
            PascalToken::DollarSign => write!(f, "$"),
            PascalToken::Plus => write!(f, "+"),
            PascalToken::Minus => write!(f, "-"),
            PascalToken::Times => write!(f, "*"),
            PascalToken::Divide => write!(f, "/"),
            PascalToken::Greater => write!(f, ">"),
            PascalToken::GreaterEquals => write!(f, ">="),
            PascalToken::Less => write!(f, "<"),
            PascalToken::LessEquals => write!(f, "<="),
            PascalToken::Equals => write!(f, "=="),
            PascalToken::NotEquals => write!(f, "!="),
            PascalToken::DoubleDot => write!(f, ".."),
            PascalToken::Gets => write!(f, ":="),
            PascalToken::Equivalence => write!(f, "==="),
            PascalToken::Colon => write!(f, ":"),
            PascalToken::Caret => write!(f, "^"),
            PascalToken::Period => write!(f, "."),
            PascalToken::Hash => write!(f, "#"),
            PascalToken::StringPoolChecksum => write!(f, "$STRING_POOL_CHECKSUM"),
            PascalToken::DefinitionFlag => Ok(()),
            PascalToken::CancelDefinitionFlag => Ok(()),
            PascalToken::IntLiteral(_, v) => write!(f, "{}", v),
            PascalToken::VerbatimPascal(text) => write!(f, "{}", text.value),
        }
    }
}

fn take_until_nlspace<'a>(mut span: Span<'a>) -> ParseResult<Span<'a>> {
    let text_begin = span.clone();
    let mut n_taken = 0;

    loop {
        let (new_span, tok) = next_token(span)?;

        if let Token::Char(c) = tok {
            if c == ' ' || c == '\t' || c == '\n' {
                return Ok((span, text_begin.take(n_taken)));
            }
        }

        span = new_span;
        n_taken += tok.n_chars();
    }
}

fn match_tex_string_token(span: Span) -> ParseResult<PascalToken> {
    let (span, _) = expect_token(Token::Control(ControlKind::TexAnnotation))(span)?;
    let (span, text) = take_until_terminator(span)?;
    Ok((span, PascalToken::TexString(text)))
}

fn match_verbatim_pascal_token(span: Span) -> ParseResult<PascalToken> {
    let (span, _) = expect_token(Token::Control(ControlKind::VerbatimPascal))(span)?;
    let (span, text) = take_until_terminator(span)?;
    Ok((span, PascalToken::VerbatimPascal(text)))
}

fn match_pascal_control_code_token(span: Span) -> ParseResult<PascalToken> {
    let (span, tok) = next_token(span)?;

    let ptok = match tok {
        Token::Control(ControlKind::BeginMetaComment) => {
            PascalToken::OpenDelimiter(DelimiterKind::MetaComment)
        }
        Token::Control(ControlKind::EndMetaComment) => {
            PascalToken::CloseDelimiter(DelimiterKind::MetaComment)
        }
        Token::Control(ControlKind::FormatThinSpace) => PascalToken::Formatting,
        Token::Control(ControlKind::FormatBigBreak) => PascalToken::Formatting,
        Token::Control(ControlKind::FormatBreak) => PascalToken::Formatting,
        Token::Control(ControlKind::FormatCancelBreak) => PascalToken::Formatting,
        Token::Control(ControlKind::FormatOptionalBreak) => PascalToken::Formatting,
        Token::Control(ControlKind::PasteText) => PascalToken::PasteText,
        Token::Control(ControlKind::PascalForceEol) => PascalToken::ForcedEol,

        // Is this right? "This control code is treated like a semicolon, for
        // formatting purposes, except that it is invisible. You can use it, for
        // example, after a module name when the Pascal text represented by that
        // module name ends with a semicolon."
        Token::Control(ControlKind::FormatLikeSemicolon) => PascalToken::Semicolon,

        Token::Control(ControlKind::StringPoolChecksum) => PascalToken::StringPoolChecksum,

        // Not sure if we should try to handle this at a lower level or not
        // Cf xetex: `procedure@?show_info; forward;@t\2@>@?{|show_node_list(info(temp_ptr))|}`
        //                                                ^^
        Token::Control(ControlKind::DefinitionFlag) => PascalToken::DefinitionFlag,
        Token::Control(ControlKind::CancelDefinitionFlag) => PascalToken::CancelDefinitionFlag,

        _ => return new_parse_error(span, ErrorKind::Char),
    };

    Ok((span, ptok))
}

fn match_reserved_word_token(span: Span) -> ParseResult<PascalToken> {
    let (span, start) = position(span)?;
    let (span, text) = take_until_nlspace(span)?;
    let (span, end) = position(span)?;

    match PascalReservedWord::try_from(&text[..]) {
        Ok(value) => Ok((
            span,
            PascalToken::ReservedWord(SpanValue { start, end, value }),
        )),
        Err(_) => new_parse_error(span, ErrorKind::Tag),
    }
}

/// See WEAVE:98
fn match_identifier_token(span: Span) -> ParseResult<PascalToken> {
    // We can ignore control codes since the alphanumeric scanner won't match them.
    let (span, start) = position(span)?;
    let (span, text) = recognize(pair(alpha1, many0_count(alt((alphanumeric1, tag("_"))))))(span)?;
    let (span, end) = position(span)?;

    Ok((
        span,
        PascalToken::Identifier(StringSpan {
            start,
            end,
            value: Cow::Borrowed(&text),
        }),
    ))
}

/// See WEAVE:97
fn match_punct_token(span: Span) -> ParseResult<PascalToken> {
    let (mut span, tok) = next_token(span)?;

    let c = match tok {
        Token::Char(c) => c,
        _ => return new_parse_error(span, ErrorKind::Char),
    };

    let tok = match c {
        '(' => {
            if let Ok((new_span, _)) = char::<Span, ParseError>('*')(span) {
                span = new_span;
                PascalToken::OpenDelimiter(DelimiterKind::MetaComment)
            } else if let Ok((new_span, _)) = char::<Span, ParseError>('.')(span) {
                span = new_span;
                PascalToken::OpenDelimiter(DelimiterKind::SquareBracket)
            } else {
                PascalToken::OpenDelimiter(DelimiterKind::Paren)
            }
        }

        ')' => PascalToken::CloseDelimiter(DelimiterKind::Paren),
        '[' => PascalToken::OpenDelimiter(DelimiterKind::SquareBracket),
        ']' => PascalToken::CloseDelimiter(DelimiterKind::SquareBracket),
        ',' => PascalToken::Comma,
        ';' => PascalToken::Semicolon,
        '$' => PascalToken::DollarSign,
        '+' => PascalToken::Plus,
        '-' => PascalToken::Minus,
        '^' => PascalToken::Caret,
        '/' => PascalToken::Divide,
        '#' => PascalToken::Hash,

        '.' => {
            if let Ok((new_span, _)) = char::<Span, ParseError>('.')(span) {
                span = new_span;
                PascalToken::DoubleDot
            } else if let Ok((new_span, _)) = char::<Span, ParseError>(')')(span) {
                span = new_span;
                PascalToken::CloseDelimiter(DelimiterKind::SquareBracket)
            } else {
                PascalToken::Period
            }
        }

        ':' => {
            if let Ok((new_span, _)) = char::<Span, ParseError>('=')(span) {
                span = new_span;
                PascalToken::Gets
            } else {
                PascalToken::Colon
            }
        }

        '=' => {
            if let Ok((new_span, _)) = char::<Span, ParseError>('=')(span) {
                span = new_span;
                PascalToken::Equivalence
            } else {
                PascalToken::Equals
            }
        }

        '>' => {
            if let Ok((new_span, _)) = char::<Span, ParseError>('=')(span) {
                span = new_span;
                PascalToken::GreaterEquals
            } else {
                PascalToken::Greater
            }
        }

        '<' => {
            if let Ok((new_span, _)) = char::<Span, ParseError>('=')(span) {
                span = new_span;
                PascalToken::LessEquals
            } else if let Ok((new_span, _)) = char::<Span, ParseError>('>')(span) {
                span = new_span;
                PascalToken::NotEquals
            } else {
                PascalToken::Less
            }
        }

        '*' => {
            if let Ok((new_span, _)) = char::<Span, ParseError>(')')(span) {
                span = new_span;
                PascalToken::CloseDelimiter(DelimiterKind::MetaComment)
            } else {
                PascalToken::Times
            }
        }

        _ => return new_parse_error(span, ErrorKind::Char),
    };

    Ok((span, tok))
}

fn scan_decimal_literal(span: Span) -> ParseResult<usize> {
    // FIXME this is blah; derived from nom example
    map_res(recognize(many1(one_of("0123456789"))), |out: Span| {
        usize::from_str_radix(&out, 10)
    })(span)
}

fn match_decimal_literal_token(span: Span) -> ParseResult<PascalToken> {
    let (span, v) = scan_decimal_literal(span)?;
    Ok((span, PascalToken::IntLiteral(IntLiteralKind::Decimal, v)))
}

fn scan_octal_literal(span: Span) -> ParseResult<usize> {
    // FIXME this is blah; derived from nom example
    map_res(recognize(many1(one_of("01234567"))), |out: Span| {
        usize::from_str_radix(&out, 8)
    })(span)
}

fn match_octal_literal_token(span: Span) -> ParseResult<PascalToken> {
    let (span, _) = expect_token(Token::Control(ControlKind::OctalLiteral))(span)?;
    let (span, v) = scan_octal_literal(span)?;
    Ok((span, PascalToken::IntLiteral(IntLiteralKind::Octal, v)))
}

fn scan_hex_literal(span: Span) -> ParseResult<usize> {
    // FIXME this is blah; derived from nom example
    map_res(
        recognize(many1(one_of("0123456789ABCDEFabcdef"))),
        |out: Span| usize::from_str_radix(&out, 16),
    )(span)
}

fn match_hex_literal_token(span: Span) -> ParseResult<PascalToken> {
    let (span, _) = expect_token(Token::Control(ControlKind::HexLiteral))(span)?;
    let (span, v) = scan_hex_literal(span)?;
    Ok((span, PascalToken::IntLiteral(IntLiteralKind::Hex, v)))
}

/// See WEAVE:99
///
/// In WEB, string literals escape their delimiters by repeating them: `""""` is
/// `"\""`. WEAVE parsing ignores the semantics here and just treats such
/// sequences as two adjacent string literals.
fn match_string_literal(span: Span) -> ParseResult<PascalToken> {
    let (span, start) = position(span)?;
    let (span, tok) = next_token(span)?;

    let (delim, kind) = match tok {
        Token::Char('\"') => ('\"', StringLiteralKind::DoubleQuote),
        Token::Char('\'') => ('\'', StringLiteralKind::SingleQuote),
        _ => return new_parse_error(span, ErrorKind::Char),
    };

    let (span, contents) = span.split_at_position(|c| c == '\n' || c == delim)?;
    let (span, terminator) = next_token(span)?;

    if let Token::Char('\n') = terminator {
        return new_parse_error(span, ErrorKind::Char);
    }

    let (span, end) = position(span)?;

    Ok((
        span,
        PascalToken::StringLiteral(
            kind,
            StringSpan {
                start,
                end,
                value: Cow::Borrowed(&contents),
            },
        ),
    ))
}

fn match_index_entry(span: Span) -> ParseResult<PascalToken> {
    let (span, tok) = next_token(span)?;

    let kind = match tok.as_index_kind() {
        Some(k) => k,
        None => return new_parse_error(span, ErrorKind::Char),
    };

    let (span, text) = take_until_terminator(span)?;
    Ok((span, PascalToken::IndexEntry(kind, text)))
}

pub fn match_pascal_token(span: Span) -> ParseResult<PascalToken> {
    let (span, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;
    alt((
        match_tex_string_token,
        match_verbatim_pascal_token,
        match_reserved_word_token,
        match_identifier_token,
        match_punct_token,
        match_pascal_control_code_token,
        match_decimal_literal_token,
        match_octal_literal_token,
        match_hex_literal_token,
        match_string_literal,
        match_index_entry,
    ))(span)
}
