//! Tokens in our semi-Pascal language

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, alphanumeric1, char, one_of},
    combinator::{map_res, recognize},
    error::ErrorKind,
    multi::{many0_count, many1},
    sequence::pair,
    InputTakeAtPosition,
};
use nom_locate::position;
use std::{borrow::Cow, collections::HashMap, convert::TryFrom, fmt};

use crate::{
    control::ControlKind,
    index::IndexEntryKind,
    parse_base::{new_parse_error, ParseError, ParseResult, Span, SpanValue, StringSpan},
    prettify::{Prettifier, RenderInline},
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

    /// An identifier that has been @f-defined to act like a reserved word.
    FormattedIdentifier(StringSpan<'a>, PascalReservedWord),

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

    /// Equality test: `=` in Pascal, `==` in C
    Equals,

    NotEquals,

    DoubleDot,

    /// Variable assignment: `:=` in Pascal, `=` in C
    Gets,

    /// Equivalence (macro and module definitions): `==` in WEB
    Equivalence,

    Colon,

    Caret,

    Period,

    /// Needed to parse WEB macros. We track a span so that we can pretend it's
    /// an identifier.
    Hash(Span<'a>),

    StringPoolChecksum,

    DefinitionFlag,

    CancelDefinitionFlag,

    IntLiteral(IntLiteralKind, usize),

    StringLiteral(StringLiteralKind, StringSpan<'a>),

    IndexEntry(IndexEntryKind, StringSpan<'a>),

    VerbatimPascal(StringSpan<'a>),
}

impl<'a> PascalToken<'a> {
    pub fn is_reserved_word(&self, rw: PascalReservedWord) -> bool {
        if let PascalToken::ReservedWord(SpanValue {
            value: found_rw, ..
        }) = self
        {
            *found_rw == rw
        } else {
            false
        }
    }
}

impl<'a> fmt::Display for PascalToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PascalToken::Identifier(s) => write!(f, "{}", s.value),
            PascalToken::IndexEntry(k, s) => write!(f, "IndexEntry({:?}, {:?})", k, s.value),
            PascalToken::ReservedWord(s) => write!(f, "{}", s.value),

            PascalToken::FormattedIdentifier(s, _) => write!(f, "{}", s.value),

            PascalToken::StringLiteral(k, s) => match k {
                StringLiteralKind::SingleQuote => write!(f, "{:?}", s.value),
                StringLiteralKind::DoubleQuote => {
                    if s.value.len() == 1 {
                        write!(f, "ord!({:?})", s.value)
                    } else {
                        write!(f, "pool!({:?})", s.value)
                    }
                }
            },

            PascalToken::IntLiteral(k, v) => match k {
                // See also handle_tex in pass2
                IntLiteralKind::Octal => write!(f, "0x{:X}", v),
                IntLiteralKind::Hex => write!(f, "0x{:X}", v),
                IntLiteralKind::Decimal => write!(f, "{}", v),
            },

            PascalToken::TexString(s) => write!(f, "TexString({:?})", s.value),
            PascalToken::OpenDelimiter(DelimiterKind::MetaComment) => write!(f, "/* "),
            PascalToken::CloseDelimiter(DelimiterKind::MetaComment) => write!(f, " */"),
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
            PascalToken::Gets => write!(f, "="), // sorry, Pascal
            PascalToken::Equivalence => write!(f, "==="),
            PascalToken::Colon => write!(f, ":"),
            PascalToken::Caret => write!(f, "^"),
            PascalToken::Period => write!(f, "."),
            PascalToken::Hash(_) => write!(f, "#"),
            PascalToken::StringPoolChecksum => write!(f, "$STRING_POOL_CHECKSUM"),
            PascalToken::DefinitionFlag => Ok(()),
            PascalToken::CancelDefinitionFlag => Ok(()),
            PascalToken::VerbatimPascal(text) => write!(f, "{}", text.value),
        }
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

        // "This control code is treated like a semicolon, for formatting
        // purposes, except that it is invisible. You can use it, for example,
        // after a module name when the Pascal text represented by that module
        // name ends with a semicolon." Empirically, it seems to work better to
        // ignore this token (which happens for Formatting ones) than to treat
        // it as a syntactic semicolon.
        Token::Control(ControlKind::FormatLikeSemicolon) => PascalToken::Formatting,

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
    let (span, text) = recognize(pair(alpha1, many0_count(alt((alphanumeric1, tag("_"))))))(span)?;
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
fn match_identifier_token<'a>(
    overrides: Option<&FormatOverrides>,
) -> impl Fn(Span<'a>) -> ParseResult<'a, PascalToken<'a>> + '_ {
    move |span: Span<'a>| {
        // We can ignore control codes since the alphanumeric scanner won't match them.
        let (span, start) = position(span)?;
        let (span, text) =
            recognize(pair(alpha1, many0_count(alt((alphanumeric1, tag("_"))))))(span)?;
        let (span, end) = position(span)?;

        let value: Cow<str> = Cow::Borrowed(&text);
        let rw = overrides.and_then(|hm| hm.get(value.as_ref()));
        let val_span = StringSpan { start, end, value };

        let tok = if let Some(rw) = rw {
            PascalToken::FormattedIdentifier(val_span, *rw)
        } else {
            PascalToken::Identifier(val_span)
        };

        Ok((span, tok))
    }
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

        '#' => {
            let pos;
            (span, pos) = position(span)?;
            PascalToken::Hash(pos)
        }

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

pub fn scan_octal_literal(span: Span) -> ParseResult<usize> {
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

pub fn scan_hex_literal(span: Span) -> ParseResult<usize> {
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

pub type FormatOverrides = HashMap<String, PascalReservedWord>;

pub fn match_pascal_token<'a>(
    span: Span<'a>,
    overrides: Option<&FormatOverrides>,
) -> ParseResult<'a, PascalToken<'a>> {
    let (span, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;
    alt((
        match_tex_string_token,
        match_verbatim_pascal_token,
        match_reserved_word_token,
        match_identifier_token(overrides),
        match_punct_token,
        match_pascal_control_code_token,
        match_decimal_literal_token,
        match_octal_literal_token,
        match_hex_literal_token,
        match_string_literal,
        match_index_entry,
    ))(span)
}

// Prettification

impl<'a> RenderInline for PascalToken<'a> {
    fn measure_inline(&self) -> usize {
        match self {
            PascalToken::TexString(_) => 0,
            PascalToken::ReservedWord(sv) => sv.value.to_string().len(),
            PascalToken::Identifier(ss) => ss.len(),
            PascalToken::FormattedIdentifier(ss, _) => ss.len(),

            PascalToken::OpenDelimiter(dk) => match dk {
                DelimiterKind::MetaComment => 2,
                _ => 1,
            },

            PascalToken::CloseDelimiter(dk) => match dk {
                DelimiterKind::MetaComment => 2,
                _ => 1,
            },

            PascalToken::Comma => 1,
            PascalToken::Semicolon => 1,
            PascalToken::Formatting => 0,
            PascalToken::PasteText => 0,
            PascalToken::ForcedEol => 0,
            PascalToken::DollarSign => 1,
            PascalToken::Plus => 1,
            PascalToken::Minus => 1,
            PascalToken::Times => 1,
            PascalToken::Divide => 1,
            PascalToken::Greater => 1,
            PascalToken::GreaterEquals => 2,
            PascalToken::Less => 1,
            PascalToken::LessEquals => 2,
            PascalToken::Equals => 2,
            PascalToken::NotEquals => 2,
            PascalToken::DoubleDot => 2,
            PascalToken::Gets => 2,
            PascalToken::Equivalence => 3,
            PascalToken::Colon => 1,
            PascalToken::Caret => 1,
            PascalToken::Period => 1,
            PascalToken::Hash(_) => 1,
            PascalToken::StringPoolChecksum => "stringpoolchecksum!()".len(),
            PascalToken::DefinitionFlag => 0,
            PascalToken::CancelDefinitionFlag => 0,

            PascalToken::IntLiteral(kind, n) => {
                if kind == &IntLiteralKind::Decimal {
                    n.to_string().len()
                } else {
                    format!("0x{:x}", n).len()
                }
            }

            PascalToken::StringLiteral(kind, ss) => match kind {
                StringLiteralKind::SingleQuote => format!("{:?}", ss.value).len(),

                StringLiteralKind::DoubleQuote => {
                    if ss.len() == 1 {
                        format!("ord!({:?})", ss.value).len()
                    } else {
                        format!("strpool!({:?})", ss.value).len()
                    }
                }
            },

            PascalToken::IndexEntry(..) => 0,
            PascalToken::VerbatimPascal(ss) => ss.value.len(),
        }
    }

    fn render_inline(&self, dest: &mut Prettifier) {
        match self {
            PascalToken::TexString(_) => {}

            PascalToken::ReservedWord(sv) => {
                dest.noscope_push(sv.value);
            }

            PascalToken::Identifier(ss) => {
                dest.noscope_push(ss.value.as_ref());
            }

            PascalToken::FormattedIdentifier(ss, _) => {
                dest.noscope_push(ss.value.as_ref());
            }

            PascalToken::OpenDelimiter(dk) => {
                dest.noscope_push(match dk {
                    DelimiterKind::MetaComment => "/*",
                    DelimiterKind::Paren => "(",
                    DelimiterKind::SquareBracket => "[",
                });
            }

            PascalToken::CloseDelimiter(dk) => {
                dest.noscope_push(match dk {
                    DelimiterKind::MetaComment => "*/",
                    DelimiterKind::Paren => ")",
                    DelimiterKind::SquareBracket => "]",
                });
            }

            PascalToken::Comma => {
                dest.noscope_push(',');
            }

            PascalToken::Semicolon => {
                dest.noscope_push(';');
            }

            PascalToken::Formatting => {}
            PascalToken::PasteText => {}
            PascalToken::ForcedEol => {}

            PascalToken::DollarSign => {
                dest.noscope_push('$');
            }

            PascalToken::Plus => {
                dest.noscope_push('+');
            }

            PascalToken::Minus => {
                dest.noscope_push('-');
            }

            PascalToken::Times => {
                dest.noscope_push('*');
            }

            PascalToken::Divide => {
                dest.noscope_push('/');
            }

            PascalToken::Greater => {
                dest.noscope_push('>');
            }

            PascalToken::GreaterEquals => {
                dest.noscope_push(">=");
            }

            PascalToken::Less => {
                dest.noscope_push('<');
            }

            PascalToken::LessEquals => {
                dest.noscope_push("<=");
            }

            PascalToken::Equals => {
                dest.noscope_push("==");
            }

            PascalToken::NotEquals => {
                dest.noscope_push("!=");
            }

            PascalToken::DoubleDot => {
                dest.noscope_push("..");
            }

            PascalToken::Gets => {
                dest.noscope_push("=");
            }

            PascalToken::Equivalence => {
                dest.noscope_push("===");
            }

            PascalToken::Colon => {
                dest.noscope_push(':');
            }

            PascalToken::Caret => {
                dest.noscope_push('^');
            }

            PascalToken::Period => {
                dest.noscope_push('.');
            }

            PascalToken::Hash(_) => {
                dest.noscope_push('#');
            }

            PascalToken::StringPoolChecksum => {
                dest.noscope_push("stringpoolchecksum!()");
            }

            PascalToken::DefinitionFlag => {}
            PascalToken::CancelDefinitionFlag => {}

            PascalToken::IntLiteral(kind, n) => {
                match kind {
                    IntLiteralKind::Decimal => dest.noscope_push(n),

                    // I think octal is dumb, so I present it as hex.
                    IntLiteralKind::Octal | IntLiteralKind::Hex => {
                        dest.noscope_push(format!("0x{:x}", n));
                    }
                }
            }

            PascalToken::StringLiteral(kind, ss) => match kind {
                StringLiteralKind::SingleQuote => {
                    dest.noscope_push(format!("{:?}", ss.value));
                }

                StringLiteralKind::DoubleQuote => {
                    if ss.len() == 1 {
                        dest.noscope_push(format!("ord!({:?})", ss.value));
                    } else {
                        dest.noscope_push(format!("strpool!({:?})", ss.value));
                    }
                }
            },

            PascalToken::IndexEntry(..) => {}

            PascalToken::VerbatimPascal(ss) => {
                dest.noscope_push(ss.value.as_ref());
            }
        }
    }
}
