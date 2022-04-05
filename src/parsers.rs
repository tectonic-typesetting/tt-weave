//! Parsers for the WEB inputs.

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::complete::{alpha1, alphanumeric1, char},
    combinator::recognize,
    error::{ErrorKind, ParseError as NomParseError},
    multi::{many0, many0_count},
    sequence::pair,
    Err, Finish, IResult, InputIter, Slice,
};
use nom_locate::position;
use std::{borrow::Cow, convert::TryFrom, fmt};
use tectonic_errors::prelude::*;

use crate::Span;

type ParseError<'a> = (Span<'a>, ErrorKind);
type ParseResult<'a, T> = IResult<Span<'a>, T, ParseError<'a>>;

fn new_parse_error<'a, T>(s: Span<'a>, k: ErrorKind) -> ParseResult<'a, T> {
    Err(Err::Error(ParseError::from_error_kind(s, k)))
}

/// Um, surely there must be an already-built-in way to do this??? But I
/// can't find one.
#[inline(always)]
fn next_char(span: Span) -> ParseResult<char> {
    match span.iter_elements().next() {
        Some(c) => Ok((span.slice(1..), c)),
        None => new_parse_error(span, ErrorKind::Eof),
    }
}

/// Skip the "limbo" section at the start of the WEB file.
///
/// This method approximately corresponds to WEAVE:89, `skip_limbo`.
fn skip_limbo(mut span: Span) -> ParseResult<()> {
    // I feel like there must be a better way to do this. The way that peeking
    // seems to work in nom, I think we have to loop char-by-char in order to be
    // able to rewind to the '@' when we find our match.
    loop {
        let prev_span = span.clone();
        let (new_span, c) = next_char(span)?;
        span = new_span;

        if c != '@' {
            continue;
        }

        let (new_span, c) = next_char(span)?;
        span = new_span;

        if c == ' ' || c == '\t' || c == '*' {
            return Ok((prev_span, ()));
        }
    }
}

#[derive(Debug)]
struct SpanValue<'a, T> {
    pub start: Span<'a>,
    pub end: Span<'a>,
    pub value: T,
}

type StringSpan<'a> = SpanValue<'a, Cow<'a, str>>;

/// Scan the name of a WEB module.
///
/// Module names are terminated by periods. Leading and trailing whitespace are
/// eaten, and inner whitespace is collapsed.
fn scan_module_name(span: Span) -> ParseResult<StringSpan> {
    let (span, start) = position(span)?;
    let (span, _ignored) = take_while(|c| c == ' ' || c == '\t')(span)?;

    let mut value = String::new();
    let mut space_needed = false;
    let mut n_consumed = 0;
    let mut finished = false;

    for c in span.iter_elements() {
        n_consumed += 1;

        if c == '.' {
            finished = true;
            break;
        } else if c == ' ' || c == '\t' {
            space_needed = true;
        } else {
            if space_needed {
                value.push(' ');
                space_needed = false;
            }

            value.push(c);
        }
    }

    if !finished {
        return new_parse_error(span, ErrorKind::Eof);
    }

    let span = span.slice(n_consumed..);
    let (span, end) = position(span)?;
    Ok((
        span,
        StringSpan {
            start,
            end,
            value: value.into(),
        },
    ))
}

#[derive(Debug)]
enum ModuleKind {
    Major,
    Minor,
}

#[derive(Debug)]
struct ModuleDeclaration<'a> {
    kind: ModuleKind,
    name: StringSpan<'a>,
}

fn scan_module_declaration(span: Span) -> ParseResult<ModuleDeclaration> {
    let (span, _) = char('@')(span)?;

    let (span, c) = next_char(span)?;
    let kind = if c == ' ' || c == '\t' {
        ModuleKind::Minor
    } else if c == '*' {
        ModuleKind::Major
    } else {
        return new_parse_error(span, ErrorKind::Char);
    };

    let (span, name) = scan_module_name(span)?;
    Ok((span, ModuleDeclaration { kind, name }))
}

/// Placeholder (??)
/// Note that we eat the @p here
fn skip_to_pascal(mut span: Span) -> ParseResult<()> {
    loop {
        let (new_span, c) = next_char(span)?;
        span = new_span;
        if c != '@' {
            continue;
        }

        let (new_span, c) = next_char(span)?;
        span = new_span;
        if c == 'p' || c == 'P' {
            return Ok((span, ()));
        }
    }
}

/// Reserved words in WEB's Pascal.
///
/// See WEAVE:64.
#[derive(Debug)]
enum PascalReservedWord {
    And,
    Array,
    Begin,
    Case,
    Const,
    Div,
    Do,
    Downto,
    Else,
    End,
    File,
    For,
    Function,
    Goto,
    If,
    In,
    Label,
    Mod,
    Nil,
    Not,
    Of,
    Or,
    Packed,
    Procedure,
    Program,
    Record,
    Repeat,
    Set,
    Then,
    To,
    Type,
    Until,
    Var,
    While,
    With,
    Xclause,
}

impl fmt::Display for PascalReservedWord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text = match self {
            PascalReservedWord::And => "and",
            PascalReservedWord::Array => "array",
            PascalReservedWord::Begin => "begin",
            PascalReservedWord::Case => "case",
            PascalReservedWord::Const => "const",
            PascalReservedWord::Div => "div",
            PascalReservedWord::Do => "do",
            PascalReservedWord::Downto => "downto",
            PascalReservedWord::Else => "else",
            PascalReservedWord::End => "end",
            PascalReservedWord::File => "file",
            PascalReservedWord::For => "for",
            PascalReservedWord::Function => "function",
            PascalReservedWord::Goto => "goto",
            PascalReservedWord::If => "if",
            PascalReservedWord::In => "in",
            PascalReservedWord::Label => "label",
            PascalReservedWord::Mod => "mod",
            PascalReservedWord::Nil => "nil",
            PascalReservedWord::Not => "not",
            PascalReservedWord::Of => "of",
            PascalReservedWord::Or => "or",
            PascalReservedWord::Packed => "packed",
            PascalReservedWord::Procedure => "procedure",
            PascalReservedWord::Program => "program",
            PascalReservedWord::Record => "record",
            PascalReservedWord::Repeat => "repeat",
            PascalReservedWord::Set => "set",
            PascalReservedWord::Then => "then",
            PascalReservedWord::To => "to",
            PascalReservedWord::Type => "type",
            PascalReservedWord::Until => "until",
            PascalReservedWord::Var => "var",
            PascalReservedWord::While => "while",
            PascalReservedWord::With => "with",
            PascalReservedWord::Xclause => "xclause",
        };
        write!(f, "{}", text)
    }
}

impl TryFrom<&str> for PascalReservedWord {
    type Error = ();

    fn try_from(value: &str) -> std::result::Result<Self, ()> {
        match value.as_ref() {
            "and" => Ok(PascalReservedWord::And),
            "array" => Ok(PascalReservedWord::Array),
            "begin" => Ok(PascalReservedWord::Begin),
            "case" => Ok(PascalReservedWord::Case),
            "const" => Ok(PascalReservedWord::Const),
            "div" => Ok(PascalReservedWord::Div),
            "do" => Ok(PascalReservedWord::Do),
            "downto" => Ok(PascalReservedWord::Downto),
            "else" => Ok(PascalReservedWord::Else),
            "end" => Ok(PascalReservedWord::End),
            "file" => Ok(PascalReservedWord::File),
            "for" => Ok(PascalReservedWord::For),
            "function" => Ok(PascalReservedWord::Function),
            "goto" => Ok(PascalReservedWord::Goto),
            "if" => Ok(PascalReservedWord::If),
            "in" => Ok(PascalReservedWord::In),
            "label" => Ok(PascalReservedWord::Label),
            "mod" => Ok(PascalReservedWord::Mod),
            "nil" => Ok(PascalReservedWord::Nil),
            "not" => Ok(PascalReservedWord::Not),
            "of" => Ok(PascalReservedWord::Of),
            "or" => Ok(PascalReservedWord::Or),
            "packed" => Ok(PascalReservedWord::Packed),
            "procedure" => Ok(PascalReservedWord::Procedure),
            "program" => Ok(PascalReservedWord::Program),
            "record" => Ok(PascalReservedWord::Record),
            "repeat" => Ok(PascalReservedWord::Repeat),
            "set" => Ok(PascalReservedWord::Set),
            "then" => Ok(PascalReservedWord::Then),
            "to" => Ok(PascalReservedWord::To),
            "type" => Ok(PascalReservedWord::Type),
            "until" => Ok(PascalReservedWord::Until),
            "var" => Ok(PascalReservedWord::Var),
            "while" => Ok(PascalReservedWord::While),
            "with" => Ok(PascalReservedWord::With),
            "xclause" => Ok(PascalReservedWord::Xclause),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
enum PascalToken<'a> {
    /// The `@t` control code: TeX text for the woven output.
    TexString(StringSpan<'a>),

    /// The `@<` control code: a reference to a module.
    ModuleRef(StringSpan<'a>),

    /// One of the Pascal reserved words
    ReservedWord(SpanValue<'a, PascalReservedWord>),

    /// An identifier
    Identifier(StringSpan<'a>),

    /// Formatting control codes that we don't care about: @/, @|, @#, @+
    Formatting,
}

fn take_until_nlspace<'a>() -> impl Fn(Span<'a>) -> ParseResult<Span<'a>> {
    take_while(|c| c != ' ' && c != '\t' && c != '\n')
}

fn take_until_end_token<'a>() -> impl Fn(Span<'a>) -> ParseResult<Span<'a>> {
    move |span: Span| -> ParseResult<Span<'a>> {
        let (span, result) = take_until("@>")(span)?;
        Ok((span.slice(2..), result))
    }
}

fn scan_tex_string_token(span: Span) -> ParseResult<PascalToken> {
    let (span, start) = position(span)?;
    let (span, _) = char('@')(span)?;
    let (span, _) = char('t')(span)?;
    let (span, text) = take_until_end_token()(span)?;
    let (span, end) = position(span)?;
    Ok((
        span,
        PascalToken::TexString(StringSpan {
            start,
            end,
            value: Cow::Borrowed(&text),
        }),
    ))
}

/// TODO: clarify about resolving partial names to full names
fn scan_module_reference_token(span: Span) -> ParseResult<PascalToken> {
    let (span, start) = position(span)?;
    let (span, _) = char('@')(span)?;
    let (span, _) = char('<')(span)?;
    let (span, text) = take_until_end_token()(span)?;
    let (span, end) = position(span)?;
    Ok((
        span,
        PascalToken::ModuleRef(StringSpan {
            start,
            end,
            value: Cow::Borrowed(&text),
        }),
    ))
}

fn scan_formatting_token(span: Span) -> ParseResult<PascalToken> {
    let (span, _) = char('@')(span)?;
    let (span, _) = alt((char('/'), char('|'), char('#'), char('+')))(span)?;
    Ok((span, PascalToken::Formatting))
}

fn scan_reserved_word_token(span: Span) -> ParseResult<PascalToken> {
    let (span, start) = position(span)?;
    let (span, text) = take_until_nlspace()(span)?;
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
fn scan_identifier_token(span: Span) -> ParseResult<PascalToken> {
    let (span, start) = position(span)?;
    let (span, text) = recognize(pair(alpha1, many0_count(alt((alphanumeric1, tag("_"))))))(span)?;
    //let (span, text) = alt((alphanumeric1, tag("_")))(span)?;
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

fn scan_pascal_token(span: Span) -> ParseResult<PascalToken> {
    let (span, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;
    alt((
        scan_tex_string_token,
        scan_module_reference_token,
        scan_reserved_word_token,
        scan_identifier_token,
        scan_formatting_token,
    ))(span)
}

fn first_pass_inner(span: Span) -> ParseResult<()> {
    let (span, _) = skip_limbo(span)?;
    let (span, moddec) = scan_module_declaration(span)?;
    println!("Module: {:?}", moddec);
    let (span, _) = skip_to_pascal(span)?;
    let (span, toks) = many0(scan_pascal_token)(span)?;

    println!("Got some pascal toks ({}):", toks.len());
    for tok in &toks {
        println!("- {:?}", tok);
    }

    println!("Stopped at: {}", &span[..32]);
    Ok((span, ()))
}

pub fn first_pass(span: Span) -> Result<()> {
    match first_pass_inner(span).finish() {
        Ok((_remainder, _value)) => Ok(()),
        Err((_remainder, kind)) => Err(anyhow!(kind.description().to_owned())),
    }
}
