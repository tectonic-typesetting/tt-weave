//! Parsing for the WEB input.

use lexical_sort::{natural_lexical_cmp, StringSort};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, alphanumeric1, char, one_of},
    combinator::{map_res, recognize},
    error::ErrorKind,
    multi::{many0_count, many1},
    sequence::pair,
    Finish, InputTake, InputTakeAtPosition, Slice,
};
use nom_locate::position;
use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap},
    convert::TryFrom,
    fmt::{self, Write},
};
use tectonic_errors::prelude::*;

use crate::{
    control::ControlKind,
    parse_base::{new_parse_error, ParseError, ParseResult, Span, SpanValue, StringSpan},
    token::{expect_token, next_token, Token},
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

/// Scan the name of a WEB module.
///
/// Module names are terminated by a terminator control token. Leading and
/// trailing whitespace are eaten, and inner whitespace is collapsed.
///
/// See WEAVE:103-104.
fn scan_module_name<'a>(state: &State, span: Span<'a>) -> ParseResult<'a, StringSpan<'a>> {
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

        for name in state
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
enum DelimiterKind {
    Paren,

    /// A `@{` or `@}` meta-comment. Note that these need not be balanced.
    MetaComment,

    /// Obtained with the `(.` digraph, or literal square brackets.
    SquareBracket,
}

#[derive(Debug)]
enum IntLiteralKind {
    Decimal,
    Octal,
    Hex,
}

#[derive(Debug)]
enum StringLiteralKind {
    SingleQuote,
    DoubleQuote,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum IndexEntryKind {
    /// Auto-sourced from Pascal code; printed in italics
    Normal,

    /// `@^`: for human language
    Roman,

    /// `@.`: for UI strings
    Typewriter,

    /// `@:`: used for custom TeX typesetting, essentially
    Wildcard,
}

#[derive(Debug)]
enum PascalToken<'a> {
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

    Comment(StringSpan<'a>),

    VerbatimPascal(StringSpan<'a>),
}

impl<'a> fmt::Display for PascalToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PascalToken::Comment(s) => write!(f, "Comment({:?})", s.value),
            PascalToken::Identifier(s) => write!(f, "Identifier({:?})", s.value),
            PascalToken::IndexEntry(k, s) => write!(f, "IndexEntry({:?}, {:?})", k, s.value),
            PascalToken::ReservedWord(s) => write!(f, "ReservedWord({:?})", s.value),
            PascalToken::StringLiteral(k, s) => write!(f, "StringLiteral({:?}, {:?})", k, s.value),
            PascalToken::TexString(s) => write!(f, "TexString({:?})", s.value),
            _ => write!(f, "{:?}", self),
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

/// WEAVE:106
fn take_until_terminator<'a>(span: Span<'a>) -> ParseResult<StringSpan<'a>> {
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

fn match_comment(span: Span) -> ParseResult<PascalToken> {
    let (span, start) = position(span)?;
    let (mut span, _) = char('{')(span)?;
    let s_begin = span.clone();

    let mut brace_depth = 1;
    let mut inner_pascal = false;
    let mut tok;

    loop {
        (span, tok) = next_token(span)?;

        // TODO: if we're in pascal mode we should
        // actually scan as Pascal tokens.

        match tok {
            Token::Char('{') => {
                if !inner_pascal {
                    brace_depth += 1;
                }
            }

            Token::Char('|') => {
                inner_pascal = !inner_pascal;
            }

            Token::Char('}') => {
                if !inner_pascal {
                    brace_depth -= 1;

                    if brace_depth == 0 {
                        break;
                    }
                }
            }

            _ => {}
        }
    }

    let (span, end) = position(span)?;
    let len = end.location_offset() - (1 + s_begin.location_offset());

    Ok((
        span,
        PascalToken::Comment(StringSpan {
            start,
            end,
            value: Cow::Borrowed(&s_begin.slice(..len)),
        }),
    ))
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

    let kind = match tok {
        Token::Control(ControlKind::RomanIndexEntry) => IndexEntryKind::Roman,
        Token::Control(ControlKind::TypewriterIndexEntry) => IndexEntryKind::Typewriter,
        Token::Control(ControlKind::WildcardIndexEntry) => IndexEntryKind::Wildcard,
        _ => return new_parse_error(span, ErrorKind::Char),
    };

    let (span, text) = take_until_terminator(span)?;
    Ok((span, PascalToken::IndexEntry(kind, text)))
}

fn match_pascal_token(span: Span) -> ParseResult<PascalToken> {
    let (span, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;
    alt((
        match_tex_string_token,
        match_verbatim_pascal_token,
        match_reserved_word_token,
        match_identifier_token,
        match_punct_token,
        match_comment,
        match_pascal_control_code_token,
        match_decimal_literal_token,
        match_octal_literal_token,
        match_hex_literal_token,
        match_string_literal,
        match_index_entry,
    ))(span)
}

type ModuleId = usize;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Reference {
    pub module: ModuleId,
    pub is_definition: bool,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct IndexState {
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
struct State {
    definition_flag: bool,

    /// Map from full-length module name to the module that initially defines
    /// it. Every entry here also has a record in the index table, where
    /// `is_definition` modules indicate ones that contribute code to the
    /// module.
    ///
    /// https://stackoverflow.com/questions/27344452/how-can-i-have-a-sorted-key-value-map-with-prefix-key-search
    named_modules: BTreeSet<String>,

    index_entries: HashMap<String, IndexState>,
}

impl State {
    fn add_index_entry<S: Into<String>>(
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

    fn set_definition_flag(&mut self, f: bool) {
        self.definition_flag = f;
    }

    fn scan_add_next<'a>(
        &mut self,
        kind: IndexEntryKind,
        module: ModuleId,
        span: Span<'a>,
    ) -> ParseResult<'a, Token> {
        let (span, text) = take_until_terminator(span)?;
        self.add_index_entry(text.value.into_owned(), kind, module);
        next_token(span)
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

        (span, ptok) = match_pascal_token(prev_span)?;

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
                (span, ptok) = match_pascal_token(span)?;

                if let PascalToken::Identifier(text) = ptok {
                    state.add_index_entry(
                        text.value.into_owned(),
                        IndexEntryKind::Normal,
                        cur_module,
                    );
                    (span, ptok) = match_pascal_token(span)?;

                    if let PascalToken::Equivalence = ptok {
                        (span, ptok) = match_pascal_token(span)?;

                        if let PascalToken::Identifier(text) = ptok {
                            // TODO? Register the new formatting convention
                            state.add_index_entry(
                                text.value.into_owned(),
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
                let text;
                (span, text) = scan_module_name(state, span)?;
                state.named_modules.insert(text.value.to_string());
                state.add_index_entry(text.value.into_owned(), IndexEntryKind::Normal, cur_module);
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
                println!("- Major module #{}", cur_module);
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
                let text;
                (span, text) = scan_module_name(state, span)?;

                state.set_definition_flag(true);
                state.named_modules.insert(text.value.to_string());
                state.add_index_entry(text.value.into_owned(), IndexEntryKind::Normal, cur_module);

                // there's like one module in XeTeX with a space between module name and equals sign
                (span, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;
                (span, _) = char('=')(span)?;
                (span, tok) = first_pass_handle_pascal(cur_module, state, span)?;
            }

            _ => {}
        }
    }
}

pub fn first_pass(span: Span) -> Result<()> {
    let mut state = State::default();

    match first_pass_inner(&mut state, span).finish() {
        Ok((_remainder, _value)) => {}
        Err((_remainder, ErrorKind::Eof)) => {}
        Err((_remainder, kind)) => return Err(anyhow!(kind.description().to_owned())),
    }

    for name in state.named_modules.iter() {
        println!("{:?}", name);
    }

    println!();

    let mut index: Vec<_> = state.index_entries.keys().collect();
    index.string_sort_unstable(natural_lexical_cmp);

    for name in &index {
        if state.named_modules.contains(&**name) {
            continue;
        }

        let info = state.index_entries.get(&**name).unwrap();

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

        println!("{} ({:?}) => {}", name, info.kind, refs);
    }

    Ok(())
}
