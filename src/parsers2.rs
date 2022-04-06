//! Parsing for the WEB input.

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::complete::{alpha1, alphanumeric1, char, one_of},
    combinator::{map_res, recognize},
    error::{ErrorKind, ParseError as NomParseError},
    multi::{many0, many0_count, many1},
    sequence::{pair, terminated},
    Err, Finish, IResult, InputIter, InputTake, InputTakeAtPosition, Slice,
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ControlKind {
    /// `@>`
    Terminator,

    /// `@@`
    AtLiteral,

    /// `@ ` or `@\t`
    NewMinorModule,

    /// `@*`
    NewMajorModule,

    /// `@d` or `@D`
    MacroDefinition,

    /// `@f` or `@F`
    FormatDefinition,

    /// `@p` or `@P`
    StartUnnamedPascal,

    /// `@<`
    ModuleName,

    /// `@'`
    OctalLiteral,

    /// `@"`
    HexLiteral,

    /// `@$`
    StringPoolChecksum,

    /// `@{`
    BeginMetaComment,

    /// `@}`
    EndMetaComment,

    /// `@&`
    PasteText,

    /// `@^`
    RomanIndexEntry,

    /// `@.`
    TypewriterIndexEntry,

    /// `@:`
    WildcardIndexEntry,

    /// `@t`
    TexAnnotation,

    /// `@=`
    VerbatimPascal,

    /// `@\`
    PascalForceEol,

    /// `@!`
    DefinitionFlag,

    /// `@?`
    CancelDefinitionFlag,

    /// `@,`
    FormatThinSpace,

    /// `@/`
    FormatBreak,

    /// `@|`
    FormatOptionalBreak,

    /// `@#`
    FormatBigBreak,

    /// `@+`
    FormatCancelBreak,

    /// `@;`
    FormatLikeSemicolon,
}

impl ControlKind {
    fn syntax_char(&self) -> char {
        match self {
            ControlKind::Terminator => '>',
            ControlKind::AtLiteral => '@',
            ControlKind::NewMinorModule => ' ',
            ControlKind::NewMajorModule => '*',
            ControlKind::MacroDefinition => 'd',
            ControlKind::FormatDefinition => 'f',
            ControlKind::StartUnnamedPascal => 'p',
            ControlKind::ModuleName => '<',
            ControlKind::OctalLiteral => '\'',
            ControlKind::HexLiteral => '"',
            ControlKind::StringPoolChecksum => '$',
            ControlKind::BeginMetaComment => '{',
            ControlKind::EndMetaComment => '}',
            ControlKind::PasteText => '&',
            ControlKind::RomanIndexEntry => '^',
            ControlKind::TypewriterIndexEntry => '.',
            ControlKind::WildcardIndexEntry => ':',
            ControlKind::TexAnnotation => 't',
            ControlKind::VerbatimPascal => '=',
            ControlKind::PascalForceEol => '\\',
            ControlKind::DefinitionFlag => '!',
            ControlKind::CancelDefinitionFlag => '?',
            ControlKind::FormatThinSpace => ',',
            ControlKind::FormatBreak => '/',
            ControlKind::FormatOptionalBreak => '|',
            ControlKind::FormatBigBreak => '#',
            ControlKind::FormatCancelBreak => '+',
            ControlKind::FormatLikeSemicolon => ';',
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Token {
    Char(char),
    Control(ControlKind),
}

impl Token {
    fn n_chars(&self) -> usize {
        match self {
            Token::Char(_) => 1,
            Token::Control(_) => 2,
        }
    }

    fn push_syntax_into(&self, s: &mut String) {
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

fn next_token(span: Span) -> ParseResult<Token> {
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

fn expect_token<'a>(tok: Token) -> impl Fn(Span<'a>) -> ParseResult<Token> {
    move |span: Span| -> ParseResult<Token> {
        let (new_span, actual_tok) = next_token(span)?;

        if actual_tok == tok {
            Ok((new_span, actual_tok))
        } else {
            new_parse_error(span, ErrorKind::Char)
        }
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
        let (new_span, tok) = next_token(span)?;
        span = new_span;

        match tok {
            Token::Char(_) | Token::Control(ControlKind::AtLiteral) => continue,

            Token::Control(ControlKind::NewMajorModule)
            | Token::Control(ControlKind::NewMinorModule) => return Ok((prev_span, ())),

            _ => return new_parse_error(span, ErrorKind::Char),
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
/// Module names are terminated by a terminator control token. Leading and
/// trailing whitespace are eaten, and inner whitespace is collapsed.
///
/// See WEAVE:103-104.
fn scan_module_name(span: Span) -> ParseResult<StringSpan> {
    let (span, start) = position(span)?;
    let (mut span, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;

    let mut value = String::new();
    let mut space_needed = false;

    loop {
        let (new_span, tok) = next_token(span)?;
        span = new_span;

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
    Ok((
        span,
        StringSpan {
            start,
            end,
            value: value.into(),
        },
    ))
}

/// Placeholder (??)
fn skip_to_pascal(mut span: Span) -> ParseResult<()> {
    loop {
        let (new_span, tok) = next_token(span)?;
        span = new_span;

        match tok {
            Token::Control(ControlKind::StartUnnamedPascal) => return Ok((span, ())),

            Token::Control(ControlKind::ModuleName) => {
                let (span, _) = scan_module_name(span)?;
                let (span, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;
                let (span, _) = char('=')(span)?;
                return Ok((span, ()));
            }

            _ => continue,
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
enum DelimiterKind {
    Paren,

    /// A `@{` or `@}` meta-comment. Note that these need not be balanced.
    MetaComment,

    /// Obtained with the `(.` digraph.
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

#[derive(Debug)]
enum IndexEntryKind {
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

    /// The `@<` control code: a reference to a module.
    ModuleRef(StringSpan<'a>),

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

    StringPoolChecksum,

    DefinitionFlag,

    CancelDefinitionFlag,

    IntLiteral(IntLiteralKind, usize),

    StringLiteral(StringLiteralKind, StringSpan<'a>),

    IndexEntry(IndexEntryKind, StringSpan<'a>),

    Comment(StringSpan<'a>),
}

impl<'a> fmt::Display for PascalToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PascalToken::Comment(s) => write!(f, "Comment({:?})", s.value),
            PascalToken::Identifier(s) => write!(f, "Identifier({:?})", s.value),
            PascalToken::IndexEntry(k, s) => write!(f, "IndexEntry({:?}, {:?})", k, s.value),
            PascalToken::ModuleRef(s) => write!(f, "ModuleRef({:?})", s.value),
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

    loop {
        let (new_span, tok) = next_token(span)?;
        span = new_span;

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

/// TODO: clarify about resolving partial names to full names
fn match_module_reference_token(span: Span) -> ParseResult<PascalToken> {
    let (span, _) = expect_token(Token::Control(ControlKind::ModuleName))(span)?;
    let (span, text) = scan_module_name(span)?;
    Ok((span, PascalToken::ModuleRef(text)))
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

    loop {
        let (new_span, tok) = next_token(span)?;
        span = new_span;

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
        match_module_reference_token,
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

fn first_pass_inner(span: Span) -> ParseResult<()> {
    let (mut span, _) = skip_limbo(span)?;

    loop {
        let (ispan, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;
        let (ispan, _) = skip_to_pascal(ispan)?;

        let (ispan, toks) = many0(match_pascal_token)(ispan)?;

        println!("Got some pascal toks ({}):", toks.len());
        for tok in &toks {
            println!("- {}", tok);
        }
        println!();
        println!("Stopped at: {:?}", &ispan[..32]);

        // If we (properly) finished a chunk of Pascal, the next thing is a new module.
        let (ispan, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(ispan)?;
        let (ispan, _) = alt((
            expect_token(Token::Control(ControlKind::NewMajorModule)),
            expect_token(Token::Control(ControlKind::NewMinorModule)),
        ))(ispan)?;
        span = ispan;
    }
}

pub fn first_pass(span: Span) -> Result<()> {
    match first_pass_inner(span).finish() {
        Ok((_remainder, _value)) => Ok(()),
        Err((_remainder, kind)) => Err(anyhow!(kind.description().to_owned())),
    }
}
