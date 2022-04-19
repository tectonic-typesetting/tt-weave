//! Higher-level WEB language processing.
//!
//! This is *mostly* Pascal, but with a few additions. This doesn't feel like an
//! awesome technique, but we implement parsing with `nom` where the underlying
//! datatype is a sequence of tokens.

use nom::{
    branch::alt,
    bytes::complete::{take_while, take_while1},
    combinator::{map, opt},
    error::{ErrorKind, ParseError as NomParseError},
    multi::{many1, separated_list0},
    sequence::tuple,
    Err, Finish, IResult, InputIter, InputLength, InputTake, Needed, Slice, UnspecializedInput,
};
use std::{
    iter::{Cloned, Enumerate},
    slice::Iter,
};

use crate::{
    parse_base::{SpanValue, StringSpan},
    pascal_token::{DelimiterKind, PascalToken},
    reserved::PascalReservedWord,
};

/// Information about a typeset comment.
///
/// This type is lame. The structure is an interleaving of TeX code and inline
/// Pascal text, but our data structure doesn't capture that very effectively.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypesetComment<'a> {
    Pascal(Vec<PascalToken<'a>>),
    Tex(String),
}

/// A logical token of the WEB language, which we treat as a superset of Pascal.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebToken<'a> {
    /// A basic Pascal token
    Pascal(PascalToken<'a>),

    /// A typeset comment, which contains alternating bits of TeX code and Pascal
    /// token sequences.
    Comment(Vec<TypesetComment<'a>>),

    /// A reference to a WEB module.
    ModuleReference(StringSpan<'a>),
}

impl<'a> WebToken<'a> {
    pub fn as_pascal(&self) -> Option<&PascalToken> {
        if let WebToken::Pascal(ptok) = self {
            Some(ptok)
        } else {
            None
        }
    }

    pub fn into_pascal(self) -> PascalToken<'a> {
        if let WebToken::Pascal(ptok) = self {
            ptok
        } else {
            panic!("into_pascal() of non-Pascal WEB token");
        }
    }
}

/// A block of WEB syntax: just a sequence of WEB tokens.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebSyntax<'a>(pub Vec<WebToken<'a>>);

/// The parse input: a slice of tokens
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ParseInput<'a>(&'a [WebToken<'a>]);

impl<'a> InputLength for ParseInput<'a> {
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

/// This is a monkey-see-monkey-do impl based on how nom does things for slices.
/// The main difference is that unfortunately we have to clone instead of
/// copying.
impl<'a> InputIter for ParseInput<'a> {
    type Item = WebToken<'a>;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = Cloned<Iter<'a, WebToken<'a>>>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.iter_elements().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.0.iter().cloned()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(|b| predicate(b.clone()))
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.0.len() >= count {
            Ok(count)
        } else {
            Err(Needed::new(count - self.0.len()))
        }
    }
}

impl<'a, R> Slice<R> for ParseInput<'a>
where
    &'a [WebToken<'a>]: Slice<R>,
{
    fn slice(&self, range: R) -> Self {
        ParseInput(self.0.slice(range))
    }
}

impl<'a> InputTake for ParseInput<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        ParseInput(&self.0[0..count])
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (ParseInput(suffix), ParseInput(prefix))
    }
}

/// Implementing this gives is InputTakeAtPosition and Compare
impl<'a> UnspecializedInput for ParseInput<'a> {}

/// Our parse error kinds, including a lame catch-all for Nom's built-in ones.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum WebErrorKind {
    Eof,
    ExpectedPascalToken,
    ExpectedIdentifer,
    ExpectedStringLiteral,
    ExpectedIntLiteral,
    ExpectedComment,
    ExpectedToplevel,
    ExpectedReservedWord(PascalReservedWord),
    ExpectedAnyReservedWord,
    ExpectedOpenDelimiter(DelimiterKind),
    ExpectedCloseDelimiter(DelimiterKind),
    Nom(ErrorKind),
}

/// The parse error type.
type ParseError<'a> = (ParseInput<'a>, WebErrorKind);

impl<'a> NomParseError<ParseInput<'a>> for ParseError<'a> {
    fn from_error_kind(input: ParseInput<'a>, kind: ErrorKind) -> Self {
        (input, WebErrorKind::Nom(kind))
    }

    fn append(_: ParseInput<'a>, _: ErrorKind, other: Self) -> Self {
        other
    }
}

/// The parse result type.
type ParseResult<'a, T> = IResult<ParseInput<'a>, T, ParseError<'a>>;

fn new_parse_err<'a, T>(s: ParseInput<'a>, k: WebErrorKind) -> ParseResult<'a, T> {
    Err(Err::Error((s, k)))
}

// Low-level parse tools

fn next_token<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToken<'a>> {
    let wt = input
        .iter_elements()
        .next()
        .ok_or_else(|| Err::Error((input, WebErrorKind::Eof)))?;
    Ok((input.slice(1..), wt))
}

/// Expect a specific Pascal token.
///
/// This matches use an equality test, so it's probably only what you want if
/// the token variant you're testing for is a content-less one.
fn pascal_token<'a>(
    expected: PascalToken<'a>,
) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    move |input: ParseInput<'a>| {
        let (input, wt) = next_token(input)?;

        if let WebToken::Pascal(found) = wt {
            if found == expected {
                return Ok((input, found));
            }
        }

        return new_parse_err(input, WebErrorKind::ExpectedPascalToken);
    }
}

/// Expect a Pascal identifier, returning its text.
fn identifier<'a>(input: ParseInput<'a>) -> ParseResult<'a, StringSpan<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(PascalToken::Identifier(s)) = wt {
        Ok((input, s))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedIdentifer);
    }
}

/// Expect a Pascal reserved word, returning its span-value.
fn reserved_word<'a>(
    rw: PascalReservedWord,
) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, SpanValue<'a, PascalReservedWord>> {
    move |input: ParseInput<'a>| {
        let (input, wt) = next_token(input)?;

        if let WebToken::Pascal(PascalToken::ReservedWord(sv)) = wt {
            if sv.value == rw {
                return Ok((input, sv));
            }
        }

        return new_parse_err(input, WebErrorKind::ExpectedReservedWord(rw));
    }
}

/// Accept any Pascal reserved word.
fn any_reserved_word<'a>(
    input: ParseInput<'a>,
) -> ParseResult<'a, SpanValue<'a, PascalReservedWord>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(PascalToken::ReservedWord(sv)) = wt {
        return Ok((input, sv));
    }

    return new_parse_err(input, WebErrorKind::ExpectedAnyReservedWord);
}

/// Expect a Pascal string literal token, returning it.
fn string_literal<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(lit @ PascalToken::StringLiteral(..)) = wt {
        Ok((input, lit))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedStringLiteral);
    }
}

/// Expect a Pascal integer literal token, returning it.
fn int_literal<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalToken<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Pascal(lit @ PascalToken::IntLiteral(..)) = wt {
        Ok((input, lit))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedIntLiteral);
    }
}

/// An open delimiter.
fn open_delimiter<'a>(kind: DelimiterKind) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, ()> {
    move |input: ParseInput<'a>| {
        let (input, wt) = next_token(input)?;

        if let WebToken::Pascal(PascalToken::OpenDelimiter(found_kind)) = wt {
            if found_kind == kind {
                return Ok((input, ()));
            }
        }

        return new_parse_err(input, WebErrorKind::ExpectedOpenDelimiter(kind));
    }
}

/// A close delimiter.
fn close_delimiter<'a>(kind: DelimiterKind) -> impl Fn(ParseInput<'a>) -> ParseResult<'a, ()> {
    move |input: ParseInput<'a>| {
        let (input, wt) = next_token(input)?;

        if let WebToken::Pascal(PascalToken::CloseDelimiter(found_kind)) = wt {
            if found_kind == kind {
                return Ok((input, ()));
            }
        }

        return new_parse_err(input, WebErrorKind::ExpectedCloseDelimiter(kind));
    }
}

/// Expect a comment, returning it.
fn comment<'a>(input: ParseInput<'a>) -> ParseResult<'a, Vec<TypesetComment<'a>>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::Comment(c) = wt {
        Ok((input, c))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedComment);
    }
}

/// Expect a module reference, returning its value.
fn module_reference<'a>(input: ParseInput<'a>) -> ParseResult<'a, StringSpan<'a>> {
    let (input, wt) = next_token(input)?;

    if let WebToken::ModuleReference(s) = wt {
        Ok((input, s))
    } else {
        return new_parse_err(input, WebErrorKind::ExpectedIdentifer);
    }
}

/// A top-level WEB production.
///
/// Because we're not actually compiling the WEB language in any meaningful we,
/// we're pretty sloppy with the definition of "toplevel" here. It's anything
/// that can show up as a top-level item in WEB code, including `@define` and
/// `@format` statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebToplevel<'a> {
    /// A `@d` definition.
    Define(WebDefine<'a>),

    /// A `@f` format definition.
    Format(WebFormat<'a>),

    /// A module reference.
    ModuleReference(StringSpan<'a>),

    /// A single Pascal token.
    ///
    /// To match: string literal, int literal, texstring, identifier
    Standalone(WebStandalone<'a>),

    /// The program definition.
    ProgramDefinition(WebProgramDefinition<'a>),

    /// A label declaration.
    LabelDeclaration(WebLabelDeclaration<'a>),

    /// Declarations that are done by referencing a module.
    ModulifiedDeclaration(WebModulifiedDeclaration<'a>),

    /// Definition of a procedure or function
    FunctionDefinition(WebFunctionDefinition<'a>),

    /// A meta-comment for WEB's version of `#ifdef` processing
    IfdefLike(WebIfdefLike<'a>),
}

/// A block of WEB code: a sequence of parsed-out WEB toplevels
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebCode<'a>(pub Vec<WebToplevel<'a>>);

impl<'a> WebCode<'a> {
    pub fn parse(syntax: &'a WebSyntax<'a>) -> Option<WebCode<'a>> {
        let input = ParseInput(&syntax.0[..]);

        match many1(parse_toplevel)(input).finish() {
            Ok((remainder, value)) => {
                if remainder.input_len() > 0 {
                    eprintln!("\nincomplete parse");
                    return None;
                } else {
                    return Some(WebCode(value));
                }
            }

            Err((_remainder, e)) => {
                eprintln!("parse error: {:?}", e);
                return None;
            }
        }
    }
}

fn parse_toplevel<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    fn is_ignored_token(t: WebToken) -> bool {
        match t {
            WebToken::Pascal(PascalToken::Formatting)
            | WebToken::Pascal(PascalToken::ForcedEol)
            | WebToken::Pascal(PascalToken::TexString(..)) => true,
            _ => false,
        }
    }

    let (input, _) = take_while(is_ignored_token)(input)?;

    fn parse_fails<'b>(input: ParseInput<'b>) -> ParseResult<'b, WebToplevel<'b>> {
        if input.input_len() == 0 {
            new_parse_err(input, WebErrorKind::Eof)
        } else {
            eprintln!("\n\nTL fail at: {:?}\n", input);
            new_parse_err(input, WebErrorKind::ExpectedToplevel)
        }
    }

    alt((
        // Define comes first since its tail is a toplevel in and of itself.
        parse_define,
        parse_format,
        map(module_reference, |s| WebToplevel::ModuleReference(s)),
        parse_program_definition,
        parse_label_declaration,
        parse_modulified_declaration,
        parse_function_definition,
        parse_ifdef_like,
        // This goes second-to-last since it will match nearly anything
        parse_standalone,
        // This goes last for debugging
        parse_fails,
    ))(input)
}

/// A `@d` definition
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebDefine<'a> {
    /// The LHS of the define. This may be a sequence of tokens like `blah(#)`.
    lhs: Vec<PascalToken<'a>>,

    /// The RHS. This could be anything, including partial bits of syntax.
    rhs: Box<WebToplevel<'a>>,
}

fn parse_define<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    fn is_define_lhs_token(t: WebToken) -> bool {
        if let Some(pt) = t.as_pascal() {
            match pt {
                PascalToken::Equals | PascalToken::Equivalence => false,
                _ => true,
            }
        } else {
            false
        }
    }

    let (input, items) = tuple((
        reserved_word(PascalReservedWord::Define),
        take_while1(is_define_lhs_token),
        alt((
            pascal_token(PascalToken::Equivalence),
            pascal_token(PascalToken::Equals),
        )),
        parse_toplevel,
    ))(input)?;

    Ok((
        input,
        WebToplevel::Define(WebDefine {
            lhs: items.1 .0.iter().map(|t| t.clone().into_pascal()).collect(),
            rhs: Box::new(items.3),
        }),
    ))
}

/// A `@f` format definition
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebFormat<'a> {
    /// The LHS of the format: an identifier.
    lhs: StringSpan<'a>,

    /// The RHS: a reserved word
    rhs: PascalReservedWord,
}

fn parse_format<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::Format),
        identifier,
        pascal_token(PascalToken::Equivalence),
        any_reserved_word,
    ))(input)?;

    Ok((
        input,
        WebToplevel::Format(WebFormat {
            lhs: items.1,
            rhs: items.3.value,
        }),
    ))
}

/// A "standalone" token with an optional comment.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebStandalone<'a> {
    /// The token.
    token: PascalToken<'a>,

    /// An optional associated comment.
    comment: Option<Vec<TypesetComment<'a>>>,
}

/// A single Pascal token, maybe followed by a comment.
fn parse_standalone<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, token) = alt((
        map(identifier, |s| PascalToken::Identifier(s)),
        string_literal,
        int_literal,
    ))(input)?;

    let (input, comment) = opt(comment)(input)?;

    Ok((
        input,
        WebToplevel::Standalone(WebStandalone { token, comment }),
    ))
}

/// The top-level program definition
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebProgramDefinition<'a> {
    name: StringSpan<'a>,
    args: Vec<StringSpan<'a>>,
}

/// A Pascal program definition
///
/// `PROGRAM $name($arg1, ...);`
///
/// Really this should have the same structure as a procedure definition, but
/// WEB implementations always split the program across the entire source file,
/// so the program definition is always incomplete.
fn parse_program_definition<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::Program),
        identifier,
        open_delimiter(DelimiterKind::Paren),
        separated_list0(pascal_token(PascalToken::Comma), identifier),
        close_delimiter(DelimiterKind::Paren),
        pascal_token(PascalToken::Semicolon),
    ))(input)?;

    Ok((
        input,
        WebToplevel::ProgramDefinition(WebProgramDefinition {
            name: items.1,
            args: items.3,
        }),
    ))
}

/// A label declaration.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebLabelDeclaration<'a> {
    /// The label name.
    name: StringSpan<'a>,

    /// An optional associated comment.
    comment: Option<Vec<TypesetComment<'a>>>,
}

fn parse_label_declaration<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, items) = tuple((
        reserved_word(PascalReservedWord::Label),
        identifier,
        pascal_token(PascalToken::Semicolon),
        opt(comment),
    ))(input)?;

    Ok((
        input,
        WebToplevel::LabelDeclaration(WebLabelDeclaration {
            name: items.1,
            comment: items.3,
        }),
    ))
}

/// A group of declarations done by referencing a module.
///
/// TODO: replace this with code used for functions below.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebModulifiedDeclaration<'a> {
    /// The kind of declaration
    kind: PascalReservedWord,

    /// The associated module
    module: StringSpan<'a>,
}

/// `(const|type|var) <module-ref>`
fn parse_modulified_declaration<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    fn declaration_keyword<'a>(input: ParseInput<'a>) -> ParseResult<'a, PascalReservedWord> {
        let (input, wt) = next_token(input)?;

        if let WebToken::Pascal(PascalToken::ReservedWord(sv)) = wt {
            match sv.value {
                PascalReservedWord::Const | PascalReservedWord::Type | PascalReservedWord::Var => {
                    return Ok((input, sv.value));
                }
                _ => {}
            }
        }

        new_parse_err(input, WebErrorKind::ExpectedPascalToken)
    }

    let (input, items) = tuple((declaration_keyword, module_reference))(input)?;

    Ok((
        input,
        WebToplevel::ModulifiedDeclaration(WebModulifiedDeclaration {
            kind: items.0,
            module: items.1,
        }),
    ))
}

/// Definition of a function or procedure.
///
/// For simplicity, we just call them both "functions".
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebFunctionDefinition<'a> {
    /// The name of the function.
    name: StringSpan<'a>,

    /// The function's arguments. Each of these WebVariables items should
    /// contain only one name.
    args: Vec<WebVariables<'a>>,

    /// The return type. If `Some`, this is a function; otherwise it is a
    /// procedure.
    return_type: Option<StringSpan<'a>>,

    /// The comment associated with the definition of the function.
    opening_comment: Option<Vec<TypesetComment<'a>>>,

    /// Records in the function's `var` block.
    vars: Vec<WebVarBlockItem<'a>>,

    /// The statements that comprise the function.
    stmts: Vec<WebStatement<'a>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebVariables<'a> {
    /// The name(s) of the variable(s).
    names: Vec<StringSpan<'a>>,

    /// The type of the variable(s).
    ty: StringSpan<'a>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebVarBlockItem<'a> {
    /// A reference to a module that (hopefully) contains variable definitions.
    ModuleReference(StringSpan<'a>),

    /// Actual in-place definitions
    InPlace(WebVariables<'a>),
}

fn parse_var_block_item<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebVarBlockItem<'a>> {
    alt((
        map(module_reference, |t| WebVarBlockItem::ModuleReference(t)),
        map(
            tuple((
                separated_list0(pascal_token(PascalToken::Comma), identifier),
                pascal_token(PascalToken::Colon),
                identifier,
                pascal_token(PascalToken::Semicolon),
            )),
            |tup| {
                WebVarBlockItem::InPlace(WebVariables {
                    names: tup.0,
                    ty: tup.2,
                })
            },
        ),
    ))(input)
}

fn parse_single_variable<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebVariables<'a>> {
    map(
        tuple((identifier, pascal_token(PascalToken::Colon), identifier)),
        |tup| WebVariables {
            names: vec![tup.0],
            ty: tup.2,
        },
    )(input)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WebStatement<'a> {
    /// A reference to a module.
    ModuleReference(StringSpan<'a>),
}

fn parse_statement<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebStatement<'a>> {
    // TODO: more!
    map(module_reference, |t| WebStatement::ModuleReference(t))(input)
}

fn parse_function_definition<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, items) = tuple((
        alt((
            reserved_word(PascalReservedWord::Function),
            reserved_word(PascalReservedWord::Procedure),
        )),
        identifier,
        opt(tuple((
            open_delimiter(DelimiterKind::Paren),
            separated_list0(pascal_token(PascalToken::Comma), parse_single_variable),
            close_delimiter(DelimiterKind::Paren),
        ))),
        opt(tuple((pascal_token(PascalToken::Colon), identifier))),
        pascal_token(PascalToken::Semicolon),
        opt(comment),
        opt(tuple((
            reserved_word(PascalReservedWord::Var),
            many1(parse_var_block_item),
        ))),
        reserved_word(PascalReservedWord::Begin),
        many1(parse_statement),
        reserved_word(PascalReservedWord::End),
        pascal_token(PascalToken::Semicolon),
    ))(input)?;

    let name = items.1;
    let args = items.2.map(|t| t.1).unwrap_or_default();
    let return_type = items.3.map(|t| t.1);
    let opening_comment = items.5;
    let vars = items.6.map(|t| t.1).unwrap_or_default();
    let stmts = items.8;

    Ok((
        input,
        WebToplevel::FunctionDefinition(WebFunctionDefinition {
            name,
            args,
            return_type,
            opening_comment,
            vars,
            stmts,
        }),
    ))
}

/// An `#ifdef`-like construct
///
/// Needed for `@define debug = { { blah blah }`
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebIfdefLike<'a> {
    /// Whether this is an "open" meta-comment or not (i.e., close).
    is_open: bool,

    /// An optional associated comment
    comment: Option<Vec<TypesetComment<'a>>>,
}

/// `(const|type|var) <module-ref>`
fn parse_ifdef_like<'a>(input: ParseInput<'a>) -> ParseResult<'a, WebToplevel<'a>> {
    let (input, items) = tuple((
        alt((
            pascal_token(PascalToken::OpenDelimiter(DelimiterKind::MetaComment)),
            pascal_token(PascalToken::CloseDelimiter(DelimiterKind::MetaComment)),
        )),
        opt(comment),
    ))(input)?;

    let is_open = if let PascalToken::OpenDelimiter(DelimiterKind::MetaComment) = items.0 {
        true
    } else {
        false
    };

    let comment = items.1;

    Ok((
        input,
        WebToplevel::IfdefLike(WebIfdefLike { is_open, comment }),
    ))
}
