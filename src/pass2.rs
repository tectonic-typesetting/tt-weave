//! The second pass -- emitting TeX

use nom::{bytes::complete::take_while, character::complete::char, error::ErrorKind, Finish};
use std::borrow::Cow;
use syntect::highlighting::ThemeSet;
use tectonic_errors::prelude::*;

use crate::{
    control::ControlKind,
    parse_base::{new_parse_error, ParseResult, Span, SpanValue, StringSpan},
    pascal_token::PascalToken,
    prettify::Prettifier,
    reserved::PascalReservedWord,
    state::{ModuleId, State},
    token::{next_token, Token},
    weblang::{TypesetComment, WebCode, WebSyntax, WebToken},
};

#[derive(Debug, Default)]
struct OutputState {
    col: usize,
    saw_phantom: bool,
}

impl OutputState {
    fn printc(&mut self, c: char) {
        if c == '\n' {
            if self.col != 0 || !self.saw_phantom {
                print!("{}", c);
            }
            self.col = 0;
            self.saw_phantom = false;
        } else {
            print!("{}", c);
            self.col += 1
        }
    }

    fn prints<S: AsRef<str>>(&mut self, s: S) {
        for c in s.as_ref().chars() {
            self.printc(c);
        }
    }

    fn see_phantom(&mut self) {
        self.saw_phantom = true;
    }
}

/// WEAVE:132, `copy_limbo`, or so.
fn copy_limbo<'a>(output: &mut OutputState, mut span: Span<'a>) -> ParseResult<'a, Token> {
    let mut tok;

    loop {
        (span, tok) = next_token(span)?;

        match tok {
            Token::Char(c) => {
                output.printc(c);
            }

            Token::Control(ControlKind::AtLiteral) => {
                output.printc('@');
            }

            Token::Control(ControlKind::NewMajorModule)
            | Token::Control(ControlKind::NewMinorModule) => return Ok((span, tok)),

            _ => return new_parse_error(span, ErrorKind::Char),
        }
    }
}

/// WEAVE:134, `copy_tex`
///
/// TODO: may need to monitor linebreaks as in WEAVE finish_line, etc.,
/// to produce correct output with index entries.
fn copy_tex<'a>(output: &mut OutputState, mut span: Span<'a>) -> ParseResult<'a, Token> {
    let mut tok;

    loop {
        (span, tok) = next_token(span)?;

        match tok {
            Token::Char('|') | Token::Control(_) => return Ok((span, tok)),
            Token::Char(c) => {
                output.printc(c);
            }
        }
    }
}

/// WEAVE:136, `copy_comment`
fn copy_comment<'a>(mut depth: usize, mut span: Span<'a>) -> ParseResult<'a, (String, usize)> {
    let mut text = String::new();
    let mut tok;

    loop {
        (span, tok) = next_token(span)?;

        match tok {
            Token::Char('|') => return Ok((span, (text, depth))),

            Token::Char('\\') => {
                // WEAVE handles '\@' specially in a way that might give
                // different behavior than this, but it looks like that
                // construction basically doesn't arise in practice.
                text.push('\\');

                let c;
                (span, c) = next_token(span)?;
                c.push_syntax_into(&mut text);
            }

            Token::Char('{') => {
                text.push('{');
                depth += 1;
            }

            Token::Char('}') => {
                // TODO: decide how we want to handle comment typesetting
                //text.push('}');
                depth -= 1;

                if depth == 0 {
                    return Ok((span, (text, depth)));
                } else {
                    text.push('}');
                }
            }

            Token::Control(ControlKind::NewMajorModule)
            | Token::Control(ControlKind::NewMinorModule) => {
                return new_parse_error(span, ErrorKind::Char)
            }

            other => {
                other.push_syntax_into(&mut text);
            }
        }
    }
}

fn scan_pascal_only<'a>(
    mut span: Span<'a>,
    state: &State,
) -> ParseResult<'a, (Vec<PascalToken<'a>>, Token)> {
    let mut ptoks = Vec::new();
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
                return Ok((span, (ptoks, tok)));
            }
            _ => {}
        }

        // Looks like we still have Pascal. Now parse it as such.

        (span, ptok) = state.match_pascal_token_with_formats(prev_span)?;

        match ptok {
            PascalToken::IndexEntry(_, _)
            | PascalToken::DefinitionFlag
            | PascalToken::CancelDefinitionFlag
            | PascalToken::ForcedEol
            | PascalToken::Formatting => {}

            // Occasionally TexStrings are used as placeholders inside Pascal
            // expressions in inline Pascal expressions. If we elide them
            // entirely, this causes WEB parser to choke. Our current hack is to
            // try to pick out the ones that are used this way and transform
            // them into identifiers. It *looks* like we can do this by
            // searching for ones that wrap math expressions.
            PascalToken::TexString(sv) => {
                let text = sv.value.as_ref();

                if let Some(t) = text.strip_prefix("$") {
                    if let Some(t) = t.strip_suffix("$") {
                        ptoks.push(PascalToken::Identifier(StringSpan {
                            start: sv.start.clone(),
                            end: sv.start.clone(),
                            value: Cow::Owned(t.to_owned()),
                        }));
                    }
                }
            }

            other => {
                ptoks.push(other);
            }
        }
    }
}

fn scan_pascal<'a>(mut span: Span<'a>, state: &State) -> ParseResult<'a, (WebSyntax<'a>, Token)> {
    let mut code = Vec::new();
    let mut tok;
    let mut ptoks;

    let mut prev_span = span;
    (span, tok) = next_token(span)?;

    loop {
        match tok {
            Token::Char('{') => {
                let mut comment = Vec::new();
                let text;
                let mut depth;

                (span, (text, depth)) = copy_comment(1, span)?;
                comment.push(TypesetComment::Tex(text));

                while depth > 0 {
                    (span, (ptoks, tok)) = scan_pascal_only(span, state)?;
                    comment.push(TypesetComment::Pascal(ptoks));

                    if let Token::Char('|') = tok {
                        let text;
                        (span, (text, depth)) = copy_comment(depth, span)?;
                        comment.push(TypesetComment::Tex(text));
                    } else {
                        return new_parse_error(span, ErrorKind::Char);
                    }
                }

                code.push(WebToken::Comment(comment));
                prev_span = span;
                (span, tok) = next_token(span)?;
            }

            Token::Control(ControlKind::MacroDefinition)
            | Token::Control(ControlKind::FormatDefinition)
            | Token::Control(ControlKind::StartUnnamedPascal)
            | Token::Control(ControlKind::ModuleName)
            | Token::Control(ControlKind::NewMinorModule)
            | Token::Control(ControlKind::NewMajorModule) => {
                return Ok((span, (WebSyntax(code), tok)));
            }

            _ => {
                (span, (ptoks, tok)) = scan_pascal_only(prev_span, state)?;
                code.extend(ptoks.drain(..).map(|t| WebToken::Pascal(t)));
            }
        }
    }
}

fn emit_pascal<'a>(syntax: WebSyntax<'a>, inline: bool) {
    // parse into the AST

    let code = WebCode::parse(&syntax).expect("parse failed");

    // Prettify

    let mut pretty = Prettifier::new_inline(inline);
    let mut first = true;

    for tl in &code.0 {
        if first {
            first = false;
        } else {
            pretty.toplevel_separator();
        }

        tl.prettify(&mut pretty);
    }

    // Emit with highlighting.

    let ts = ThemeSet::load_defaults();
    let theme = &ts.themes["InspiredGitHub"];
    pretty.emit(theme, inline);
}

/// WEAVE:222
fn handle_tex<'a>(
    state: &State,
    output: &mut OutputState,
    mut span: Span<'a>,
) -> ParseResult<'a, Token> {
    let mut tok;

    (span, tok) = copy_tex(output, span)?;

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

            Token::Char('|') => {
                let mut ptoks;
                (span, (ptoks, _)) = scan_pascal_only(span, state)?;
                let wrapped = ptoks.drain(..).map(|t| WebToken::Pascal(t)).collect();
                emit_pascal(WebSyntax(wrapped), true);
                (span, tok) = copy_tex(output, span)?;
            }

            Token::Char(c) => {
                output.printc(c);
                (span, tok) = copy_tex(output, span)?;
            }

            Token::Control(ControlKind::AtLiteral) => {
                output.printc('@');
                (span, tok) = copy_tex(output, span)?;
            }

            //WEAVE:223. See also stringification of PascalToken IntLiteral values.
            Token::Control(ControlKind::OctalLiteral) => {
                let value;
                (span, value) = crate::pascal_token::scan_octal_literal(span)?;
                output.prints(format!("\\WebOctalLiteralHexed{{{:X}}}", value));
                (span, tok) = copy_tex(output, span)?;
            }

            //WEAVE:223
            Token::Control(ControlKind::HexLiteral) => {
                let value;
                (span, value) = crate::pascal_token::scan_hex_literal(span)?;
                output.prints(format!("\\WebHexLiteral{{{:X}}}", value));
                (span, tok) = copy_tex(output, span)?;
            }

            Token::Control(ControlKind::RomanIndexEntry)
            | Token::Control(ControlKind::TypewriterIndexEntry)
            | Token::Control(ControlKind::WildcardIndexEntry) => {
                output.see_phantom();
                (span, tok) = state.scan_next(span)?;
            }

            Token::Control(ControlKind::DefinitionFlag) => {
                (span, tok) = copy_tex(output, span)?;
            }

            other => {
                eprintln!("** unhandled TeX control code in pass2: {:?}", other);
                return new_parse_error(span, ErrorKind::Char);
            }
        }
    }
}

/// WEAVE:225-228.
fn handle_definitions<'a>(
    state: &State,
    mut span: Span<'a>,
    mut tok: Token,
) -> ParseResult<'a, Token> {
    let mut ptoks;

    loop {
        match tok {
            Token::Control(ControlKind::NewMajorModule)
            | Token::Control(ControlKind::NewMinorModule)
            | Token::Control(ControlKind::StartUnnamedPascal)
            | Token::Control(ControlKind::ModuleName) => {
                return Ok((span, tok));
            }

            Token::Control(ControlKind::MacroDefinition) => {
                let mut code;
                (span, (code, tok)) = scan_pascal(span, state)?;
                code.0.insert(
                    0,
                    WebToken::Pascal(PascalToken::ReservedWord(SpanValue {
                        start: Span::new(""),
                        end: Span::new(""),
                        value: PascalReservedWord::Define,
                    })),
                );
                emit_pascal(code, false);
            }

            Token::Control(ControlKind::FormatDefinition) => {
                let mut code = vec![WebToken::Pascal(PascalToken::ReservedWord(SpanValue {
                    start: Span::new(""),
                    end: Span::new(""),
                    value: PascalReservedWord::Format,
                }))];
                let ptok;

                (span, ptok) = state.match_pascal_token_with_formats(span)?;
                code.push(WebToken::Pascal(ptok.clone()));

                if let PascalToken::Identifier(_) = ptok {
                    let ptok2;
                    (span, ptok2) = state.match_pascal_token_with_formats(span)?;
                    code.push(WebToken::Pascal(ptok2.clone()));

                    if let PascalToken::Equivalence = ptok2 {
                        let ptok3;
                        (span, ptok3) = state.match_pascal_token_with_formats(span)?;
                        code.push(WebToken::Pascal(ptok3));
                    }
                }

                let mut rest;
                (span, (rest, tok)) = scan_pascal(span, state)?;
                code.append(&mut rest.0);
                emit_pascal(WebSyntax(code), false);
            }

            Token::Control(ControlKind::RomanIndexEntry) => {
                (span, tok) = state.scan_next(span)?;
            }
            Token::Control(ControlKind::TypewriterIndexEntry) => {
                (span, tok) = state.scan_next(span)?;
            }
            Token::Control(ControlKind::WildcardIndexEntry) => {
                (span, tok) = state.scan_next(span)?;
            }

            Token::Char('|') => {
                (span, (ptoks, tok)) = scan_pascal_only(span, state)?;
                let wrapped = ptoks.drain(..).map(|t| WebToken::Pascal(t)).collect();
                emit_pascal(WebSyntax(wrapped), true);
            }

            _ => {
                (span, tok) = next_token(span)?;
            }
        }
    }
}

fn handle_pascal<'a>(state: &State, mut span: Span<'a>) -> ParseResult<'a, Token> {
    let mut tok;

    let mut prev_span = span.clone();
    (span, tok) = next_token(span)?;

    let mut code = Vec::new();

    loop {
        match tok {
            Token::Control(ControlKind::NewMajorModule)
            | Token::Control(ControlKind::NewMinorModule) => {
                emit_pascal(WebSyntax(code), false);
                return Ok((span, tok));
            }

            Token::Control(ControlKind::ModuleName) => {
                let mod_name;
                (span, mod_name) = state.scan_module_name(span)?;
                code.push(WebToken::ModuleReference(mod_name));

                prev_span = span.clone();
                (span, tok) = next_token(span)?;
            }

            _ => {
                let mut block;
                (span, (block, tok)) = scan_pascal(prev_span, state)?;
                code.append(&mut block.0);
            }
        }
    }
}

/// WEAVE:218, WEAVE:220, etc.
fn second_pass_inner<'a>(basename: &str, state: &State, span: Span<'a>) -> ParseResult<'a, ()> {
    let mut output = OutputState::default();

    // Note: we *don't* start by emitting `\input webmac` ...
    output
        .prints("% Generated by tt-weave\n% Note: webmac.tex is (intentionally) not loaded here\n");
    let (mut span, mut tok) = copy_limbo(&mut output, span)?;
    let mut cur_module: ModuleId = 0;

    // ... but we do have a hack to allow overrides of "limbo" macros
    output.prints(format!("\n\\input{{{}-overrides.tex}}\n", basename));

    loop {
        // At the top of this loop, we've just read a new-module boundary token.
        // At the moment we don't really care about major vs minor.
        cur_module += 1;
        match tok {
            Token::Control(ControlKind::NewMajorModule) => {
                output.prints(format!("\n\\WebMajorModule{{{}}} ", cur_module));
            }
            Token::Control(ControlKind::NewMinorModule) => {
                output.prints(format!("\n\\WebMinorModule{{{}}} ", cur_module));
            }
            _ => {
                eprintln!("unexpected module end {:?}", tok);
                return new_parse_error(span, ErrorKind::Complete);
            }
        }

        // Handle the TeX chunk (which can be empty), and find out what ended it.

        (span, tok) = handle_tex(state, &mut output, span)?;

        // If there are macro/format definitions, handle those

        match tok {
            Token::Control(ControlKind::MacroDefinition)
            | Token::Control(ControlKind::FormatDefinition) => {
                (span, tok) = handle_definitions(state, span, tok)?;
            }
            _ => {}
        }

        // If there's Pascal, handle that

        match tok {
            Token::Control(ControlKind::StartUnnamedPascal) => {
                (span, tok) = handle_pascal(state, span)?;
            }

            Token::Control(ControlKind::ModuleName) => {
                (span, _) = state.scan_module_name(span)?;

                // there's like one module in XeTeX with a space between module name and equals sign
                (span, _) = take_while(|c| c == ' ' || c == '\t' || c == '\n')(span)?;
                (span, _) = char('=')(span)?;
                (span, tok) = handle_pascal(state, span)?;
            }

            _ => {}
        }
    }
}

pub fn execute(basename: &str, state: &State, span: Span) -> Result<()> {
    match second_pass_inner(basename, state, span).finish() {
        Ok((_remainder, _value)) => {}
        Err((_remainder, ErrorKind::Eof)) => {}
        Err((_remainder, kind)) => return Err(anyhow!(kind.description().to_owned())),
    }

    Ok(())
}

// Start:
// ```
// let ps = SyntaxSet::load_defaults_newlines();
// let ts = ThemeSet::load_defaults();
// let syntax = ps.find_syntax_by_extension("rs").unwrap();
// ```
//
// In most cases you'd get scope stack operations with
// `syntect::parsing::ParseState::parse_line`. But we're generating this stuff
// manually!
//
// Use `syntect::highlighting::HighlightIterator`:
// ```
// pub fn new(
//     state: &'a mut HighlightState,
//     changes: &'a [(usize, ScopeStackOp)],
//     text: &'b str,
//     highlighter: &'a Highlighter<'_>
// ) -> HighlightIterator<'a, 'b>ⓘ
// ```
//
// Generates sequence of `(Style, &str)`. That can be converted to HTML.
