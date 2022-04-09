//! Reserved words in WEB's Pascal.

use std::{convert::TryFrom, fmt};

/// Reserved words in WEB's Pascal.
///
/// See WEAVE:64.
#[derive(Debug)]
pub enum PascalReservedWord {
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
