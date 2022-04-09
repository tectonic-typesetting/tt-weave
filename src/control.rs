//! WEB control codes.

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ControlKind {
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
    pub fn syntax_char(&self) -> char {
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
