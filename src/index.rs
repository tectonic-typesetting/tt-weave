//! Types for indexing.

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum IndexEntryKind {
    /// Auto-sourced from Pascal code; printed in italics
    Normal,

    /// `@^`: for human language
    Roman,

    /// `@.`: for UI strings
    Typewriter,

    /// `@:`: used for custom TeX typesetting, essentially
    Wildcard,
}
