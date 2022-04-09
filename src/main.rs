use clap::Parser;
use std::path::PathBuf;
use tectonic_errors::prelude::*;

mod control;
mod index;
mod parse_base;
mod parsers;
mod pascal_token;
mod reserved;
mod token;

/// CLI arguments.
#[derive(Parser, Debug)]
#[clap(version, about, long_about = None)]
struct Args {
    /// Name of the input WEB file to process
    #[clap()]
    input_path: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Make life easy on ourselves: just read the input into a huge string.
    let text = atry!(
        std::fs::read_to_string(&args.input_path);
        ["failed to read input path `{}` as text", args.input_path.display()]
    );

    let input = parse_base::Span::new(&text);

    // First pass
    parsers::first_pass(input)?;

    Ok(())
}
