pub mod ast;
pub mod frontend;
pub mod ty;

use failure::{self, format_err};
use std::fs;
use std::path::Path;

pub fn compile(source: &str) -> Result<(), failure::Error> {
    let mut ctx = ast::Context::default();
    let mut lexer = frontend::lexer::Lexer::new(source);
    let parser = frontend::parser::ProgramParser::new();
    parser
        .parse(&mut ctx, &mut lexer)
        .map_err(|e| format_err!("parse error: {:?}", e))?;
    Ok(())
}

pub fn compile_file(path: &Path) -> Result<(), failure::Error> {
    let source = fs::read_to_string(path)?;
    compile(&source)
}
