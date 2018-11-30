use cool_rs::ast;
use cool_rs::frontend;
use cool_rs::ty;
use std::fs;
use std::path::Path;

fn assert_typing(input: &Path, expected: &Path) {
    let source = fs::read_to_string(input).unwrap();
    let expected = fs::read_to_string(expected).unwrap();
    let expect_err = expected.contains("Compilation halted due to static semantic errors.");

    let mut ctx = ast::Context::default();
    let mut lexer = frontend::lexer::Lexer::new(&source);
    let parser = frontend::parser::ProgramParser::new();
    let classes = parser.parse(&mut ctx, &mut lexer).unwrap();

    unimplemented!()
}

include!(concat!(env!("OUT_DIR"), "/typing_tests.rs"));
