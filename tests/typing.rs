use cool_rs::ast;
use cool_rs::frontend;
use std::fs;
use std::path::Path;

fn assert_typing(input: &Path, expected: &Path) {
    let source = fs::read_to_string(input).unwrap();
    let expected = fs::read_to_string(expected).unwrap();
    let expect_err = expected.contains("Compilation halted due to static semantic errors.");

    let mut ctx = ast::Context::default();
    let mut lexer = frontend::lexer::Lexer::new(&source);
    let parser = frontend::parser::ProgramParser::new();
    let _classes = parser.parse(&mut ctx, &mut lexer).unwrap();

    match ctx.type_check() {
        Ok(()) if expect_err => panic!("Expected type checking errors, but found none!"),
        Err(ref e) if !expect_err => panic!("Found an unexpected type checking error: {}", e),
        _ => {}
    }
}

include!(concat!(env!("OUT_DIR"), "/typing_tests.rs"));
