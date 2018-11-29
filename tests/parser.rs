use cool_rs::ast;
use cool_rs::ast::DumpAst;
use cool_rs::frontend;
use std::fs;
use std::path::Path;

fn assert_parser(input: &Path, expected: &Path) {
    let source = fs::read_to_string(input).unwrap();
    let expected = fs::read_to_string(expected).unwrap();
    let expect_err = expected.contains("Compilation halted due to lex and parse errors");

    let mut ctx = ast::Context::default();
    let mut lexer = frontend::lexer::Lexer::new(&source);
    let parser = frontend::parser::ProgramParser::new();

    match parser.parse(&mut ctx, &mut lexer) {
        Err(_) if expect_err => return,
        Err(e) => panic!("Parsing failed, but it was not supposed to: {}", e),
        Ok(_) if expect_err => panic!(
            "Parsing succeeded, but it was supposed to fail with:\n\n{}",
            expected
        ),
        Ok(classes) => {
            let filename = format!("\"{}\"", input.file_name().unwrap().to_str().unwrap());
            let expected: Vec<_> = expected
                .lines()
                // Ignore line numbers.
                .filter(|l| !l.trim_start().starts_with("#"))
                // Ignore file names.
                .filter(|l| l.trim() != &filename)
                .collect();
            let expected = expected.join("\n");

            let mut actual = String::new();
            classes.dump_ast(&ctx, &mut actual, 0);

            let mut any_diff = false;

            println!("");
            println!("--- expected");
            println!("+++ actual");
            for d in diff::lines(expected.trim(), actual.trim()) {
                match d {
                    diff::Result::Left(l) => {
                        println!("-{}", l);
                        any_diff = true;
                    }
                    diff::Result::Right(r) => {
                        println!("+{}", r);
                        any_diff = true;
                    }
                    diff::Result::Both(b, _) => println!(" {}", b),
                }
            }

            if any_diff {
                panic!("parsed an unexpected AST");
            }
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/parser_tests.rs"));
