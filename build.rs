use std::env;
use std::fs;
use std::path::Path;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    build_tests();
    build_lalrpop();
}

fn build_tests() {
    build_lexer_tests();
    build_parser_tests();
    build_typing_tests();
}

fn build_lexer_tests() {
    let mut lexer_tests = String::new();
    println!("cargo:rerun-if-changed=tests/lexer.rs");
    println!("cargo:rerun-if-changed=tests/lexer/");
    for entry in glob::glob("tests/lexer/*.cool").unwrap() {
        let path = entry.unwrap();
        println!("cargo:rerun-if-changed={}", path.display());

        let name: String = path
            .display()
            .to_string()
            .chars()
            .map(|c| match c {
                'a'...'z' | 'A'...'Z' | '0'...'9' => c,
                _ => '_',
            })
            .collect();

        lexer_tests.push_str(&format!(
            r#"
                #[test]
                fn {name}() {{
                    assert_lexer(Path::new("{path}"), Path::new("{path}.out"));
                }}
            "#,
            name = name,
            path = path.display(),
        ));
    }
    fs::write(
        Path::new(&env::var("OUT_DIR").unwrap()).join("lexer_tests.rs"),
        lexer_tests.as_bytes(),
    )
    .unwrap();
}

fn build_parser_tests() {
    let mut parser_tests = String::new();
    println!("cargo:rerun-if-changed=tests/parser.rs");
    println!("cargo:rerun-if-changed=tests/parser/");
    for entry in glob::glob("tests/parser/*.test").unwrap() {
        let path = entry.unwrap();
        println!("cargo:rerun-if-changed={}", path.display());

        let name: String = path
            .display()
            .to_string()
            .chars()
            .map(|c| match c {
                'a'...'z' | 'A'...'Z' | '0'...'9' => c,
                '+' => 'p',
                '-' => 'm',
                _ => '_',
            })
            .collect();

        parser_tests.push_str(&format!(
            r#"
                #[test]
                fn {name}() {{
                    assert_parser(Path::new("{path}"), Path::new("{path}.out"));
                }}
            "#,
            name = name,
            path = path.display(),
        ));
    }
    fs::write(
        Path::new(&env::var("OUT_DIR").unwrap()).join("parser_tests.rs"),
        parser_tests.as_bytes(),
    )
    .unwrap();
}

fn build_typing_tests() {
    let mut typing_tests = String::new();
    println!("cargo:rerun-if-changed=tests/typing.rs");
    println!("cargo:rerun-if-changed=tests/typing/");
    for entry in glob::glob("tests/typing/*.test").unwrap() {
        let path = entry.unwrap();
        println!("cargo:rerun-if-changed={}", path.display());

        let name: String = path
            .display()
            .to_string()
            .chars()
            .map(|c| match c {
                'a'...'z' | 'A'...'Z' | '0'...'9' => c,
                _ => '_',
            })
            .collect();

        typing_tests.push_str(&format!(
            r#"
                #[test]
                fn {name}() {{
                    assert_typing(Path::new("{path}"), Path::new("{path}.out"));
                }}
            "#,
            name = name,
            path = path.display(),
        ));
    }
    fs::write(
        Path::new(&env::var("OUT_DIR").unwrap()).join("typing_tests.rs"),
        typing_tests.as_bytes(),
    )
    .unwrap();
}

fn build_lalrpop() {
    println!("cargo:rerun-if-changed=src/frontend/grammar.lalrpop");
    lalrpop::Configuration::new()
        .use_cargo_dir_conventions()
        .process()
        .unwrap();
}
