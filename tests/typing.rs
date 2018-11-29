use std::path::Path;

fn assert_typing(input: &Path, expected: &Path) {
    unimplemented!()
}

include!(concat!(env!("OUT_DIR"), "/typing_tests.rs"));
