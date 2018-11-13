use std::path::PathBuf;
use std::process;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Options {
    /// The input source file.
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

fn main() {
    let args = ::std::env::args().collect::<Vec<_>>();
    println!("args = {:?}", args);
    if let Err(e) = try_main() {
        eprintln!("Error: {}", e);
        for c in e.iter_chain() {
            eprintln!("    caused by: {}", c);
        }
        process::exit(1);
    }
}

fn try_main() -> Result<(), failure::Error> {
    let opts = Options::from_args();
    cool_rs::compile_file(&opts.input)
}
