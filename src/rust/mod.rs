use std::fs::File;
use std::io::prelude::*;

pub struct RustError;

pub fn typecheck_file(path: &str) -> Result<(),RustError>
{
   let mut file = File::open(path).expect(&format!("Unable to open file: {}", path));
   let mut src = String::new();
   file.read_to_string(&mut src).expect(&format!("Unable to read file: {}", path));

   let _syntax = syn::parse_file(&src).expect(&format!("Unable to parse file: {}", path));

   //TODO: load Rust prelude and typecheck file
   Ok(())
}
