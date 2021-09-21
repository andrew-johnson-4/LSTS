use std::fs::File;
use std::io::prelude::*;
use crate::Judgements;

pub struct RustError;

pub fn judge_item(j: &mut Judgements, syntax: &syn::Item) {
   match syntax {
      syn::Item::Fn(f) => {
         //main: () -> ()
         let fname = f.sig.ident.to_string();
         println!("judge fn: {:?}", f.sig);
      },
      _ => {}
   }
}
pub fn judge_file(j: &mut Judgements, syntax: &syn::File) {
   for i in syntax.items.iter() {
      judge_item(j, i);
   }
}

pub fn typecheck_file(path: &str) -> Result<(),RustError>
{
   let mut file = File::open(path).expect(&format!("Unable to open file: {}", path));
   let mut src = String::new();
   file.read_to_string(&mut src).expect(&format!("Unable to read file: {}", path));

   let syntax = syn::parse_file(&src).expect(&format!("Unable to parse file: {}", path));

   let mut j = Judgements::zero();
   judge_file(&mut j, &syntax);

   //TODO: load Rust prelude and typecheck file
   Ok(())
}
