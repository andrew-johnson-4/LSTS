use std::fs::File;
use std::io::prelude::*;
use crate::{Judgements,Type};

pub struct RustError;

pub fn judge_typeof(tt: &syn::Type) -> Type {
   Type::Ground("")
}
pub fn judge_item(j: &mut Judgements, syntax: &syn::Item) {
   match syntax {
      syn::Item::Fn(f) => {
         //main: () -> ()
         let fname = f.sig.ident.to_string();
         let fargs = Type::Param("()", f.sig.inputs.iter().map(
            |tt| match tt {
               syn::FnArg::Receiver(_) => panic!("Self type not implemented for fnargs"),
               syn::FnArg::Typed(pt) => panic!("Pattern type not implemented for fnargs"),
            }
         ).collect::<Vec<Box<Type>>>());
         let rtype = match &f.sig.output {
            syn::ReturnType::Default => Type::Param("()",vec![]),
            syn::ReturnType::Type(_,box tt) => judge_typeof(&tt),
         };
         println!("{}: () -> {}", fname, rtype);
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
