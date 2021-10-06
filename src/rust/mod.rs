use std::fs::File;
use std::io::prelude::*;
use crate::{Judgements,Type};

pub struct RustError;

pub fn judge_typeof_lit(l: &syn::ExprLit) -> Type {
   match l.lit {
      syn::Lit::Str(_) => Type::Ground("str"),
      syn::Lit::Char(_) => Type::Ground("char"),
      syn::Lit::Int(_) => Type::Ground("int"),
      syn::Lit::Float(_) => Type::Ground("float"),
      syn::Lit::Bool(_) => Type::Ground("bool"),
      _ => panic!("unrecognized literal type: {:?}", l.lit)
   }
}
pub fn judge_typeof(tt: &syn::Type) -> Type {
   Type::Ground("")
}
pub fn judge_stmt(j: &mut Judgements, syntax: &syn::Stmt) {
   match syntax {
      syn::Stmt::Local(l) => {
         match &l.pat {
            syn::Pat::Ident(i) => {
               let iref = i.by_ref.is_some();
               let imut = i.mutability.is_some();
               let ident = i.ident.to_string();
               match &l.init {
//ident local "x" = Lit(ExprLit { attrs: [], lit: Int(LitInt { token: 5 }) });
                  Some((_,box syn::Expr::Lit(l))) => {
                     println!("ident local literal {}{}{:?} = {};", if iref {"ref "} else {""}, if imut {"mut "} else {""}, ident, judge_typeof_lit(l))
                  }, Some((_,e)) => {
                     println!("ident local {}{}{:?} = {:?};", if iref {"ref "} else {""}, if imut {"mut "} else {""}, ident, e)
                  },
                  None => println!("ident local {}{}{:?};", if iref {"ref "} else {""}, if imut {"mut "} else {""}, ident)
               }
            },
            p => println!("stmt local {:?}", l)
         }
      },
      s => println!("unknown stmt {:?}", s)
   }
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
         for s in f.block.stmts.iter() {
            judge_stmt(j, s)
         }
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
