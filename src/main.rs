use std::env;
use lsts::tlc::TLC;
use lsts::token::{tokenize_file,Symbol};
use gag::Gag;

fn main() {
   let mut tlc = TLC::new();
   let mut command = "help".to_string();
   let mut args = Vec::new();
   for (argi,argument) in env::args().enumerate() {
      if argi==0 { continue; }
      if argi==1 { command = argument.clone(); continue; }
      args.push(argument.clone());
   }
   if command=="build" {
      let mut env = None;
      for fp in args.iter() {
         println!("Compiling: {}", fp);
         env = Some(tlc.import_file(env, fp).unwrap());
      }
   } else if command=="run" {
      for fp in args.iter() {
         let r = {
            let _gag_order = Gag::stdout().unwrap();
            tlc.reduce_file(None, fp)
         };
         if let Err(msg) = r {
            eprintln!("{:?}", msg);
         } else if let Ok(v) = r {
            println!("{:?}", v);
         }
      }
   } else if command=="tokenize" {
      for fp in args.iter() {
         println!("Tokenizing: {}", fp);
         let mut tks = tokenize_file(&mut tlc, fp).expect("Could not read file during tokenization");
         loop {
         match tks.take() {
            Ok(Some(tok)) if tok.symbol==Symbol::EOF => {
               println!("{:?}", tok.symbol);
               break;
            },
            Ok(Some(tok)) => {
               println!("{:?}", tok.symbol);
            },
            Ok(None) => { break; },
            Err(msg) => {
               eprintln!("{:?}", msg);
               break;
            }
         }}
      }
   } else if command=="parse" {
      for fp in args.iter() {
         println!("Parsing: {}", fp);
         tlc.parse_file(None, fp).unwrap();
      }
   } else if command=="check" {
      let mut env = None;
      for fp in args.iter() {
         println!("Typechecking: {}", fp);
         env = Some(tlc.import_file(env, fp).unwrap());
      }
   } else {
      println!("lsts help");
      println!("     parse [filenames] -- parse files but nothing more");
      println!("     check [filenames] -- parse and typecheck files");
      println!("     build [filenames] -- compile provided files as a program");
      println!("     run   [filenames] -- execute provided files as a program");
      println!("");
   }
}
