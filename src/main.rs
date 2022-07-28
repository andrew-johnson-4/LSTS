use std::env;
use lsts::tlc::TLC;

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
      let mut env = None;
      for fp in args.iter() {
         println!("Running: {}", fp);
         env = Some(tlc.import_file(env, fp).unwrap());
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
         env = Some(tlc.check_file(env, fp).unwrap());
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
