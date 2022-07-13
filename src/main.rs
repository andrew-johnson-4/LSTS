use std::env;
use lsts::tlc::TLC;

fn main() {
   let mut tlc = TLC::new();
   let mut env = None;
   for (argi,argument) in env::args().enumerate() {
      if argi==0 { continue; }
      env = Some(tlc.import_file(env, &argument).unwrap());
   }
}
