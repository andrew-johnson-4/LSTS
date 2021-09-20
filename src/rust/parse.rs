use std::fs::File;
use std::io::prelude::*;

fn main()
{
   for (pi,path) in std::env::args().enumerate() {
      if pi==0 { continue; }
      println!("parse {}", &path);
      let mut file = File::open(path.clone()).expect(&format!("Unable to open file: {}", &path));
      let mut src = String::new();
      file.read_to_string(&mut src).expect(&format!("Unable to read file: {}", &path));

      let syntax = syn::parse_file(&src).expect(&format!("Unable to parse file: {}", &path));

      println!("{:#?}", syntax);
   }
}
