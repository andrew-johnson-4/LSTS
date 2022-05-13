use std::fs::File;
use std::io::prelude::*;

/*
#[test]
fn rustly_testbed() {
   for p in std::fs::read_dir("../rust/src/test/ui").unwrap() {
      if let Ok(p) = p {
         let path = p.path();
         if !path.is_file() { continue; }
         let mut file = File::open(path.clone()).expect(&format!("Unable to open file: {:?}", path.clone()));
         let mut src = String::new();
         file.read_to_string(&mut src).expect(&format!("Unable to read file: {:?}", path.clone()));
         
         let u = p.path().into_os_string().into_string().unwrap();
         if u.ends_with(".rs") {
            let mut perr = p.path();
            perr.set_extension("stderr");
            let fails = src.contains("//~") || std::path::Path::new(&perr).exists();
            println!("test rust ui: {} fails={} {}", u, fails, src.contains("//~"));
            if fails {
               assert!(lsts::rust::typecheck_file(&u).is_err());
            } else {
               assert!(lsts::rust::typecheck_file(&u).is_ok());
            }
         }
      }
   }
}
*/
