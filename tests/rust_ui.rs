
#[test]
fn rustly_testbed() {
   for p in std::fs::read_dir("../rust/src/test/ui").unwrap() {
      if let Ok(p) = p {
         let u = p.path().into_os_string().into_string().unwrap();
         if u.ends_with(".rs") {
            let mut perr = p.path();
            perr.set_extension("stderr");
            let fails = std::path::Path::new(&perr).exists();
            println!("test rust ui: {} fails={}", u, fails);
            if fails {
               assert!(lsts::rust::typecheck_file(&u).is_err());
            } else {
               assert!(lsts::rust::typecheck_file(&u).is_ok());
            }
         }
      }
   }
}
