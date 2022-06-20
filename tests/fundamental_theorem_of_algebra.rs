use lsts::tlc::TLC;

#[test]
fn check_fundamental_theorem() {
   let mut tlc = TLC::new();
   let sa = tlc.import_str(None, r#"
"#).unwrap();

   //prove that f(z) has n complex roots
   tlc.check(Some(sa), r#"
let f(z: Complex): Complex = z^n + a[n−1]*z^(n-1) + ... + a[0]
    where: n:Integer    | n >= 1
    where: a:Complex[n] | a[0] != 0
    prove: roots(f).size() == n;
"#).unwrap();
}
