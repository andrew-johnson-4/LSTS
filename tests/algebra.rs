use lsts::tlc::TLC;

#[test]
fn check_alpha_conversion() {
   let mut tlc = TLC::new();

   tlc.check(None, "let a:[self+2]; a\\[self|3]").unwrap();
   tlc.check(None, "let a:[self+2]; a\\[self|3]:[5]").unwrap();
   tlc.check(None, "let a:[self+2]; a\\[self|3]:[6]").unwrap_err();
   tlc.check(None, "let a:[self%2==0]; a\\[self%2|0]:[True]").unwrap();
   tlc.check(None, "let a:[self%2==0]; a\\[self%2|0]:[False]").unwrap_err();
   tlc.check(None, "let a:[self%2==0]; a\\[self%2|1]:[True]").unwrap_err();
   tlc.check(None, "let a:[self%2==0]; a\\[self%2|1]:[False]").unwrap();
}
