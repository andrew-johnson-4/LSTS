use lsts::tlc::TLC;

#[test]
fn check_constant_numbers() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "0: [0]").unwrap();
   tlc.check(Some(si), "1: [1]").unwrap();
   tlc.check(Some(si), "2: [2]").unwrap();
   tlc.check(Some(si), "0: [1]").unwrap_err();
   tlc.check(Some(si), "0: [2]").unwrap_err();
   tlc.check(Some(si), "1: [0]").unwrap_err();

   /*
   tlc.check(Some(si), "-0: [0]").unwrap();
   tlc.check(Some(si), "-0: [-0]").unwrap();
   tlc.check(Some(si), "-1: [-1]").unwrap();
   tlc.check(Some(si), "-2: [-2]").unwrap();
   tlc.check(Some(si), "-2: [2]").unwrap_err();
   tlc.check(Some(si), "-(-0): [0]").unwrap();
   tlc.check(Some(si), "-(-1): [1]").unwrap();
   tlc.check(Some(si), "-(-2): [2]").unwrap();
   tlc.check(Some(si), "-(-1): [-1]").unwrap_err();

   tlc.check(Some(si), "0+0: [0]").unwrap();
   tlc.check(Some(si), "0+1: [1]").unwrap();
   tlc.check(Some(si), "1+0: [1]").unwrap();
   tlc.check(Some(si), "1+1: [2]").unwrap();
   tlc.check(Some(si), "2+1: [3]").unwrap();
   tlc.check(Some(si), "1+2: [3]").unwrap();
   tlc.check(Some(si), "2+2: [4]").unwrap();
   tlc.check(Some(si), "1+1: [3]").unwrap_err();
   tlc.check(Some(si), "2+2: [5]").unwrap_err();

   tlc.check(Some(si), "0-0: [0]").unwrap();
   tlc.check(Some(si), "0-1: [-1]").unwrap();
   tlc.check(Some(si), "1-0: [1]").unwrap();
   tlc.check(Some(si), "1-1: [0]").unwrap();
   tlc.check(Some(si), "2-1: [1]").unwrap();
   tlc.check(Some(si), "1-2: [-1]").unwrap();
   tlc.check(Some(si), "2-2: [0]").unwrap();
   tlc.check(Some(si), "5-2: [0]").unwrap_err();
   tlc.check(Some(si), "2-5: [1]").unwrap_err();

   tlc.check(Some(si), "0*0: [0]").unwrap();
   tlc.check(Some(si), "0*1: [0]").unwrap();
   tlc.check(Some(si), "1*0: [0]").unwrap();
   tlc.check(Some(si), "1*1: [1]").unwrap();
   tlc.check(Some(si), "2*1: [2]").unwrap();
   tlc.check(Some(si), "1*2: [2]").unwrap();
   tlc.check(Some(si), "2*2: [4]").unwrap();
   tlc.check(Some(si), "2*2: [5]").unwrap_err();
   tlc.check(Some(si), "1*3: [4]").unwrap_err();

   tlc.check(Some(si), "0/0: [NaN]").unwrap();
   tlc.check(Some(si), "0/1: [0]").unwrap();
   tlc.check(Some(si), "1/0: [NaN]").unwrap();
   tlc.check(Some(si), "1/1: [1]").unwrap();
   tlc.check(Some(si), "2/1: [2]").unwrap();
   tlc.check(Some(si), "1/2: [0]").unwrap();
   tlc.check(Some(si), "2/2: [1]").unwrap();
   tlc.check(Some(si), "3/2: [1]").unwrap();
   tlc.check(Some(si), "4/2: [2]").unwrap();
   tlc.check(Some(si), "5/2: [2]").unwrap();
   tlc.check(Some(si), "5/0: [2]").unwrap_err();
   tlc.check(Some(si), "5/2: [3]").unwrap_err();

   tlc.check(Some(si), "0%0: [NaN]").unwrap();
   tlc.check(Some(si), "0%1: [0]").unwrap();
   tlc.check(Some(si), "1%0: [NaN]").unwrap();
   tlc.check(Some(si), "1%1: [0]").unwrap();
   tlc.check(Some(si), "2%1: [0]").unwrap();
   tlc.check(Some(si), "1%2: [1]").unwrap();
   tlc.check(Some(si), "2%2: [0]").unwrap();
   tlc.check(Some(si), "3%2: [1]").unwrap();
   tlc.check(Some(si), "4%2: [0]").unwrap();
   tlc.check(Some(si), "5%2: [1]").unwrap();
   tlc.check(Some(si), "5%2: [7]").unwrap_err();
   tlc.check(Some(si), "0%1: [NaN]").unwrap_err();
   tlc.check(Some(si), "0%0: [0]").unwrap_err();
   */
}
