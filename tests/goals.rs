#![feature(box_patterns)]

#[test]
fn test_arith1(){
   //prove that 2 + 2 = 4
   let ts = lsts::declare([
      lsts::typedef("Int", lsts::or([
         lsts::ground("Zero"),
         lsts::param("Succ",[lsts::ground("Int")]),
      ])),
      lsts::typefun("Int::add", {fn add(args:Vec<Box<lsts::Type>>) -> Box<lsts::Type> {
         //type functions are weakly typed themselves
         match args.as_slice() {
            [box lsts::Type::Ground("Zero"),r] => r.clone(),
            [box lsts::Type::Param("Succ",dls),r] => match dls.as_slice() {
               [dl] => lsts::param("Succ",[add(vec![dl.clone(),r.clone()])]),
               _ => lsts::tfalse()
            }
            _ => lsts::tfalse()
         }
      } add}),
      lsts::eq(
         lsts::param("Succ",[
            lsts::param("Succ",[
               lsts::param("Succ",[
                  lsts::param("Succ",[
                     lsts::ground("Zero")
                  ])
               ])
            ])
         ]),
         lsts::typecall("Int::add", [
            lsts::param("Succ",[
               lsts::param("Succ",[
                  lsts::ground("Zero")
               ])
	    ]),
            lsts::param("Succ",[
               lsts::param("Succ",[
                  lsts::ground("Zero")
               ])
	    ])
         ])
      )
   ]).normalize();

   assert_eq!(
      ts.lines[2].to_string(),
      "T".to_string()
   );
}

