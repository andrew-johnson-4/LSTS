use std::collections::{HashMap};
use std::io::Read;
use crate::term::{Term,TermId};
use crate::debug::{Error};
use crate::token::{Symbol,TokenReader,span_of};
use crate::scope::{ScopeId,Scope};
use crate::tlc::{TLC,TypeRule,Invariant,Typedef,Inference};
use crate::typ::{Type};
use crate::kind::{Kind};

fn peek_is_regex<R: Read>(tokens: &mut TokenReader<R>) -> bool {
   if let Ok(Some(t)) = tokens.peek() {
      if let Symbol::Regex(_) = t.symbol {
         true
      } else { false }
   } else { false }
}
fn peek_is_typename<R: Read>(tokens: &mut TokenReader<R>) -> bool {
   if let Ok(Some(t)) = tokens.peek() {
      if let Symbol::Typename(_) = t.symbol {
         true
      } else { false }
   } else { false }
}
fn peek_is<R: Read>(tokens: &mut TokenReader<R>, is: &Vec<Symbol>) -> bool {
   if let Ok(Some(t)) = tokens.peek() {
      is.contains(&t.symbol)
   } else { false }
}
fn pop_is<R: Read>(rule: &str, tokens: &mut TokenReader<R>, is: &Vec<Symbol>) -> Result<Symbol,Error> {
   match tokens.take()? {
      Some(t) => {
         if !is.contains(&t.symbol) {
            Err(Error {
               kind: "Parse Error".to_string(),
               rule: format!("unexpected Symbol {:?} in rule {}, expected one of {}", &t.symbol, rule,
               is.iter().map(|s|format!("{:?}",s)).collect::<Vec<String>>().join(" or ") ),
               span: span_of(tokens),
            })
         } else { Ok(t.symbol.clone()) }
      },
      None => {
         Err(Error {
            kind: "Parse Error".to_string(),
            rule: format!("unexpected End-Of-File in rule {}", rule),
            span: span_of(tokens),
         })
      },
   }
}

pub fn ll1_kind<R: Read>(tlc: &mut TLC, tokens: &mut TokenReader<R>) -> Result<Kind,Error> {
   let mut kinds = Vec::new();
   while peek_is_typename(tokens) {
      let mut kname = "Nil".to_string();
      let mut ks = Vec::new();
      if let Some(Symbol::Typename(kn)) = tokens.peek_symbol()? {
         tokens.take_symbol()?;
         kname = kn.clone();
      }

      if peek_is(tokens, &vec![Symbol::LessThan]) {
         pop_is("kind", tokens, &vec![Symbol::LessThan])?;
         while !peek_is(tokens, &vec![Symbol::GreaterThan]) {
            if peek_is(tokens, &vec![Symbol::Comma]) {
               pop_is("kind", tokens, &vec![Symbol::Comma])?;
            }
            ks.push(ll1_kind(tlc, tokens)?);
         }
         pop_is("kind", tokens, &vec![Symbol::GreaterThan])?;
      }

      if peek_is(tokens, &vec![Symbol::Plus]) {
         pop_is("kind", tokens, &vec![Symbol::Plus])?;
      }
      if &kname=="Nil" {
         kinds.push(Kind::Nil);
      } else {
         kinds.push(Kind::Simple(kname,ks));
      }
   }
   if kinds.len()==1 {
      Ok(kinds[0].clone())
   } else { 
      Ok(Kind::And(kinds))
   }
}

pub fn ll1_type_stmt<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut t = "".to_string();
   let mut normal = false;
   let mut implies = None;
   let mut tiks: Vec<(String,Option<Type>,Kind)> = Vec::new();
   let mut typedef = Vec::new();
   let mut kinds = tlc.term_kind.clone();
   let mut props: Vec<Invariant> = Vec::new();
   let mut constructors: Vec<String> = Vec::new();
   let mut dept: HashMap<String,TermId> = HashMap::new();

   pop_is("type-stmt", tokens, &vec![Symbol::Type])?;
   if peek_is(tokens, &vec![Symbol::Normal]) {
      pop_is("type-stmt", tokens, &vec![Symbol::Normal])?;
      normal = true;
   }
   if let Some(Symbol::Typename(tname)) = tokens.peek_symbol()? {
      tokens.take_symbol()?;
      t = tname.clone();
   } else {
      pop_is("type-stmt", tokens, &vec![Symbol::Typename("T".to_string())])?;
   }

   if peek_is(tokens, &vec![Symbol::LessThan]) {
      pop_is("type-stmt", tokens, &vec![Symbol::LessThan])?;

      while !peek_is(tokens, &vec![Symbol::GreaterThan]) {
         if peek_is(tokens, &vec![Symbol::Comma]) {
            pop_is("type-stmt", tokens, &vec![Symbol::Comma])?;
         }

         let mut typ = "".to_string();
         let mut inf = None;
         let mut kind = tlc.term_kind.clone();

         if let Some(Symbol::Typename(tn)) = tokens.peek_symbol()? {
            tokens.take_symbol()?;
            typ = tn.clone();
         }
         if peek_is(tokens, &vec![Symbol::Ascript]) {
            pop_is("type-stmt", tokens, &vec![Symbol::Ascript])?;
            inf = Some( ll1_type(tlc, &mut dept, scope, tokens)? );
         }
         if peek_is(tokens, &vec![Symbol::KAscript]) {
            pop_is("type-stmt", tokens, &vec![Symbol::KAscript])?;
            kind = ll1_kind(tlc, tokens)?;
         }
         tiks.push((typ,inf,kind));
      }

      pop_is("type-stmt", tokens, &vec![Symbol::GreaterThan])?;
   }
   let struct_typ = Type::Ident(t.clone(), tiks.iter().map(|(t,_i,_k)|Type::Ident(t.clone(),Vec::new())).collect::<Vec<Type>>());

   if peek_is(tokens, &vec![Symbol::Ascript]) {
      pop_is("type-stmt", tokens, &vec![Symbol::Ascript])?;
      implies = Some( ll1_type(tlc, &mut dept, scope, tokens)? );
   }

   if peek_is(tokens, &vec![Symbol::Is]) {
      pop_is("type-stmt", tokens, &vec![Symbol::Is])?;

      while peek_is_typename(tokens) || peek_is_regex(tokens) || peek_is(tokens, &vec![Symbol::LeftBrace]) {
         let mut tcname = t.clone();
         let mut tcrows = Vec::new();
         if let Some(Symbol::Regex(r)) = tokens.peek_symbol()? {
            tokens.take_symbol()?;
            typedef.push( Typedef::Regex(r.clone()) );
            continue;
         };
         if let Some(Symbol::Typename(tn)) = tokens.peek_symbol()? {
            tokens.take_symbol()?;
            tcname = tn.clone();
         };
         if peek_is(tokens, &vec![Symbol::LeftBrace]) {
            pop_is("type-stmt", tokens, &vec![Symbol::LeftBrace])?;

            while !peek_is(tokens, &vec![Symbol::RightBrace]) {
               if peek_is(tokens, &vec![Symbol::Comma]) {
                  pop_is("type-stmt", tokens, &vec![Symbol::Comma])?;
               }
               let ki = if let Some(Symbol::Ident(f)) = tokens.peek_symbol()? {
                  tokens.take_symbol()?;
                  f.clone()
               } else {
                  pop_is("type-stmt", tokens, &vec![Symbol::Ident("x".to_string())])?;
                  unreachable!("type-stmt")
               };
               pop_is("type-stmt", tokens, &vec![Symbol::Ascript])?;
               let kt = ll1_type(tlc, &mut dept, scope, tokens)?;
               let vn = format!(".{}", ki.clone());
               let vt = tlc.push_term(Term::Ident(vn.clone()),&span);
               tlc.untyped(vt);
               tlc.scopes[scope.id].children.push((
                  vn,
                  HashMap::new(),
                  Type::Arrow(Box::new(struct_typ.clone()),Box::new(kt.clone())),
                  vt
               ));
               tcrows.push((ki,kt));
            }
            pop_is("type-stmt", tokens, &vec![Symbol::RightBrace])?;
         }
         constructors.push(tcname.clone());
         typedef.push( Typedef::Constructor(tcname,tcrows) );

         if peek_is(tokens, &vec![Symbol::Bar]) {
            pop_is("type-stmt", tokens, &vec![Symbol::Bar])?;
         };
      }
   }

   if peek_is(tokens, &vec![Symbol::KAscript]) {
      pop_is("type-stmt", tokens, &vec![Symbol::KAscript])?;
      kinds = ll1_kind(tlc, tokens)?;
   }

   if peek_is(tokens, &vec![Symbol::Where]) {
      while peek_is(tokens, &vec![Symbol::Where,Symbol::AndAlso]) {
         pop_is("type-stmt", tokens, &vec![Symbol::Where,Symbol::AndAlso])?;
         let mut itks = Vec::new();
         let prop;
         let mut algs = None;

         while !peek_is(tokens, &vec![Symbol::Dot]) {
            if peek_is(tokens, &vec![Symbol::Comma]) {
               pop_is("type-stmt", tokens, &vec![Symbol::Comma])?;
            }
            let mut idn = None;
            let mut inf = None;
            let mut kind = tlc.term_kind.clone();
            if let Some(Symbol::Ident(n)) = tokens.peek_symbol()? {
               tokens.take_symbol()?;
               idn = Some(n.clone());
            }
            if peek_is(tokens, &vec![Symbol::Ascript]) {
               pop_is("type-stmt", tokens, &vec![Symbol::Ascript])?;
               inf = Some( ll1_type(tlc, &mut dept, scope, tokens)? );
            }
            if peek_is(tokens, &vec![Symbol::KAscript]) {
               pop_is("type-stmt", tokens, &vec![Symbol::KAscript])?;
               kind = ll1_kind(tlc, tokens)?;
            }
            itks.push((idn,inf,kind));
         }
         pop_is("type-stmt", tokens, &vec![Symbol::Dot])?;
         prop = Some( ll1_term(tlc, scope, tokens)? );
         if peek_is(tokens, &vec![Symbol::Bar]) {
            pop_is("type-stmt", tokens, &vec![Symbol::Bar])?;
            algs = Some( ll1_term(tlc, scope, tokens)? );
         }
         let algs = if let Some(a) = algs { a }
         else { tlc.push_term(Term::Ident("True".to_string()),&span) };
         props.push(Invariant {
            itks: itks,
            prop: prop.expect("TLC Grammar Error in rule [typ_invariant]"),
            algs: algs,
         });
      }
   }

   if normal {
      if constructors.len()==0 {
         //constructors are preferred normal forms
         tlc.type_is_normal.insert(Type::Ident(t.clone(),Vec::new()));
      }
      for k in kinds.flatten().iter() {
         if k == &tlc.term_kind { continue; } //Term is never normal
         tlc.kind_is_normal.insert(k.clone());
      }
   }
   tlc.typedef_index.insert(t.clone(), tlc.rules.len());
   for c in constructors.iter() {
      if normal {
         tlc.type_is_normal.insert(Type::Ident(c.clone(),Vec::new()));
      }
      if &t==c { continue; } //constructor has same name as type
      tlc.typedef_index.insert(c.clone(), tlc.rules.len());
   }
   for inv in props.iter() {
   if let Term::App(g,x) = &tlc.rows[inv.prop.id].term.clone() {
   if let Term::Ident(gn) = &tlc.rows[g.id].term.clone() {
      let mut xs = Vec::new();
      let mut fkts = HashMap::new();
      match &tlc.rows[x.id].term.clone() {
         Term::Tuple(ts) => {
            for ct in ts.iter() {
               if let Term::Ident(ctn) = &tlc.rows[ct.id].term.clone() {
               if ctn == "self" { //replace "self" in invariants with this type rule
                  xs.push(Type::Ident(t.clone(),Vec::new()));
                  fkts.insert(xs[xs.len()-1].clone(), tlc.term_kind.clone());
                  continue;
               }}
               let ctt = tlc.rows[ct.id].term.clone();
               xs.push(tlc.push_dep_type(&ctt, *ct));
               fkts.insert(xs[xs.len()-1].clone(), tlc.constant_kind.clone());
            }
         }, pt => {
            let mut is_self = false;
            if let Term::Ident(ctn) = pt {
            if ctn == "self" { //replace "self" in invariants with this type rule
               xs.push(Type::Ident(t.clone(),Vec::new()));
               fkts.insert(xs[xs.len()-1].clone(), tlc.term_kind.clone());
               is_self = true;
            }}
            if !is_self {
               xs.push(tlc.push_dep_type(&pt, *x));
               fkts.insert(xs[xs.len()-1].clone(), tlc.constant_kind.clone());
            }
         }
      }
      let rt = tlc.push_dep_type(&tlc.rows[inv.algs.id].term.clone(), inv.algs);

      let pt = if xs.len()==1 {
         xs[0].clone()
      } else {
         Type::Tuple(xs)
      };
      let ft = Type::Arrow(Box::new(pt),Box::new(rt));
      let vt = tlc.push_term(Term::Ident(gn.clone()),&span);
      tlc.untyped(vt);
      tlc.scopes[scope.id].children.push((gn.clone(), fkts, ft, vt));
   }}}
   tlc.rules.push(TypeRule::Typedef(
      t,
      normal,
      tiks,
      implies,
      typedef,
      kinds,
      props,
      span.clone()
   ));
   Ok(TermId { id:0 })
}

pub fn ll1_forall_stmt<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut quants: Vec<(Option<String>,Option<Type>,Kind)> = Vec::new();
   let inference;
   let mut term = None;
   let mut kind = tlc.term_kind.clone();
   let mut dept = HashMap::new();

   pop_is("forall-stmt", tokens, &vec![Symbol::Forall])?;

   while !peek_is(tokens, &vec![Symbol::Dot]) {
      if peek_is(tokens, &vec![Symbol::Comma]) {
         pop_is("forall-stmt", tokens, &vec![Symbol::Comma])?;
      }

      let mut ident = None;
      let mut typ = None;
      let mut kind = tlc.term_kind.clone();

      if let Some(Symbol::Ident(v)) = tokens.peek_symbol()? {
         tokens.take_symbol()?;
         ident = Some(v.clone());
      }
      if peek_is(tokens, &vec![Symbol::Ascript]) {
         pop_is("forall-stmt", tokens, &vec![Symbol::Ascript])?;
         typ = Some( ll1_type(tlc, &mut dept, scope, tokens)? );
      }
      if peek_is(tokens, &vec![Symbol::KAscript]) {
         pop_is("forall-stmt", tokens, &vec![Symbol::KAscript])?;
         kind = ll1_kind(tlc, tokens)?;
      }

      if let Some(tt) = &typ {
      if tt.is_constant() {
         kind = tlc.constant_kind.clone();
      }};
      quants.push((ident, typ, kind));
   }

   pop_is("forall-stmt", tokens, &vec![Symbol::Dot])?;
   let inf1 = ll1_type(tlc, &mut dept, scope, tokens)?;
   if peek_is(tokens, &vec![Symbol::Imply]) {
      pop_is("forall-stmt", tokens, &vec![Symbol::Imply])?;
      let inf2 = ll1_type(tlc, &mut dept, scope, tokens)?;
      inference = Some(Inference::Imply(inf1,inf2));
   } else {
      inference = Some(Inference::Type(inf1));
   };

   if peek_is(tokens, &vec![Symbol::Is]) {
      pop_is("forall-stmt", tokens, &vec![Symbol::Is])?;
      term = Some( ll1_term(tlc, scope, tokens)? );
   }

   if peek_is(tokens, &vec![Symbol::KAscript]) {
      pop_is("forall-stmt", tokens, &vec![Symbol::KAscript])?;
      kind = ll1_kind(tlc, tokens)?	;
   }

   tlc.push_forall(
      quants.clone(),
      inference.expect("TLC Grammar Error in rule [forall_stmt], expected inference"),
      term,
      kind,
      span.clone(),
   );
   if let Some(t) = term {
      let mut children = Vec::new();
      for (i,t,_k) in quants.iter() {
         let vn = i.clone().unwrap_or("_".to_string());
         let vt = tlc.push_term(Term::Ident(vn.clone()), &span);
         tlc.untyped(vt);
         children.push((vn.clone(), HashMap::new(), t.clone().unwrap_or(tlc.bottom_type.clone()), vt));
      }
      let sid = tlc.push_scope(Scope {
         parent: Some(scope),
         children: children,
      }, &span);
      Ok(tlc.push_term(Term::Let(
         sid,
         "".to_string(),
         vec![quants.clone()],
         Some(t),
         Type::Any,
         tlc.term_kind.clone(),
      ),&span))
   } else {
      Ok(TermId { id:0 })
   }
}

pub fn ll1_let_stmt<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   pop_is("let-stmt", tokens, &vec![Symbol::Let])?;
   let mut dot = false;
   if peek_is(tokens, &vec![Symbol::Dot]) {
      pop_is("let-stmt", tokens, &vec![Symbol::Dot])?;
      dot = true;
   }
   let mut ident = if let Some(Symbol::Ident(id)) = tokens.peek_symbol()? {
      tokens.take_symbol()?;
      id.clone()
   } else {
      pop_is("let-stmt", tokens, &vec![Symbol::Ident("x".to_string())])?;
      unreachable!("let-stmt")
   };
   if dot { ident = format!(".{}", ident); };
   let mut pars: Vec<Vec<(Option<String>,Option<Type>,Kind)>> = Vec::new();
   let mut rt = tlc.nil_type.clone();
   let mut rk = tlc.term_kind.clone();
   let mut t: Option<TermId> = None;
   let mut dept = HashMap::new();

   while peek_is(tokens, &vec![Symbol::LeftParen]) {
      pop_is("let-stmt", tokens, &vec![Symbol::LeftParen])?;
      let mut itks = Vec::new();
      while !peek_is(tokens, &vec![Symbol::RightParen]) {
         while peek_is(tokens, &vec![Symbol::Comma]) {
            pop_is("let-stmt", tokens, &vec![Symbol::Comma])?;
         }
         let mut ident = None;
         let mut typ = None;
         let mut kind = tlc.term_kind.clone();
         if let Some(Symbol::Ident(id)) = tokens.peek_symbol()? {
            tokens.take_symbol()?;
            ident = Some(id.clone());
         }
         if peek_is(tokens, &vec![Symbol::Ascript]) {
            pop_is("let-stmt", tokens, &vec![Symbol::Ascript])?;
         }
         if !peek_is(tokens, &vec![Symbol::RightParen,Symbol::Comma,Symbol::KAscript]) {
            typ = Some(ll1_type(tlc, &mut dept, scope, tokens)?);
         }
         if peek_is(tokens, &vec![Symbol::KAscript]) {
            pop_is("let-stmt", tokens, &vec![Symbol::KAscript])?;
            kind = ll1_kind(tlc, tokens)?;
         }
         if let Some(tt) = &typ {
         if tt.is_constant() {
            kind = tlc.constant_kind.clone();
         }};
         itks.push((ident,typ,kind));
      }
      pop_is("let-stmt", tokens, &vec![Symbol::RightParen])?;
      pars.push(itks);
   }

   if peek_is(tokens, &vec![Symbol::Ascript]) {
      pop_is("let-stmt", tokens, &vec![Symbol::Ascript])?;
      rt = ll1_type(tlc, &mut dept, scope, tokens)?;
   }
   if peek_is(tokens, &vec![Symbol::KAscript]) {
      pop_is("let-stmt", tokens, &vec![Symbol::KAscript])?;
      rk = ll1_kind(tlc, tokens)?;
   }
   if peek_is(tokens, &vec![Symbol::Is]) {
      pop_is("let-stmt", tokens, &vec![Symbol::Is])?;
      t = Some(ll1_term(tlc, scope, tokens)?);
   }

   if rt.is_constant() {
      rk = tlc.constant_kind.clone();
   }
   let mut children = Vec::new();
   for itks in pars.iter() {
      for (i,t,k) in itks.iter() {
         let t = t.clone().unwrap_or(tlc.bottom_type.clone()).normalize();
         let mut ks = HashMap::new(); ks.insert(t.clone(),k.clone());
         let vn = i.clone().unwrap_or("_".to_string());
         let vt = tlc.push_term(Term::Ident(vn.clone()),&span);
         tlc.untyped(vt);
         children.push((vn.clone(), ks, t.clone(), vt));
      }
   }
   let mut ft = rt.clone();
   let mut fkts = HashMap::new();
   for itks in pars.iter().rev() {
      let mut ps = Vec::new();
      for (_i,t,k) in itks.iter() {
         let t = t.clone().unwrap_or(tlc.bottom_type.clone()).normalize();
         fkts.insert(t.clone(),k.clone());
         ps.push(t.clone());
      }
      let pt = if ps.len()==1 {
         ps[0].clone()
      } else {
         Type::Tuple(ps.clone())
      };
      ft = Type::Arrow(Box::new(pt),Box::new(ft));
   }
   tlc.reduce_type(&HashMap::new(), &mut ft, &span); //destructively reduce constants in type
   ft = ft.normalize();
   let vt = tlc.push_term(Term::Ident(ident.clone()), &span);
   tlc.untyped(vt);
   tlc.scopes[scope.id].children.push((ident.clone(), fkts, ft, vt));
   let inner_scope = tlc.push_scope(Scope {
      parent: Some(scope),
      children: children,
   }, &span);
   Ok(tlc.push_term(Term::Let(inner_scope,ident,pars,t,rt,rk), &span))
}

pub fn ll1_if_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   pop_is("if-term", tokens, &vec![Symbol::If])?;
   let cond = ll1_expr_term(tlc, scope, tokens)?;
   pop_is("if-term", tokens, &vec![Symbol::Then])?;
   let branch1 = ll1_expr_term(tlc, scope, tokens)?;
   let branch2 = if peek_is(tokens, &vec![Symbol::Else]) {
      pop_is("if-term", tokens, &vec![Symbol::Else])?;
      ll1_term(tlc, scope, tokens)?
   } else {
      tlc.push_term(Term::Tuple(Vec::new()),&span)
   };
   Ok({let t = Term::App(
      tlc.push_term(Term::Ident("if".to_string()),&span),
      tlc.push_term(Term::Tuple(vec![cond,branch1,branch2]),&span),
   ); tlc.push_term(t,&span)})
}

pub fn ll1_logical_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_compare_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::And,Symbol::Or]) {
      let op = pop_is("logical-term", tokens, &vec![Symbol::And,Symbol::Or])?;
      let op = format!("{}", op);
      let term2 = ll1_compare_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_compare_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_addsub_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Equal,Symbol::NotEqual,Symbol::GreaterThan,Symbol::GreaterThanOrEqual,Symbol::LessThan,Symbol::LessThanOrEqual]) {
      let op = pop_is("compare-term", tokens, &vec![Symbol::Equal,Symbol::NotEqual,Symbol::GreaterThan,Symbol::GreaterThanOrEqual,Symbol::LessThan,Symbol::LessThanOrEqual])?;
      let op = format!("{}", op);
      let term2 = ll1_addsub_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_addsub_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_divmul_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Plus,Symbol::Minus]) {
      let op = pop_is("addsub-term", tokens, &vec![Symbol::Plus,Symbol::Minus])?;
      let op = format!("{}", op);
      let term2 = ll1_divmul_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_divmul_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_power_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Div,Symbol::Mul,Symbol::Mod]) {
      let op = pop_is("divmul-term", tokens, &vec![Symbol::Div,Symbol::Mul,Symbol::Mod])?;
      let op = format!("{}", op);
      let term2 = ll1_power_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_power_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_prefix_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Pow]) {
      let op = pop_is("power-term", tokens, &vec![Symbol::Pow])?;
      let op = format!("{}", op);
      let term2 = ll1_prefix_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_prefix_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut ops = Vec::new();
   while peek_is(tokens, &vec![Symbol::Plus,Symbol::Minus]) {
      let op = pop_is("prefix-term", tokens, &vec![Symbol::Plus,Symbol::Minus])?;
      ops.push(op);
   }
   let mut term = ll1_atom_term(tlc, scope, tokens)?;
   while ops.len()>0 {
      let topop = if ops.pop()==Some(Symbol::Plus) { "pos".to_string() } else { "neg".to_string() };
      let t = Term::App(
         tlc.push_term(Term::Ident(topop),&span),
         term
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_tuple_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   pop_is("tuple-term", tokens, &vec![Symbol::LeftParen])?;
   let mut ts = Vec::new();
   while !peek_is(tokens, &vec![Symbol::RightParen]) {
      if peek_is(tokens, &vec![Symbol::Comma]) {
         pop_is("tuple-term", tokens, &vec![Symbol::Comma])?;
      }
      ts.push( ll1_term(tlc, scope, tokens)? );
   }
   pop_is("tuple-term", tokens, &vec![Symbol::RightParen])?;
   if ts.len()==1 {
      Ok(ts[0])
   } else {
      Ok(tlc.push_term(Term::Tuple(ts),&span))
   }
}

pub fn ll1_value_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   if let Some(sym) = tokens.peek_symbol()? {
      if let Symbol::Ident(x) = sym {
         tokens.take_symbol()?;
         return Ok(tlc.push_term(Term::Ident(x.clone()), &span))
      } else if let Symbol::Value(x) = sym {
         tokens.take_symbol()?;
         return Ok(tlc.push_term(Term::Value(x.clone()), &span))
      } else if let Symbol::Typename(cname) = sym {
         tokens.take_symbol()?;
         let mut kvs = Vec::new();
         if peek_is(tokens, &vec![Symbol::LeftBrace]) {
            pop_is("value-term", tokens, &vec![Symbol::LeftBrace])?;
            while !peek_is(tokens, &vec![Symbol::RightBrace]) {
               if peek_is(tokens, &vec![Symbol::Comma]) {
                  pop_is("value-term", tokens, &vec![Symbol::Comma])?;
               }
               if let Some(Symbol::Ident(k)) = tokens.peek_symbol()? {
                  tokens.take_symbol()?;
                  pop_is("value-term", tokens, &vec![Symbol::Is])?;
                  let v = ll1_term(tlc, scope, tokens)?;
                  kvs.push((k.clone(),v));
                  continue;
               }
               pop_is("value-term", tokens, &vec![Symbol::Ident("x".to_string())])?;
            }
            pop_is("value-term", tokens, &vec![Symbol::RightBrace])?;
         }
         return Ok(tlc.push_term(Term::Constructor(
            cname.clone(),
            kvs
         ),&span));
      }
   }
   pop_is("value-term",tokens,&vec![
      Symbol::Ident("x".to_string()),
      Symbol::Typename("A".to_string()),
      Symbol::Value("1".to_string()),
   ])?;
   unreachable!("value-term expected Ident, Typename, or Value")
}

pub fn ll1_field_term<R: Read>(tlc: &mut TLC, _scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   pop_is("field-term", tokens, &vec![Symbol::Dot])?;
   if let Some(Symbol::Ident(f)) = tokens.peek_symbol()? {
      tokens.take_symbol()?;
      return Ok(tlc.push_term(Term::Ident(format!(".{}",f)),&span))
   }
   pop_is("field-term", tokens, &vec![Symbol::Ident("x".to_string())])?;
   unreachable!("field-term")
}

pub fn ll1_atom_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = if peek_is(tokens, &vec![Symbol::LeftParen]) {
      ll1_tuple_term(tlc, scope, tokens)?
   } else {
      ll1_value_term(tlc, scope, tokens)?
   };
   while peek_is(tokens, &vec![Symbol::LeftParen,Symbol::Dot]) {
      if peek_is(tokens, &vec![Symbol::Dot]) {
         let field = ll1_field_term(tlc, scope, tokens)?;
         let t = Term::App(
            field,
            term
         );
         term = tlc.push_term(t,&span);
      } else {
         let args = ll1_tuple_term(tlc, scope, tokens)?;
         let t = Term::App(
            term,
            args
         );
         term = tlc.push_term(t,&span);
      }
   }
   Ok(term)
}

pub fn ll1_expr_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   ll1_logical_term(tlc, scope, tokens)
}

pub fn ll1_algebra_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_expr_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::BackSlash]) {
      pop_is("algebra-term", tokens, &vec![Symbol::BackSlash])?;
      pop_is("algebra-term", tokens, &vec![Symbol::LeftBracket])?;
      let mut a = ll1_expr_term(tlc, scope, tokens)?;
      pop_is("algebra-term", tokens, &vec![Symbol::Bar])?;
      let mut b = ll1_expr_term(tlc, scope, tokens)?;
      pop_is("algebra-term", tokens, &vec![Symbol::RightBracket])?;
      tlc.untyped(a); tlc.unify_varnames(&mut HashMap::new(),&mut a);
      tlc.untyped(b); tlc.unify_varnames(&mut HashMap::new(),&mut b);
      term = {let t = Term::Substitution(
         term,
         a,
         b
      ); tlc.push_term(t,&span)};
   }
   Ok(term)
}

pub fn ll1_paren_type<R: Read>(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<Type,Error> {
   pop_is("paren-type", tokens, &vec![Symbol::LeftParen])?;
   let mut ts = Vec::new();
   while !peek_is(tokens, &vec![Symbol::RightParen]) {
      if peek_is(tokens, &vec![Symbol::Comma]) {
         pop_is("paren-type", tokens, &vec![Symbol::Comma])?;
      }
      ts.push( ll1_type(tlc, dept, scope, tokens)? );
   }
   pop_is("paren-type", tokens, &vec![Symbol::RightParen])?;
   if ts.len()==1 {
      Ok(ts[0].clone())
   } else {
      Ok(Type::Tuple(ts))
   }
}

pub fn ll1_typeof_type<R: Read>(tlc: &mut TLC, _dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<Type,Error> {
   let span = span_of(tokens);
   pop_is("atom-type", tokens, &vec![Symbol::Typeof])?;
   pop_is("atom-type", tokens, &vec![Symbol::LeftParen])?;
   let vt = if let Some(Symbol::Ident(v)) = tokens.peek_symbol()? {
      tokens.take_symbol()?;
      tlc.typeof_var(&Some(scope), &v, &None, &span)?
   } else {
      pop_is("atom-type", tokens, &vec![Symbol::Ident("v".to_string())])?;
      unreachable!("atom-type")
   };
   pop_is("atom-type", tokens, &vec![Symbol::RightParen])?;
   Ok(vt)
}

pub fn ll1_ident_type<R: Read>(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<Type,Error> {
   let tn = if let Some(Symbol::Typename(tn)) = tokens.peek_symbol()? {
      tokens.take_symbol()?;
      tn.clone()
   } else {
      pop_is("ident-type", tokens, &vec![Symbol::Typename("T".to_string())])?;
      unreachable!("ident-type")
   };
   let mut tps = Vec::new();
   if peek_is(tokens, &vec![Symbol::LessThan]) {
      pop_is("ident-type", tokens, &vec![Symbol::LessThan])?;
      while !peek_is(tokens, &vec![Symbol::GreaterThan]) {
         if peek_is(tokens, &vec![Symbol::Comma]) {
            pop_is("ident-type", tokens, &vec![Symbol::Comma])?;
         }
         tps.push( ll1_type(tlc, dept, scope, tokens)? );
      }
      pop_is("ident-type", tokens, &vec![Symbol::GreaterThan])?;
   }
   Ok(Type::Ident(tn,tps))
}

pub fn ll1_dep_type<R: Read>(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<Type,Error> {
   pop_is("dependent-type", tokens, &vec![Symbol::LeftBracket])?;
   let mut t = ll1_term(tlc, scope, tokens)?;
   pop_is("dependent-type", tokens, &vec![Symbol::RightBracket])?;
   tlc.untyped(t);
   tlc.unify_varnames(dept,&mut t);
   let ct = tlc.push_dep_type(&tlc.rows[t.id].term.clone(), t);
   Ok(ct)
}

pub fn ll1_atom_type<R: Read>(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<Type,Error> {
   if peek_is(tokens, &vec![Symbol::Question]) {
      pop_is("atom-type", tokens, &vec![Symbol::Question])?;
      Ok(Type::Any)
   } else if peek_is(tokens, &vec![Symbol::LeftParen]) {
      ll1_paren_type(tlc, dept, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::LeftBracket]) {
      ll1_dep_type(tlc, dept, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::Typeof]) {
      ll1_typeof_type(tlc, dept, scope, tokens)
   } else {
      ll1_ident_type(tlc, dept, scope, tokens)
   }
}

pub fn ll1_suffix_type<R: Read>(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<Type,Error> {
   let span = span_of(tokens);
   let mut base = ll1_atom_type(tlc, dept, scope, tokens)?;
   let mut ts = Vec::new();
   while peek_is(tokens, &vec![Symbol::LeftBracket]) {
      pop_is("suffix-type", tokens, &vec![Symbol::LeftBracket])?;
      if !peek_is(tokens, &vec![Symbol::RightBracket]) {
         ts.push(Some( ll1_term(tlc, scope, tokens)? ));
      } else {
         ts.push(None);
      };
      pop_is("suffix-type", tokens, &vec![Symbol::RightBracket])?;
   }
   for bracketed in ts.iter().rev() {
      let mut dt = if let Some(br) = bracketed {
         *br
      } else {
         tlc.push_term(Term::Ident("length".to_string()), &span)
      };
      tlc.untyped(dt);
      tlc.unify_varnames(dept,&mut dt);
      let ct = tlc.push_dep_type(&tlc.rows[dt.id].term.clone(), dt);
      base = Type::Ident("Tensor".to_string(), vec![
         base,
         ct
      ]);
   }
   Ok(base)
}

pub fn ll1_product_type<R: Read>(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<Type,Error> {
   let mut types = vec![ ll1_suffix_type(tlc, dept, scope, tokens)? ];
   while peek_is(tokens, &vec![Symbol::Mul]) {
      pop_is("product-type", tokens, &vec![Symbol::Mul])?;
      types.push( ll1_suffix_type(tlc, dept, scope, tokens)? );
   }
   if types.len()==1 {
      Ok(types[0].clone())
   } else {
      Ok(Type::Product(types))
   }
}

pub fn ll1_ratio_type<R: Read>(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<Type,Error> {
   let mut typ = ll1_product_type(tlc, dept, scope, tokens)?;
   if peek_is(tokens, &vec![Symbol::Div]) {
      pop_is("ratio-type", tokens, &vec![Symbol::Div])?;
      let typ2 = ll1_product_type(tlc, dept, scope, tokens)?;
      typ = Type::Ratio(Box::new(typ), Box::new(typ2));
   }
   Ok(typ)
}

pub fn ll1_arrow_type<R: Read>(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<Type,Error> {
   let mut typ = ll1_ratio_type(tlc, dept, scope, tokens)?;
   if peek_is(tokens, &vec![Symbol::Arrow]) {
      pop_is("and-type", tokens, &vec![Symbol::Arrow])?;
      let typ2 = ll1_arrow_type(tlc, dept, scope, tokens)?;
      typ = Type::Arrow(Box::new(typ), Box::new(typ2));
   }
   Ok(typ)
}

pub fn ll1_and_type<R: Read>(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<Type,Error> {
   let mut types = vec![ ll1_arrow_type(tlc, dept, scope, tokens)? ];
   while peek_is(tokens, &vec![Symbol::Plus]) {
      pop_is("and-type", tokens, &vec![Symbol::Plus])?;
      types.push( ll1_arrow_type(tlc, dept, scope, tokens)? );
   }
   if types.len()==1 {
      Ok(types[0].clone())
   } else {
      Ok(Type::And(types))
   }
}

pub fn ll1_type<R: Read>(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<Type,Error> {
   ll1_and_type(tlc, dept, scope, tokens)
}

pub fn ll1_ascript_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_algebra_term(tlc, scope, tokens)?;
   if peek_is(tokens, &vec![Symbol::Ascript]) {
      pop_is("ascript-term", tokens, &vec![Symbol::Ascript])?;
      let at = ll1_type(tlc, &mut HashMap::new(), scope, tokens)?;
      let t = Term::Ascript(
         term,
         at
      );
      term = tlc.push_term(t, &span);
   }
   Ok(term)
}

pub fn ll1_as_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_ascript_term(tlc, scope, tokens)?;
   if peek_is(tokens, &vec![Symbol::As]) {
      pop_is("as-term", tokens, &vec![Symbol::As])?;
      let at = ll1_type(tlc, &mut HashMap::new(), scope, tokens)?;
      let t = Term::As(
         term,
         at
      );
      term = tlc.push_term(t, &span);
   }
   Ok(term)
}

pub fn ll1_term<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   if peek_is(tokens, &vec![Symbol::If]) {
      ll1_if_term(tlc, scope, tokens)
   } else {
      ll1_as_term(tlc, scope, tokens)
   }
}

pub fn ll1_stmt<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   if peek_is(tokens, &vec![Symbol::LeftBrace]) {
      ll1_block_stmt(tlc, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::Type]) {
      ll1_type_stmt(tlc, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::Forall]) {
      ll1_forall_stmt(tlc, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::Let]) {
      ll1_let_stmt(tlc, scope, tokens)
   } else {
      ll1_term(tlc, scope, tokens)
   }
}

pub fn ll1_block_stmt<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let scope = tlc.push_scope(Scope {
      parent: Some(scope),
      children: Vec::new(),
   }, &span_of(tokens));

   pop_is("block", tokens, &vec![Symbol::LeftBrace])?;
   let mut es = Vec::new();
   while !peek_is(tokens, &vec![Symbol::RightBrace]) {
      es.push( ll1_stmt(tlc, scope, tokens)? );
      while peek_is(tokens, &vec![Symbol::SemiColon]) {
         pop_is("block", tokens, &vec![Symbol::SemiColon])?;
      }
   }
   pop_is("block", tokens, &vec![Symbol::RightBrace])?;

   Ok(tlc.push_term(Term::Block(scope,es), &span_of(tokens)))
}

pub fn ll1_file<R: Read>(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader<R>) -> Result<TermId,Error> {
   let mut es = Vec::new();
   while !peek_is(tokens, &vec![Symbol::EOF]) {
      es.push( ll1_stmt(tlc, scope, tokens)? );
      while peek_is(tokens, &vec![Symbol::SemiColon]) {
         pop_is("file", tokens, &vec![Symbol::SemiColon])?;
      }
   }
   pop_is("file", tokens, &vec![Symbol::EOF])?;
   Ok(tlc.push_term(Term::Block(scope,es), &span_of(tokens)))
}

