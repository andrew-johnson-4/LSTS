use std::collections::{HashMap};
use crate::term::{Term,TermId,LetTerm};
use crate::debug::{Error};
use crate::token::{Symbol,TokenReader,span_of,tokenize_file};
use crate::scope::{ScopeId,Scope};
use crate::tlc::{TLC,TypeRule,Invariant,TypedefRule,TypedefBranch};
use crate::constant::{Constant};
use crate::typ::{Type};
use crate::kind::{Kind};

fn peek_is_regex(tokens: &mut TokenReader) -> bool {
   if let Ok(Some(t)) = tokens.peek() {
      if let Symbol::Regex(_) = t.symbol {
         true
      } else { false }
   } else { false }
}
fn peek_is_typename(tokens: &mut TokenReader) -> bool {
   if let Ok(Some(t)) = tokens.peek() {
      if let Symbol::Typename(_) = t.symbol {
         true
      } else { false }
   } else { false }
}
fn peek_is(tokens: &mut TokenReader, is: &Vec<Symbol>) -> bool {
   if let Ok(Some(t)) = tokens.peek() {
      is.contains(&t.symbol)
   } else { false }
}
fn pop_is(rule: &str, tokens: &mut TokenReader, is: &Vec<Symbol>) -> Result<Symbol,Error> {
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

pub fn ll1_kind(tlc: &mut TLC, tokens: &mut TokenReader) -> Result<Kind,Error> {
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
         kinds.push(Kind::Named(kname,ks));
      }
   }
   if kinds.len()==1 {
      Ok(kinds[0].clone())
   } else { 
      Ok(Kind::And(kinds))
   }
}

pub fn ll1_type_stmt(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut t = "".to_string();
   let mut normal = false;
   let mut implies = None;
   let mut tiks: Vec<(String,Type,Kind)> = Vec::new();
   let mut typedef = Vec::new();
   let mut kinds = tlc.term_kind.clone();
   let mut props: Vec<Invariant> = Vec::new();
   let mut constructors: Vec<String> = Vec::new();

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
         let mut inf = Type::Any;
         let mut kind = tlc.term_kind.clone();

         if let Some(Symbol::Typename(tn)) = tokens.peek_symbol()? {
            tokens.take_symbol()?;
            typ = tn.clone();
         }
         if peek_is(tokens, &vec![Symbol::Ascript]) {
            pop_is("type-stmt", tokens, &vec![Symbol::Ascript])?;
            inf = ll1_type(tlc, scope, tokens)?;
         }
         if peek_is(tokens, &vec![Symbol::KAscript]) {
            pop_is("type-stmt", tokens, &vec![Symbol::KAscript])?;
            kind = ll1_kind(tlc, tokens)?;
         }
         tiks.push((typ,inf,kind));
      }

      pop_is("type-stmt", tokens, &vec![Symbol::GreaterThan])?;
   }
   let struct_typ = Type::Named(t.clone(), tiks.iter().map(|(t,_i,_k)|Type::Named(t.clone(),Vec::new())).collect::<Vec<Type>>());

   if peek_is(tokens, &vec![Symbol::Ascript]) {
      pop_is("type-stmt", tokens, &vec![Symbol::Ascript])?;
      implies = Some( ll1_type(tlc, scope, tokens)? );
   }

   if peek_is(tokens, &vec![Symbol::Is]) {
      pop_is("type-stmt", tokens, &vec![Symbol::Is])?;

      while peek_is_typename(tokens) || peek_is_regex(tokens) || peek_is(tokens, &vec![Symbol::LeftBrace]) {
         let mut tcname = t.clone();
         let mut tcrows = Vec::new();
         if let Some(Symbol::Regex(r)) = tokens.peek_symbol()? {
            tokens.take_symbol()?;
            typedef.push( TypedefBranch::Regex(r.clone()) );
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
               let kt = ll1_type(tlc, scope, tokens)?;
               let vn = format!(".{}", ki.clone());
               let vt = tlc.push_term(Term::Ident(vn.clone()),&span);
               tlc.untyped(vt);
               tlc.scopes[scope.id].children.push((
                  vn,
                  HashMap::new(),
                  Type::Arrow(Box::new(struct_typ.clone()),Box::new(kt.clone())),
                  Some(vt)
               ));
               tcrows.push((ki,kt));
            }
            pop_is("type-stmt", tokens, &vec![Symbol::RightBrace])?;
         }
         constructors.push(tcname.clone());
         typedef.push( TypedefBranch::Constructor(tcname,tcrows) );

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
               inf = Some( ll1_type(tlc, scope, tokens)? );
            }
            if peek_is(tokens, &vec![Symbol::KAscript]) {
               pop_is("type-stmt", tokens, &vec![Symbol::KAscript])?;
               kind = ll1_kind(tlc, tokens)?;
            }
            if idn.is_none() && inf.is_none() && kind == tlc.term_kind {
               break;
            }
            itks.push((idn,inf,kind));
         }
         pop_is("type-stmt", tokens, &vec![Symbol::Dot])?;
         let prop = ll1_term(tlc, scope, tokens)?;
         let algs = if peek_is(tokens, &vec![Symbol::Bar]) {
            pop_is("type-stmt", tokens, &vec![Symbol::Bar])?;
            ll1_constant(tlc, scope, tokens)?
         } else {
            Constant::parse(tlc, "True").unwrap()
         };
         props.push(Invariant {
            scope: scope,
            itks: itks,
            prop: prop,
            algs: algs,
         });
      }
   }

   if normal && &kinds == &tlc.term_kind {
      return Err(Error {
         kind: "Parse Error".to_string(),
         rule: format!("Term type {} cannot be normal", &t),
         span: span.clone(),
      })
   }
   if normal {
      if constructors.len()==0 {
         //constructors are preferred normal forms
         tlc.type_is_normal.insert(Type::Named(t.clone(),Vec::new()));
      }
      for k in kinds.flatten().iter() {
         if k == &tlc.term_kind { continue; } //Term is never normal
         tlc.kind_is_normal.insert(k.clone());
      }
   }
   tlc.typedef_index.insert(t.clone(), tlc.rules.len());
   for c in constructors.iter() {
      if normal {
         tlc.type_is_normal.insert(Type::Named(c.clone(),Vec::new()));
      }
      if &t==c { continue; } //constructor has same name as type
      tlc.typedef_index.insert(c.clone(), tlc.rules.len());
   }

   tlc.rules.push(TypeRule::Typedef(TypedefRule {
      name: t,
      is_normal: normal,
      parameters: tiks,
      implies: implies,
      definition: typedef,
      invariants: props,
      kind: kinds,
      span: span.clone(),
   }));

   Ok(TermId { id:0 })
}

pub fn ll1_forall_stmt(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut axiom = false;
   let mut name: Option<String> = None;
   let mut quants: Vec<(String,Type,Kind)> = Vec::new();
   let inference;
   let mut term = None;
   let mut kind = tlc.term_kind.clone();

   if peek_is(tokens, &vec![Symbol::Axiom]) {
      axiom = true;
   }
   pop_is("forall-stmt", tokens, &vec![Symbol::Forall, Symbol::Axiom])?;

   if peek_is(tokens, &vec![Symbol::At]) {
      pop_is("forall-stmt", tokens, &vec![Symbol::At])?;
      if let Some(Symbol::Ident(v)) = tokens.peek_symbol()? {
         tokens.take_symbol()?;
         name = Some(v.clone());
      }
   }

   while !peek_is(tokens, &vec![Symbol::Dot]) {
      if peek_is(tokens, &vec![Symbol::Comma]) {
         pop_is("forall-stmt", tokens, &vec![Symbol::Comma])?;
      }

      let mut ident = "_".to_string();
      let mut typ = Type::Any;
      let mut kind = tlc.term_kind.clone();

      if let Some(Symbol::Ident(v)) = tokens.peek_symbol()? {
         tokens.take_symbol()?;
         ident = v.clone();
      }
      if peek_is(tokens, &vec![Symbol::Ascript]) {
         pop_is("forall-stmt", tokens, &vec![Symbol::Ascript])?;
         typ = ll1_type(tlc, scope, tokens)?;
      }
      if peek_is(tokens, &vec![Symbol::KAscript]) {
         pop_is("forall-stmt", tokens, &vec![Symbol::KAscript])?;
         kind = ll1_kind(tlc, tokens)?;
      }

      if typ.is_constant() {
         kind = tlc.constant_kind.clone();
      };
      quants.push((ident, typ, kind));
   }

   pop_is("forall-stmt", tokens, &vec![Symbol::Dot])?;
   inference = ll1_type(tlc, scope, tokens)?;

   if peek_is(tokens, &vec![Symbol::Is]) {
      pop_is("forall-stmt", tokens, &vec![Symbol::Is])?;
      term = Some( ll1_term(tlc, scope, tokens)? );
   }

   if peek_is(tokens, &vec![Symbol::KAscript]) {
      pop_is("forall-stmt", tokens, &vec![Symbol::KAscript])?;
      kind = ll1_kind(tlc, tokens)?	;
   }

   tlc.push_forall(
      scope,
      axiom,
      name,
      quants.clone(),
      inference,
      term,
      kind,
      span.clone(),
   );
   Ok(TermId { id:0 })
}

pub fn ll1_let_stmt(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let is_extern = peek_is(tokens, &vec![Symbol::Extern]);
   pop_is("let-stmt", tokens, &vec![Symbol::Let,Symbol::Extern])?;
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
   let mut pars: Vec<Vec<(String,Type,Kind)>> = Vec::new();
   let mut rt = tlc.nil_type.clone();
   let mut rk = tlc.term_kind.clone();
   let mut t: Option<TermId> = None;

   while peek_is(tokens, &vec![Symbol::LeftParen]) {
      pop_is("let-stmt", tokens, &vec![Symbol::LeftParen])?;
      let mut itks = Vec::new();
      while !peek_is(tokens, &vec![Symbol::RightParen]) {
         while peek_is(tokens, &vec![Symbol::Comma]) {
            pop_is("let-stmt", tokens, &vec![Symbol::Comma])?;
         }
         let mut ident = "_".to_string();
         let mut typ = tlc.nil_type.clone();
         let mut kind = tlc.term_kind.clone();
         if let Some(Symbol::Ident(id)) = tokens.peek_symbol()? {
            tokens.take_symbol()?;
            ident = id.clone();
         }
         if peek_is(tokens, &vec![Symbol::Ascript]) {
            pop_is("let-stmt", tokens, &vec![Symbol::Ascript])?;
         }
         if !peek_is(tokens, &vec![Symbol::RightParen,Symbol::Comma,Symbol::KAscript]) {
            typ = ll1_type(tlc, scope, tokens)?;
         }
         if peek_is(tokens, &vec![Symbol::KAscript]) {
            pop_is("let-stmt", tokens, &vec![Symbol::KAscript])?;
            kind = ll1_kind(tlc, tokens)?;
         }
         if typ.is_constant() {
            kind = tlc.constant_kind.clone();
         };
         itks.push((ident,typ,kind));
      }
      pop_is("let-stmt", tokens, &vec![Symbol::RightParen])?;
      pars.push(itks);
   }

   if peek_is(tokens, &vec![Symbol::Ascript]) {
      pop_is("let-stmt", tokens, &vec![Symbol::Ascript])?;
      rt = ll1_type(tlc, scope, tokens)?;
   }
   if peek_is(tokens, &vec![Symbol::KAscript]) {
      pop_is("let-stmt", tokens, &vec![Symbol::KAscript])?;
      rk = ll1_kind(tlc, tokens)?;
   }
   let inner_scope = tlc.new_scope(Some(scope));
   if peek_is(tokens, &vec![Symbol::Is]) {
      pop_is("let-stmt", tokens, &vec![Symbol::Is])?;
      t = Some(ll1_term(tlc, inner_scope, tokens)?);
   }

   if rt.is_constant() {
      rk = tlc.constant_kind.clone();
   }
   for itks in pars.iter() {
      for (i,t,k) in itks.iter() {
         let t = t.normalize();
         let mut ks = HashMap::new(); ks.insert(t.clone(),k.clone());
         let vn = i.clone();
         let vt = tlc.push_term(Term::Ident(vn.clone()),&span);
         tlc.untyped(vt);
         tlc.scopes[inner_scope.id].children.push(
            (vn.clone(), ks, t.clone(), Some(vt))
         );
      }
   }
   let mut ft = rt.clone();
   let mut fkts = HashMap::new();
   for itks in pars.iter().rev() {
      let mut ps = Vec::new();
      for (_i,t,k) in itks.iter() {
         let t = t.normalize();
         fkts.insert(t.clone(),k.clone());
         ps.push(t.clone());
      }
      let pt = Type::Tuple(ps.clone());
      ft = Type::Arrow(Box::new(pt),Box::new(ft));
   }
   ft = ft.normalize();
   let vt = tlc.push_term(Term::Let(LetTerm {
      is_extern: is_extern,
      scope: inner_scope,
      name: ident.clone(),
      parameters: pars,
      body: t,
      rtype: rt,
      rkind: rk
   }), &span);
   tlc.scopes[scope.id].children.push(( ident.clone(), fkts, ft, Some(vt) ));
   if tlc.strict && t.is_none() {
      return Err(Error {
         kind: "Type Error".to_string(),
         rule: format!("in strict mode functions must have bodies"),
         span: span_of(tokens),
      })
   }
   Ok(vt)
}

pub fn ll1_if_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   pop_is("if-term", tokens, &vec![Symbol::If])?;

   if peek_is(tokens, &vec![Symbol::Let]) {
      pop_is("if-term", tokens, &vec![Symbol::Let])?;
      let lhs = ll1_expr_term(tlc, scope, tokens)?;
      pop_is("if-term", tokens, &vec![Symbol::Is])?;
      let dv = ll1_expr_term(tlc, scope, tokens)?;
      pop_is("if-term", tokens, &vec![Symbol::Then])?;
      let rhs1 = ll1_expr_term(tlc, scope, tokens)?;
      let rhs2 = if peek_is(tokens, &vec![Symbol::Else]) {
         pop_is("if-term", tokens, &vec![Symbol::Else])?;
         ll1_term(tlc, scope, tokens)?
      } else {
         tlc.push_term(Term::Tuple(Vec::new()),&span)
      };
      let else_lhs = tlc.push_term(Term::Ident("_".to_string()),&span);
      let tscope = tlc.new_scope(Some(scope));
      let fscope = tlc.new_scope(Some(scope));
      Ok(tlc.push_term(Term::Match(dv, vec![
         (tscope, lhs, rhs1),
         (fscope, else_lhs, rhs2),
      ]),&span))
   } else {
      let cond = ll1_expr_term(tlc, scope, tokens)?;
      pop_is("if-term", tokens, &vec![Symbol::Then])?;
      let branch1 = ll1_expr_term(tlc, scope, tokens)?;
      let branch2 = if peek_is(tokens, &vec![Symbol::Else]) {
         pop_is("if-term", tokens, &vec![Symbol::Else])?;
         ll1_term(tlc, scope, tokens)?
      } else {
         tlc.push_term(Term::Tuple(Vec::new()),&span)
      };
      let tlhs = tlc.push_term(Term::Constructor("True".to_string(),Vec::new()),&span);
      let flhs = tlc.push_term(Term::Constructor("False".to_string(),Vec::new()),&span);
      let tscope = tlc.new_scope(Some(scope));
      let fscope = tlc.new_scope(Some(scope));
      Ok(tlc.push_term(Term::Match(cond, vec![
         (tscope, tlhs, branch1),
         (fscope, flhs, branch2)
      ]),&span))
   }
}

pub fn ll1_while_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   pop_is("while-term", tokens, &vec![Symbol::While])?;
   pop_is("loop-term", tokens, &vec![Symbol::LeftParen])?;
   let cond = ll1_expr_term(tlc, scope, tokens)?;
   pop_is("loop-term", tokens, &vec![Symbol::RightParen])?;
   let body = ll1_block_stmt(tlc, scope, tokens)?;
   Ok({let t = Term::App(
      tlc.push_term(Term::Ident("while".to_string()),&span),
      tlc.push_term(Term::Tuple(vec![cond,body]),&span),
   ); tlc.push_term(t,&span)})
}

pub fn ll1_loop_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   pop_is("loop-term", tokens, &vec![Symbol::Loop])?;
   let body = ll1_block_stmt(tlc, scope, tokens)?;
   pop_is("loop-term", tokens, &vec![Symbol::While])?;
   pop_is("loop-term", tokens, &vec![Symbol::LeftParen])?;
   let cond = ll1_expr_term(tlc, scope, tokens)?;
   pop_is("loop-term", tokens, &vec![Symbol::RightParen])?;
   Ok({let t = Term::App(
      tlc.push_term(Term::Ident("loop".to_string()),&span),
      tlc.push_term(Term::Tuple(vec![body,cond]),&span),
   ); tlc.push_term(t,&span)})
}

pub fn ll1_for_term(tlc: &mut TLC, mut scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   enum Comb {
      CFor(ScopeId,TermId,TermId), CIf(TermId)
   }
   let mut loop_stack = Vec::new();
   while !peek_is(tokens, &vec![Symbol::Yield]) {
      if peek_is(tokens, &vec![Symbol::For]) {
         pop_is("for-term", tokens, &vec![Symbol::For])?;
         let lhs = ll1_ascript_term(tlc, scope, tokens)?;
         scope = Term::scope_of_lhs(tlc, Some(scope), lhs);
         pop_is("for-term", tokens, &vec![Symbol::In])?;
         let iterable = ll1_expr_term(tlc, scope, tokens)?;
         loop_stack.push(Comb::CFor(scope,lhs,iterable));
      } else if peek_is(tokens, &vec![Symbol::If]) {
         pop_is("for-term", tokens, &vec![Symbol::If])?;
         let cond = ll1_expr_term(tlc, scope, tokens)?;
         loop_stack.push(Comb::CIf(cond));
      }
   }

   pop_is("for-term", tokens, &vec![Symbol::Yield])?;
   let mut rhs = ll1_expr_term(tlc, scope, tokens)?;
   rhs = tlc.push_term(Term::Tuple(vec![rhs]),&span);
   
   //for = .flatmap(iterable, \lhs. rhs)
   //if  = if cond then rhs else ()
   //for a in range(10) for b in range(a,10) if a==b yield a+b
   //range(10).flatmap(\a. range(a,10).flatmap(\b. if a==b then (a+b,) else () ))
   
   for s in loop_stack.iter().rev() {
   match s {
      Comb::CFor(sc,lhs,iterable) => {
         let arr_scope = tlc.new_scope(Some(*sc));
         let arr = tlc.push_term(Term::Arrow(arr_scope, *lhs, None, rhs ),&span);
         let mut children = tlc.scopes[arr_scope.id].children.clone();
         Term::scope_of_lhs_impl(tlc, &mut children, *lhs);
         tlc.scopes[arr_scope.id].children = children;

         let t = Term::App(
            tlc.push_term(Term::Ident(".flatmap".to_string()),&span),
            tlc.push_term(Term::Tuple(vec![*iterable,arr]),&span),
         );
         rhs = tlc.push_term(t,&span);
      },
      Comb::CIf(cond) => {
         let branch1 = rhs;
         let branch2 = tlc.push_term(Term::Tuple(Vec::new()),&span);
         let tlhs = tlc.push_term(Term::Constructor("True".to_string(),Vec::new()),&span);
         let flhs = tlc.push_term(Term::Constructor("False".to_string(),Vec::new()),&span);
         let tscope = tlc.new_scope(Some(scope));
         let fscope = tlc.new_scope(Some(scope));
         rhs = tlc.push_term(Term::Match(*cond, vec![
            (tscope, tlhs, branch1),
            (fscope, flhs, branch2)
         ]),&span);
      },
   }}
 
   Ok(rhs)
}

pub fn ll1_logical_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_compare_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::And,Symbol::Or]) {
      let op = pop_is("logical-term", tokens, &vec![Symbol::And,Symbol::Or])?;
      let op = format!("{:?}", op);
      let term2 = ll1_compare_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_compare_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_addsub_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Equal,Symbol::NotEqual,Symbol::GreaterThan,Symbol::GreaterThanOrEqual,Symbol::LessThan,Symbol::LessThanOrEqual]) {
      let op = pop_is("compare-term", tokens, &vec![Symbol::Equal,Symbol::NotEqual,Symbol::GreaterThan,Symbol::GreaterThanOrEqual,Symbol::LessThan,Symbol::LessThanOrEqual])?;
      let op = format!("{:?}", op);
      let term2 = ll1_addsub_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_addsub_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_divmul_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Plus,Symbol::Minus]) {
      let op = pop_is("addsub-term", tokens, &vec![Symbol::Plus,Symbol::Minus])?;
      let op = format!("{:?}", op);
      let term2 = ll1_divmul_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_divmul_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_power_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Div,Symbol::Mul,Symbol::Mod]) {
      let op = pop_is("divmul-term", tokens, &vec![Symbol::Div,Symbol::Mul,Symbol::Mod])?;
      let op = format!("{:?}", op);
      let term2 = ll1_power_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_power_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_infix_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Pow]) {
      let op = pop_is("power-term", tokens, &vec![Symbol::Pow])?;
      let op = format!("{:?}", op);
      let term2 = ll1_infix_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

// Implement infix syntax sugar.
// We can replace "add(1,2)" with "1 `add` 2"
pub fn ll1_infix_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_prefix_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::BackQuote]) {
      pop_is("infix-term", tokens, &vec![Symbol::BackQuote])?;

      if let Some(Symbol::Ident(op)) = tokens.take_symbol()? {
         pop_is("infix-term", tokens, &vec![Symbol::BackQuote])?;
         
         let term2 = ll1_prefix_term(tlc, scope, tokens)?;
         let t = Term::App(
            tlc.push_term(Term::Ident(op),&span),
            tlc.push_term(Term::Tuple(vec![term,term2]),&span),
         );
         term = tlc.push_term(t,&span);
      } else {
          pop_is("infix-term",tokens,&vec![
             Symbol::Ident("identifier".to_string())
          ])?;
          unreachable!("infix-term expected identifier after backquote")
      }
   }
   Ok(term)
}

pub fn ll1_prefix_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
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
         tlc.push_term(Term::Tuple(vec![term]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_index_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   pop_is("index-term", tokens, &vec![Symbol::LeftBracket])?;
   let t = ll1_term(tlc, scope, tokens)?;
   pop_is("index-term", tokens, &vec![Symbol::RightBracket])?;
   Ok(t)
}

pub fn ll1_tuple_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   pop_is("tuple-term", tokens, &vec![Symbol::LeftParen])?;
   if peek_is(tokens, &vec![Symbol::For]) {
      let t = ll1_for_term(tlc, scope, tokens)?;
      pop_is("tuple-term", tokens, &vec![Symbol::RightParen])?;
      Ok(t)
   } else {
      let mut ts = Vec::new();
      let mut comma_ok = true;
      while comma_ok && !peek_is(tokens, &vec![Symbol::RightParen]) {
         comma_ok = false;
         ts.push( ll1_term(tlc, scope, tokens)? );
         if peek_is(tokens, &vec![Symbol::Comma]) {
            pop_is("tuple-term", tokens, &vec![Symbol::Comma])?;
            comma_ok = true;
         }
      }
      pop_is("tuple-term", tokens, &vec![Symbol::RightParen])?;
      if !comma_ok && ts.len()==1 {
         Ok(ts[0])
      } else {
         Ok(tlc.push_term(Term::Tuple(ts),&span))
      }
   }
}

pub fn ll1_args_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   pop_is("args-term", tokens, &vec![Symbol::LeftParen])?;
   let mut ts = Vec::new();
   let mut comma_ok = true;
   while comma_ok && !peek_is(tokens, &vec![Symbol::RightParen]) {
      comma_ok = false;
      ts.push( ll1_term(tlc, scope, tokens)? );
      if peek_is(tokens, &vec![Symbol::Comma]) {
         pop_is("args-term", tokens, &vec![Symbol::Comma])?;
         comma_ok = true;
      }
   }
   pop_is("args-term", tokens, &vec![Symbol::RightParen])?;
   Ok(tlc.push_term(Term::Tuple(ts),&span))
}

pub fn ll1_value_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   if let Some(sym) = tokens.peek_symbol()? {
      if let Symbol::Ident(x) = sym {
         tokens.take_symbol()?;
         return Ok(tlc.push_term(Term::Ident(x.clone()), &span))
      } else if let Symbol::Value(x) = sym {
         tokens.take_symbol()?;
         return Ok(tlc.push_term(Term::Value(x.clone()), &span))
      } else if let Symbol::Fail = sym {
         pop_is("value-term", tokens, &vec![Symbol::Fail])?;
         return Ok(tlc.push_term(Term::Fail, &span))
      } else if let Symbol::Literal = sym {
         pop_is("value-term", tokens, &vec![Symbol::Literal])?;
         let mut lps = Vec::new();
         loop {
            match tokens.peek_symbol()? {
               Some(Symbol::LiteralS(v)) => {
                  tokens.take_symbol()?; 
                  let v = Term::Value(format!("\"{}\"",v));
                  let v = tlc.push_term(v, &span);
                  let st = Type::Named("String".to_string(),Vec::new());
                  let t = Term::Ascript(v, st);
                  let x = tlc.push_term(t, &span);
                  lps.push(x);
               },
               Some(Symbol::LeftBrace) => {
                  pop_is("value-term", tokens, &vec![Symbol::LeftBrace])?;
                  let x = ll1_expr_term(tlc, scope, tokens)?;
                  let st = Type::Named("String".to_string(),Vec::new());
                  let t = Term::As(x, st);
                  let x = tlc.push_term(t, &span);
                  pop_is("value-term", tokens, &vec![Symbol::RightBrace])?;
                  lps.push(x);
               },
               Some(Symbol::Literal) => { break; },
               _ => {
                  pop_is("value-term", tokens, &vec![Symbol::LeftBrace, Symbol::LiteralS("abc".to_string())])?;
               },
            };
         }
         pop_is("value-term", tokens, &vec![Symbol::Literal])?;
         if lps.len() == 1 {
            return Ok(lps[0].clone())
         } else {
            let tes = tlc.push_term(Term::Tuple(lps), &span);
            let ps = tlc.push_term(Term::Tuple(vec![tes]), &span);
            let join = tlc.push_term(Term::Ident(".join".to_string()),&span);
            let joined = tlc.push_term(Term::App(join,ps),&span);
            return Ok(joined)
         }
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
      } else {
         pop_is("value-term",tokens,&vec![
            Symbol::Literal,
            Symbol::Ident("x".to_string()),
            Symbol::Typename("A".to_string()),
            Symbol::Value("1".to_string()),
         ])?;
      }
   }
   pop_is("value-term",tokens,&vec![
      Symbol::Literal,
      Symbol::Ident("x".to_string()),
      Symbol::Typename("A".to_string()),
      Symbol::Value("1".to_string()),
   ])?;
   unreachable!("value-term expected Ident, Typename, or Value")
}

pub fn ll1_field_term(tlc: &mut TLC, _scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   pop_is("field-term", tokens, &vec![Symbol::Dot])?;
   if let Some(Symbol::Ident(f)) = tokens.peek_symbol()? {
      tokens.take_symbol()?;
      return Ok(tlc.push_term(Term::Ident(format!(".{}",f)),&span))
   } else if let Some(Symbol::Value(x)) = tokens.peek_symbol()? {
      tokens.take_symbol()?;
      return Ok(
         tlc.push_term(
            Term::Project(Constant::parse(tlc,&x).unwrap())
         ,&span)
      )
   }

   pop_is("field-term", tokens, &vec![Symbol::Ident("x".to_string())])?;
   unreachable!("field-term")
}

pub fn ll1_match_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   pop_is("match-term", tokens, &vec![Symbol::Match])?;
   let dv = ll1_term(tlc, scope, tokens)?;
   pop_is("match-term", tokens, &vec![Symbol::LeftBrace])?;
   let mut pats = Vec::new();
   let mut comma_ok = true;
   while comma_ok && !peek_is(tokens, &vec![Symbol::RightBrace]) {
      if comma_ok { comma_ok = false; }
      let lhs = ll1_prefix_term(tlc, scope, tokens)?;
      pop_is("match-term", tokens, &vec![Symbol::Imply])?;
      let rhs = ll1_expr_term(tlc, scope, tokens)?;
      pats.push((tlc.new_scope(Some(scope)), lhs, rhs));
      if peek_is(tokens, &vec![Symbol::Comma]) {
         pop_is("match-term", tokens, &vec![Symbol::Comma])?;
         comma_ok = true;
      }
   }
   pop_is("match-term", tokens, &vec![Symbol::RightBrace])?;
   Ok(tlc.push_term(Term::Match(dv, pats),&span))
}

pub fn ll1_atom_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = if peek_is(tokens, &vec![Symbol::LeftParen]) {
      ll1_tuple_term(tlc, scope, tokens)?
   } else if peek_is(tokens, &vec![Symbol::Match]) {
      ll1_match_term(tlc, scope, tokens)?
   } else {
      ll1_value_term(tlc, scope, tokens)?
   };
   while peek_is(tokens, &vec![Symbol::LeftParen,Symbol::Dot,Symbol::LeftBracket]) {
      if peek_is(tokens, &vec![Symbol::Dot]) {
         let field = ll1_field_term(tlc, scope, tokens)?;
         if !peek_is(tokens, &vec![Symbol::LeftParen]) {
            if let Term::Ident(_) = &tlc.rows[field.id].term {
               term = tlc.push_term(Term::Tuple(vec![term]),&span);
               term = tlc.push_term(Term::App(field, term),&span);
            } else {
               term = tlc.push_term(Term::App(field, term),&span);
            }
         } else {
            pop_is("atom-term", tokens, &vec![Symbol::LeftParen])?;
            let mut ts = vec![term];
            let mut comma_ok = true;
            while comma_ok && !peek_is(tokens, &vec![Symbol::RightParen]) {
               comma_ok = false;
               ts.push( ll1_term(tlc, scope, tokens)? );
               if peek_is(tokens, &vec![Symbol::Comma]) {
                  pop_is("atom-term", tokens, &vec![Symbol::Comma])?;
                  comma_ok = true;
               }
            }
            pop_is("atom-term", tokens, &vec![Symbol::RightParen])?;
            if ts.len()==0 { //x.f()
               let fargs = tlc.push_term(Term::Tuple(ts),&span);
               term = tlc.push_term(Term::App(field, fargs),&span);
               let curry = tlc.push_term(Term::Tuple(Vec::new()),&span);
               term = tlc.push_term(Term::App(term, curry),&span);
            } else { //x.f(y), x.f(y,z)
               let fargs = tlc.push_term(Term::Tuple(ts),&span);
               term = tlc.push_term(Term::App(field, fargs),&span);
            }
         }
      } else if peek_is(tokens, &vec![Symbol::LeftBracket]) {
         let prj = tlc.push_term(Term::Ident("[]".to_string()),&span);
         let index = ll1_index_term(tlc, scope, tokens)?;
         let fargs = tlc.push_term(Term::Tuple(vec![term,index]),&span);
         term = tlc.push_term(Term::App(prj, fargs),&span);
      } else {
         let args = ll1_args_term(tlc, scope, tokens)?;
         let t = Term::App(
            term,
            args
         );
         term = tlc.push_term(t,&span);
      }
   }
   Ok(term)
}

pub fn ll1_expr_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   ll1_logical_term(tlc, scope, tokens)
}

pub fn ll1_paren_type(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<Type,Error> {
   pop_is("paren-type", tokens, &vec![Symbol::LeftParen])?;
   let mut ts = Vec::new();
   let mut comma_ok = true;
   while comma_ok && !peek_is(tokens, &vec![Symbol::RightParen]) {
      comma_ok = false;
      ts.push( ll1_type(tlc, scope, tokens)? );
      if peek_is(tokens, &vec![Symbol::Comma]) {
         pop_is("paren-type", tokens, &vec![Symbol::Comma])?;
         comma_ok = true;
      }
   }
   pop_is("paren-type", tokens, &vec![Symbol::RightParen])?;
   if !comma_ok && ts.len()==1 {
      Ok(ts[0].clone())
   } else {
      Ok(Type::Tuple(ts))
   }
}

pub fn ll1_typeof_type(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<Type,Error> {
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

pub fn ll1_ident_type(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<Type,Error> {
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
         tps.push( ll1_type(tlc, scope, tokens)? );
      }
      pop_is("ident-type", tokens, &vec![Symbol::GreaterThan])?;
   }
   Ok(Type::Named(tn,tps))
}

pub fn ll1_constant(tlc: &mut TLC, _scope: ScopeId, tokens: &mut TokenReader) -> Result<Constant,Error> {
   let _span = span_of(tokens);
   if let Some(sym) = tokens.peek_symbol()? {
      if let Symbol::Value(x) = sym {
         tokens.take_symbol()?;
         return Ok(Constant::parse(tlc,&x).unwrap())
      } else if let Symbol::Typename(cname) = sym {
         tokens.take_symbol()?;
         return Ok(Constant::parse(tlc,&cname).unwrap())
        //TODO tuples like (1,2,3)
        //TODO structs as Tuples like (Point2D,1,2), order fields by alphabetical order
      } else {
         pop_is("constant-term",tokens,&vec![
            Symbol::Typename("A".to_string()),
            Symbol::Value("1".to_string()),
         ])?;
      }
   }
   pop_is("constant-term",tokens,&vec![
      Symbol::Typename("A".to_string()),
      Symbol::Value("1".to_string()),
   ])?;
   unreachable!("constant-term expected Ident, Typename, or Value")
}

pub fn ll1_dep_type(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<Type,Error> {
   pop_is("dependent-type", tokens, &vec![Symbol::LeftBracket])?;
   let cv = ll1_constant(tlc, scope, tokens)?;
   pop_is("dependent-type", tokens, &vec![Symbol::RightBracket])?;
   Ok(Type::Constant(cv))
}

pub fn ll1_atom_type(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<Type,Error> {
   if peek_is(tokens, &vec![Symbol::Question]) {
      pop_is("atom-type", tokens, &vec![Symbol::Question])?;
      Ok(Type::Any)
   } else if peek_is(tokens, &vec![Symbol::LeftParen]) {
      ll1_paren_type(tlc, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::LeftBracket]) {
      ll1_dep_type(tlc, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::Typeof]) {
      ll1_typeof_type(tlc, scope, tokens)
   } else {
      ll1_ident_type(tlc, scope, tokens)
   }
}

pub fn ll1_suffix_type(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<Type,Error> {
   let mut base = ll1_atom_type(tlc, scope, tokens)?;
   let mut ts = Vec::new();
   while peek_is(tokens, &vec![Symbol::LeftBracket,Symbol::Question]) {
      if peek_is(tokens, &vec![Symbol::Question]) {
         pop_is("suffix-type", tokens, &vec![Symbol::Question])?;
         base = Type::MaybeZero(Box::new(base));
      } else if peek_is(tokens, &vec![Symbol::LeftBracket]) {
         pop_is("suffix-type", tokens, &vec![Symbol::LeftBracket])?;
         if peek_is(tokens, &vec![Symbol::RightBracket]) {
            ts.push( Constant::Tuple(Vec::new()) );
         } else {
            ts.push( ll1_constant(tlc, scope, tokens)? );
         }
         pop_is("suffix-type", tokens, &vec![Symbol::RightBracket])?;
      } else {
         pop_is("suffix-type", tokens, &vec![Symbol::LeftBracket,Symbol::Question])?;
      }
   }
   for ct in ts.iter().rev() {
      base = Type::HTuple(Box::new(base), ct.clone());
   }
   Ok(base)
}

pub fn ll1_product_type(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<Type,Error> {
   let mut types = vec![ ll1_suffix_type(tlc, scope, tokens)? ];
   while peek_is(tokens, &vec![Symbol::Mul]) {
      pop_is("product-type", tokens, &vec![Symbol::Mul])?;
      types.push( ll1_suffix_type(tlc, scope, tokens)? );
   }
   if types.len()==1 {
      Ok(types[0].clone())
   } else {
      Ok(Type::Product(types))
   }
}

pub fn ll1_ratio_type(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<Type,Error> {
   let mut typ = ll1_product_type(tlc, scope, tokens)?;
   if peek_is(tokens, &vec![Symbol::Div]) {
      pop_is("ratio-type", tokens, &vec![Symbol::Div])?;
      let typ2 = ll1_product_type(tlc, scope, tokens)?;
      typ = Type::Ratio(Box::new(typ), Box::new(typ2));
   }
   Ok(typ)
}

pub fn ll1_arrow_type(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<Type,Error> {
   let mut typ = ll1_ratio_type(tlc, scope, tokens)?;
   if peek_is(tokens, &vec![Symbol::Arrow]) {
      pop_is("and-type", tokens, &vec![Symbol::Arrow])?;
      let typ2 = ll1_arrow_type(tlc, scope, tokens)?;
      typ = Type::Arrow(Box::new(typ), Box::new(typ2));
   }
   Ok(typ)
}

pub fn ll1_and_type(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<Type,Error> {
   let mut types = vec![ ll1_arrow_type(tlc, scope, tokens)? ];
   while peek_is(tokens, &vec![Symbol::Plus]) {
      pop_is("and-type", tokens, &vec![Symbol::Plus])?;
      types.push( ll1_arrow_type(tlc, scope, tokens)? );
   }
   if types.len()==1 {
      Ok(types[0].clone())
   } else {
      Ok(Type::And(types))
   }
}

pub fn ll1_type(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<Type,Error> {
   ll1_and_type(tlc, scope, tokens)
}

pub fn ll1_ascript_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_expr_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Ascript, Symbol::At]) {
      if peek_is(tokens, &vec![Symbol::Ascript]) {
         pop_is("ascript-term", tokens, &vec![Symbol::Ascript])?;
         let at = ll1_type(tlc, scope, tokens)?;
         let t = Term::Ascript(
            term,
            at
         );
         term = tlc.push_term(t, &span);
      } else if peek_is(tokens, &vec![Symbol::At]) {
         pop_is("ascript-term", tokens, &vec![Symbol::At])?;
         let mut at = "".to_string();
         if let Some(Symbol::Ident(name)) = tokens.peek_symbol()? {
            tokens.take_symbol()?;
            at = name.clone();
         } else {
            pop_is("ascript-term", tokens, &vec![Symbol::Ident("n".to_string())])?;
         }
         let t = Term::RuleApplication(
            term,
            at
         );
         term = tlc.push_term(t, &span);
      }
   }
   Ok(term)
}

pub fn ll1_as_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_ascript_term(tlc, scope, tokens)?;
   if peek_is(tokens, &vec![Symbol::As]) {
      pop_is("as-term", tokens, &vec![Symbol::As])?;
      let at = ll1_type(tlc, scope, tokens)?;
      let t = Term::As(
         term,
         at
      );
      term = tlc.push_term(t, &span);
   }
   Ok(term)
}

pub fn ll1_asif_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   if peek_is(tokens, &vec![Symbol::If]) {
      ll1_if_term(tlc, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::Loop]) {
      ll1_loop_term(tlc, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::While]) {
      ll1_while_term(tlc, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::For]) {
      ll1_for_term(tlc, scope, tokens)
   } else {
      ll1_as_term(tlc, scope, tokens)
   }
}

pub fn ll1_arrow_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let _span = span_of(tokens);
   if peek_is(tokens, &vec![Symbol::Fn]) {
      pop_is("arrow-term", tokens, &vec![Symbol::Fn])?;
      unimplemented!("Parse Term::Arrow");
      /*
      let mut children = Vec::new();
      let inner_scope = tlc.push_scope(Scope {
         parent: Some(scope),
         children: children,
      });

      while peek_is(tokens, &vec![Symbol::Arrow]) {
         let bterm = ll1_asif_term(tlc, scope, tokens)?;
         let t = Term::Arrow(term, bterm);
         term = tlc.push_term(t, &span);
      }
      */
   } else {
      ll1_asif_term(tlc, scope, tokens)
   }
}

pub fn ll1_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   ll1_arrow_term(tlc, scope, tokens)
}

pub fn ll1_stmt(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let stmt = if peek_is(tokens, &vec![Symbol::LeftBrace]) {
      ll1_block_stmt(tlc, scope, tokens)?
   } else if peek_is(tokens, &vec![Symbol::Type]) {
      ll1_type_stmt(tlc, scope, tokens)?
   } else if peek_is(tokens, &vec![Symbol::Import]) {
      ll1_import_stmt(tlc, scope, tokens)?
   } else if peek_is(tokens, &vec![Symbol::Forall, Symbol::Axiom]) {
      ll1_forall_stmt(tlc, scope, tokens)?
   } else if peek_is(tokens, &vec![Symbol::Let,Symbol::Extern]) {
      ll1_let_stmt(tlc, scope, tokens)?
   } else {
      ll1_term(tlc, scope, tokens)?
   };
   pop_is("file", tokens, &vec![Symbol::SemiColon])?;
   Ok(stmt)
}

pub fn ll1_import_stmt(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   pop_is("import", tokens, &vec![Symbol::Import])?;
   if let Some(Symbol::Ident(fp)) = tokens.peek_symbol()? {
      tokens.take_symbol()?;
      let mut tks = tokenize_file(tlc, &fp)?;
      ll1_file_impl(tlc, scope, &mut tks, false)
   } else {
      Err(Error {
         kind: "Parse Error".to_string(),
         rule: format!("Expected identifier in import statement"),
         span: span_of(tokens),
      })
   }
}

pub fn ll1_block_stmt(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   let scope = tlc.push_scope(Scope {
      parent: Some(scope),
      children: Vec::new(),
   });

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

pub fn ll1_file(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader) -> Result<TermId,Error> {
   ll1_file_impl(tlc, scope, tokens, true)
}

fn ll1_file_impl(tlc: &mut TLC, scope: ScopeId, tokens: &mut TokenReader, user: bool) -> Result<TermId,Error> {
   let mut es = Vec::new();

   //default to L1 prelude if none specified
   if user && !peek_is(tokens, &vec![Symbol::Import]) {
      let mut tks = tokenize_file(tlc, "preludes/l1.tlc")?;
      es.push( ll1_file_impl(tlc, scope, &mut tks, false)? );
   }

   while !peek_is(tokens, &vec![Symbol::EOF]) {
      es.push( ll1_stmt(tlc, scope, tokens)? );
   }
   pop_is("file", tokens, &vec![Symbol::EOF])?;
   Ok(tlc.push_term(Term::Block(scope,es), &span_of(tokens)))
}
