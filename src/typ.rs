use crate::term::TermId;
use crate::kind::Kind;

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub enum Type {
   Any,
   Ident(String,Vec<Type>),
   And(Vec<Type>), //Bottom is the empty conjunctive
   Arrow(Box<Type>,Box<Type>),
   Tuple(Vec<Type>),   //Tuple is order-sensitive, Nil is the empty tuple
   Product(Vec<Type>), //Product is order-insensitive
   Ratio(Box<Type>,Box<Type>),
   Constant(bool,TermId),
}

impl Type {
   pub fn project_ratio(&self) -> (Vec<Type>,Vec<Type>) {
       match self {
         Type::Ratio(p,b) => {
            let (mut pn,mut pd) = p.project_ratio();
            let (mut bn,mut bd) = b.project_ratio();
            pn.append(&mut bd);
            pd.append(&mut bn);
            (pn, pd)
         },
         Type::Product(ts) => {
            let mut tn = Vec::new();
            let mut td = Vec::new();
            for tc in ts.iter() {
               let (mut tcn,mut tcd) = tc.project_ratio();
               tn.append(&mut tcn);
               td.append(&mut tcd);
            }
            (tn, td)
         },
         tt => (vec![tt.clone()], Vec::new())
      }
   }
   pub fn is_constant(&self) -> bool {
      match self {
         Type::Constant(_,_) => true,
         _ => false,
      }
   }
   pub fn term_id(&self) -> TermId {
      match self {
         Type::Constant(_,t) => *t,
         _ => TermId { id: 0 },
      }
   }
   pub fn mask(&self) -> Type {
      match self {
         Type::Any => Type::Any,
         Type::Ident(tn,_ts) if tn.chars().all(char::is_uppercase) => Type::Any,
         Type::Ident(tn,ts) => Type::Ident(tn.clone(),ts.iter().map(|_|Type::Any).collect::<Vec<Type>>()),
         Type::Arrow(p,b) => Type::Arrow(Box::new(p.mask()),Box::new(b.mask())),
         Type::Ratio(p,b) => Type::Ratio(Box::new(p.mask()),Box::new(b.mask())),
         Type::And(ts) => Type::And(ts.iter().map(|ct|ct.mask()).collect::<Vec<Type>>()),
         Type::Tuple(ts) => Type::Tuple(ts.iter().map(|ct|ct.mask()).collect::<Vec<Type>>()),
         Type::Product(ts) => Type::Product(ts.iter().map(|ct|ct.mask()).collect::<Vec<Type>>()),
         Type::Constant(v,c) => Type::Constant(*v,*c)
      }
   }
   pub fn and(&self, other:&Type) -> Type {
      match (self,other) {
         (Type::Any,r) => r.clone(),
         (l,Type::Any) => l.clone(),
         (Type::Ident(lv,_lps),rt) if lv.chars().all(char::is_uppercase) => rt.clone(),
         (lt,Type::Ident(rv,_rps)) if rv.chars().all(char::is_uppercase) => lt.clone(),
         (Type::And(ls),Type::And(rs)) => {
            let mut ts = ls.clone();
            ts.append(&mut rs.clone());
            Type::And(ts)
         },
         (Type::And(ls),r) => {
            let mut ts = ls.clone();
            ts.push(r.clone());
            Type::And(ts)
         }
         (l,Type::And(rs)) => {
            let mut ts = rs.clone();
            ts.push(l.clone());
            Type::And(ts)
         },
         (l,r) => {
            Type::And(vec![l.clone(),r.clone()])
         }
      }
   }
   pub fn is_var(&self) -> bool {
      match self {
         Type::Ident(tn,ts) => ts.len()==0 && tn.chars().all(char::is_uppercase),
         _ => false
      }
   }
   pub fn expects(&self) -> Type {
      match self {
         Type::Arrow(p,_b) => *p.clone(),
         _ => Type::And(Vec::new()), //absurd
      }
   }
   pub fn returns(&self) -> Type {
      match self {
         Type::Arrow(_p,b) => *b.clone(),
         _ => Type::And(Vec::new()), //absurd
      }
   }
   pub fn vars(&self) -> Vec<String> {
      match self {
         Type::Any => vec![],
         Type::Ident(tn,ts) => {
            let mut nv = vec![tn.clone()];
            for tt in ts.iter() {
               nv.append(&mut tt.vars());
            }
            nv
         },
         Type::Arrow(p,b) => { let mut pv=p.vars(); pv.append(&mut b.vars()); pv },
         Type::Ratio(p,b) => { let mut pv=p.vars(); pv.append(&mut b.vars()); pv },
         Type::And(ts) => {
            let mut nv = Vec::new();
            for tt in ts.iter() {
               nv.append(&mut tt.vars());
            }
            nv
         },
         Type::Tuple(ts) => {
            let mut nv = Vec::new();
            for tt in ts.iter() {
               nv.append(&mut tt.vars());
            }
            nv
         },
         Type::Product(ts) => {
            let mut nv = Vec::new();
            for tt in ts.iter() {
               nv.append(&mut tt.vars());
            }
            nv
         },
         Type::Constant(_,_) => vec![]
      }
   }
   pub fn simplify_ratio(&self) -> Type {
      //assume Typee has already been normalized
      let (mut num, den) = self.project_ratio();
      let mut rden = Vec::new();
      for d in den.into_iter() {
         if let Some(ni) = num.iter().position(|n|n==&d) {
            num.remove(ni);
         } else {
            rden.push(d);
         }
      }
      let n = if num.len()==0 {
         Type::Tuple(Vec::new())
      } else if num.len()==1 {
         num[0].clone()
      } else {
         Type::Product(num)
      };
      if rden.len()==0 {
         n
      } else if rden.len()==1 {
         Type::Ratio(Box::new(n),Box::new(rden[0].clone()))
      } else {
         let d = Type::Product(rden);
         Type::Ratio(Box::new(n),Box::new(d))
      }
   }
   pub fn normalize(&self) -> Type {
      match self {
         Type::And(ts) => {
            let mut cnf = Vec::new();
            for ct in ts.iter() {
               let ct = ct.normalize();
               match ct {
                  Type::And(mut cts) => { cnf.append(&mut cts); },
                  _ => { cnf.push(ct); }
               }
            }
            cnf.sort(); cnf.dedup();
            if cnf.len()==1 {
               cnf[0].clone()
            } else {
               Type::And(cnf)
            }
         },
         Type::Product(ts) => {
            let mut ts = ts.iter().map(|tt|tt.normalize()).collect::<Vec<Type>>();
            ts.sort();
            Type::Product(ts).simplify_ratio()
         },
         Type::Tuple(ts) => {
            let ts = ts.iter().map(|tt|tt.normalize()).collect::<Vec<Type>>();
            Type::Tuple(ts)
         },
         Type::Ident(tn,ts) => {
            let ts = ts.iter().map(|tt|tt.normalize()).collect::<Vec<Type>>();
            Type::Ident(tn.clone(),ts)
         },
         Type::Arrow(p,b) => {
            Type::Arrow(Box::new(p.normalize()), Box::new(b.normalize()))
         },
         Type::Ratio(_p,_b) => self.simplify_ratio(),
         tt => tt.clone(),
      }
   }
   pub fn remove(&self, x:&Type) -> Type {
      if self == x { return Type::And(Vec::new()); }
      match self {
         Type::Any => Type::Any,
         Type::Arrow(p,b) => Type::Arrow(Box::new(p.remove(x)),Box::new(b.remove(x))),
         Type::Ratio(p,b) => Type::Ratio(Box::new(p.remove(x)),Box::new(b.remove(x))),
         Type::Ident(tn,ts) => Type::Ident(tn.clone(),ts.iter().map(|t| t.remove(x)).collect::<Vec<Type>>()),
         Type::And(ts) => Type::And(ts.iter().map(|t| t.remove(x)).collect::<Vec<Type>>()),
         Type::Tuple(ts) => Type::Tuple(ts.iter().map(|t| t.remove(x)).collect::<Vec<Type>>()),
         Type::Product(ts) => Type::Product(ts.iter().map(|t| t.remove(x)).collect::<Vec<Type>>()),
         Type::Constant(v,c) => Type::Constant(*v,*c)
      }.normalize()
   }
   pub fn substitute(&self, subs:&Vec<(Type,Type)>) -> Type {
      for (lt,rt) in subs.iter() {
         if self==lt { return rt.clone(); }
      }
      match self {
         Type::Any => Type::Any,
         Type::Arrow(p,b) => Type::Arrow(Box::new(p.substitute(subs)),Box::new(b.substitute(subs))),
         Type::Ratio(p,b) => Type::Ratio(Box::new(p.substitute(subs)),Box::new(b.substitute(subs))),
         Type::Ident(tn,ts) => Type::Ident(tn.clone(),ts.iter().map(|t| t.substitute(subs)).collect::<Vec<Type>>()),
         Type::And(ts) => Type::And(ts.iter().map(|t| t.substitute(subs)).collect::<Vec<Type>>()),
         Type::Tuple(ts) => Type::Tuple(ts.iter().map(|t| t.substitute(subs)).collect::<Vec<Type>>()),
         Type::Product(ts) => Type::Product(ts.iter().map(|t| t.substitute(subs)).collect::<Vec<Type>>()),
         Type::Constant(v,c) => Type::Constant(*v,*c)
      }
   }
   pub fn is_concrete(&self) -> bool {
      match self {
         Type::Any => false,
         Type::Arrow(p,b) => p.is_concrete() && b.is_concrete(),
         Type::Ratio(p,b) => p.is_concrete() && b.is_concrete(),
         Type::Ident(_tn,ts) => ts.iter().all(|tc| tc.is_concrete()),
         Type::And(ts) => ts.iter().all(|tc| tc.is_concrete()), //bottom Typee is also concrete
         Type::Tuple(ts) => ts.iter().all(|tc| tc.is_concrete()),
         Type::Product(ts) => ts.iter().all(|tc| tc.is_concrete()),
         Type::Constant(v,_) => !v,
      }
   }
   pub fn unify(&self, kinds: &Vec<(Type,Kind)>, subs: &mut Vec<(Type,Type)>, other: &Type) -> Result<Type,()> {
      eprintln!("unify {:?} (x) {:?} with {}", self, other,
         kinds.iter().map(|(t,k)|format!("{:?}::{:?}",t,k))
              .collect::<Vec<String>>().join("; ") );
      self.unify_impl(&kinds, subs, other).map(|tt|tt.normalize())
   }
   pub fn kind(&self, kinds: &Vec<(Type,Kind)>) -> Kind {
      for (kt,k) in kinds.iter() {
         if self==kt { return k.clone(); }
      }
      Kind::Simple("Term".to_string(), Vec::new())
   }
   pub fn unify_impl(&self, kinds: &Vec<(Type,Kind)>, subs: &mut Vec<(Type,Type)>, rt: &Type) -> Result<Type,()> {
      //lt => rt
      let lt = self;
      //substitution can't change the kind of a type, so kinds doesn't need to be mutable
      //unification will always reject if types are not the same Kind
      if lt==&Type::Any {
         //an Any Type on the left takes precedence in unification regardles of Kind
         let rk = rt.kind(kinds);
         match rt.substitute(subs) {
            Type::And(rts) => {
               //possibly need to narrow type after unification
               let mut acc = Vec::new();
               for ct in rts.iter() {
                  if rk.has(&ct.kind(kinds)) {
                     acc.push(ct.clone());
                  }
               }
               //it is OK for ? to unify with {}
               if acc.len()==1 { return Ok(acc[0].clone());
               } else { return Ok(Type::And(acc)); }
            }, tt => { return Ok(tt) },
         }
      }
      if !lt.kind(kinds).has(&rt.kind(kinds)) {
         //an And Type on the left can narrow to unify
         match lt {
            Type::And(lts) => {
               for ct in lts.iter() {
                  if let Ok(ut) = ct.unify_impl(kinds,subs,rt) {
                     return Ok(ut);
                  }
               }
            }, _ => {},
         }
         return Err(());
      }
      match (lt,rt) {
         //wildcard match
         (Type::Any,r) => Ok(r.substitute(subs)),
         (l,Type::Any) => Ok(l.substitute(subs)),
         (Type::Ident(lv,_lps),rt) if lv.chars().all(char::is_uppercase) => {
            for (sl,sr) in subs.clone().iter() {
               if lt==sl { return sr.unify_impl(kinds,subs,rt); }
            }
            subs.push((lt.clone(),rt.clone()));
            Ok(rt.clone())
         },
         (lt,Type::Ident(rv,_rps)) if rv.chars().all(char::is_uppercase) => {
            for (sl,sr) in subs.clone().iter() {
               if rt==sl { return sr.unify_impl(kinds,subs,lt); }
            }
            subs.push((rt.clone(),lt.clone()));
            Ok(lt.clone())
         },

         //conjunctive normal form takes precedence
         (Type::And(lts),Type::And(rts)) => {
            //lt => rt
            let mut lts = lts.clone();
            for rt in rts.iter() {
               match lt.unify_impl(kinds,subs,rt)? {
                  Type::And(mut tts) => { lts.append(&mut tts); },
                  tt => { lts.push(tt); },
               }
            }
            Ok(Type::And(lts))
         },
         (Type::And(lts),rt) => {
            let mut lts = lts.clone();
            let mut accept = false;
            for ltt in lts.clone().iter() {
               if let Ok(nt) = ltt.unify_impl(kinds,subs,rt) {
                  accept = true;
                  match nt {
                     Type::And(mut tts) => { lts.append(&mut tts); },
                     tt => { lts.push(tt); },
                  }
               }
            }
            if accept {
               if lts.len()==1 { Ok(lts[0].clone()) }
               else { Ok(Type::And(lts)) }
            } else {
               Err(())
            }
         },

         //ratio Typees have next precedence
         (Type::Ratio(pl,bl),Type::Ratio(pr,br)) => {
            let pt = pl.unify_impl(kinds,subs,pr)?;
            let bt = bl.unify_impl(kinds,subs,br)?;
            Ok(Type::Ratio(Box::new(pt),Box::new(bt)))
         },
         (lt,Type::Ratio(pr,br)) => {
            match **br {
               Type::Tuple(ref bs) if bs.len()==0 => {
                  lt.unify_impl(kinds,subs,pr)
               }, _ => Err(())
            }
         },

         //everything else is a mixed bag
         (Type::Ident(lv,lps),Type::Ident(rv,rps))
         if lv==rv && lps.len()==rps.len() => {
            let mut tps = Vec::new();
            for (lp,rp) in std::iter::zip(lps,rps) {
               tps.push(lp.unify_impl(kinds,subs,rp)?);
            }
            Ok(Type::Ident(lv.clone(),tps))
         }
         (Type::Arrow(pl,bl),Type::Arrow(pr,br)) => {
            let pt = pl.unify_impl(kinds,subs,pr)?;
            let bt = bl.unify_impl(kinds,subs,br)?;
            Ok(Type::Arrow(Box::new(pt),Box::new(bt)))
         },
         (Type::Product(la),Type::Product(ra)) if la.len()==ra.len() => {
            let mut ts = Vec::new();
            for (lt,rt) in std::iter::zip(la,ra) {
               ts.push(lt.unify_impl(kinds,subs,rt)?);
            }
            Ok(Type::Product(ts))
         },
         (Type::Tuple(la),Type::Tuple(ra)) if la.len()==ra.len() => {
            let mut ts = Vec::new();
            for (lt,rt) in std::iter::zip(la,ra) {
               ts.push(lt.unify_impl(kinds,subs,rt)?);
            }
            Ok(Type::Tuple(ts))
         },

         (Type::Constant(lv,lc),Type::Constant(rv,rc)) => {
            if lc.id == rc.id {
               //unify_impl is only capable of comparing term equality
               //constants need to reduce to actually be the SAME term
               Ok(Type::Constant(*lv, *lc))
            } else if *lv {
               subs.push((lt.clone(), rt.clone()));
               Ok(rt.clone())
            } else if *rv {
               subs.push((rt.clone(), lt.clone()));
               Ok(lt.clone())
            } else {
               Err(())
            }
         },
         _ => Err(()),
      }
   }


}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           Type::Any => write!(f, "?"),
           Type::Ident(t,ts) => {
              if ts.len()==0 { write!(f, "{}", t) }
              else { write!(f, "{}<{}>", t, ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join(",") ) }
           }
           Type::And(ts) => write!(f, "{{{}}}", ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join("+") ),
           Type::Tuple(ts) => write!(f, "({})", ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join(",") ),
           Type::Product(ts) => write!(f, "({})", ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join("*") ),
           Type::Arrow(p,b) => write!(f, "({:?})=>({:?})", p, b),
           Type::Ratio(n,d) => write!(f, "({:?})/({:?})", n, d),
           Type::Constant(v,c) => write!(f, "[{}term#{}]", if *v {"'"} else {""}, c.id),
        }
    }
}

