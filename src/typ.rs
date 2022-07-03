use std::collections::HashMap;
use crate::term::TermId;
use crate::kind::Kind;

#[derive(Clone,Copy,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub enum IsParameter {
   Yes,
   No,
   Top,
}
impl std::fmt::Debug for IsParameter {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
         IsParameter::Yes => write!(f, "YES"),
         IsParameter::No => write!(f, "NO"),
         IsParameter::Top => write!(f, "TOP"),
      }
   }
}

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
   pub fn print(&self, kinds: &HashMap<Type,Kind>) -> String {
      let ts = match self {
         Type::Any => format!("?"),
         Type::Ident(t,ts) => {
            if ts.len()==0 { format!("{}", t) }
            else { format!("{}<{}>", t, ts.iter().map(|t|t.print(kinds)).collect::<Vec<String>>().join(",") ) }
         }
         Type::And(ts) => format!("{{{}}}", ts.iter().map(|t|t.print(kinds)).collect::<Vec<String>>().join("+") ),
         Type::Tuple(ts) => format!("({})", ts.iter().map(|t|t.print(kinds)).collect::<Vec<String>>().join(",") ),
         Type::Product(ts) => format!("({})", ts.iter().map(|t|t.print(kinds)).collect::<Vec<String>>().join("*") ),
         Type::Arrow(p,b) => format!("({})=>({})", p.print(kinds), b.print(kinds)),
         Type::Ratio(n,d) => format!("({})/({})", n.print(kinds), d.print(kinds)),
         Type::Constant(v,c) => format!("[{}var#{}]", if *v {"'"} else {""}, c.id),
      };
      if let Some(k) = kinds.get(self) {
         format!("{}::{:?}", ts, k)
      } else { ts }
   }
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
            ts.sort(); ts.dedup();
            Type::And(ts)
         },
         (Type::And(ls),r) => {
            let mut ts = ls.clone();
            ts.push(r.clone());
            ts.sort(); ts.dedup();
            Type::And(ts)
         }
         (l,Type::And(rs)) => {
            let mut ts = rs.clone();
            ts.push(l.clone());
            ts.sort(); ts.dedup();
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
   pub fn domain(&self) -> Type {
      match self {
         Type::Arrow(p,_b) => *p.clone(),
         Type::And(ts) => {
            let mut cts = Vec::new();
            for ct in ts.iter() {
               match ct.domain() {
                  Type::And(mut cta) => {
                     cts.append(&mut cta);
                  }, ctr => {
                     cts.push(ctr);
                  }
               }
            }
            if cts.len()==1 { cts[0].clone() }
            else { Type::And(cts) }
         },
         _ => Type::And(Vec::new()), //absurd
      }
   }
   pub fn range(&self) -> Type {
      match self {
         Type::Arrow(_p,b) => *b.clone(),
         Type::And(ts) => {
            let mut cts = Vec::new();
            for ct in ts.iter() {
               match ct.range() {
                  Type::And(mut cta) => {
                     cts.append(&mut cta);
                  }, ctr => {
                     cts.push(ctr);
                  }
               }
            }
            if cts.len()==1 { cts[0].clone() }
            else { Type::And(cts) }
         },
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
   pub fn substitute(&self, subs:&HashMap<Type,Type>) -> Type {
      if let Some(st) = subs.get(self) {
         return st.clone();
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
         Type::Constant(_,_) => true,
      }
   }
   pub fn unify(&self, kinds: &HashMap<Type,Kind>, subs: &mut HashMap<Type,Type>, other: &Type) -> Result<Type,()> {
      self.unify_impl(&kinds, subs, other).map(|tt|tt.normalize())
   }
   pub fn kind(&self, kinds: &HashMap<Type,Kind>) -> Kind {
      if let Some(k) = kinds.get(&self) {
         return k.clone();
      }
      match self {
         Type::Constant(_,_) => Kind::Simple("Constant".to_string(),Vec::new()),
         Type::And(ats) => {
            let mut aks = Vec::new();
            for at in ats.iter() {
              aks.push(at.kind(kinds));
            }
            Kind::and(aks)
         },
         _ => Kind::Nil,
      }
   }
   pub fn narrow(&self, kinds: &HashMap<Type,Kind>, k: &Kind) -> Type {
      if !self.kind(kinds).has(k) { return Type::And(Vec::new()); } //nothing here to take
      let tt = match self {
         Type::And(ts) => {
            let mut tcs = Vec::new();
            for tc in ts.iter() {
               match tc.narrow(kinds,k) {
                  Type::And(acs) => {
                     tcs.append(&mut acs.clone());
                  }, ac => {
                     tcs.push(ac.clone());
                  }
               }
            }
            if tcs.len()==1 { tcs[0].clone() }
            else { Type::And(tcs) }
         }
         tt => tt.clone(),
      };
      eprintln!("narrow {} into {:?} yields {}", self.print(kinds), k, tt.print(kinds));
      tt
   }
   pub fn unify_impl(&self, kinds: &HashMap<Type,Kind>, subs: &mut HashMap<Type,Type>, rt: &Type) -> Result<Type,()> {
      self.unify_impl_par(kinds,subs,rt,IsParameter::Top)
   }
   pub fn unify_impl_par(&self, kinds: &HashMap<Type,Kind>, subs: &mut HashMap<Type,Type>, rt: &Type, par: IsParameter) -> Result<Type,()> {
      //lt => rt
      let lt = self;
      //eprintln!("unify {} (x) {}", lt.print(kinds), rt.print(kinds));
      if (par==IsParameter::Top && !lt.kind(kinds).has(&rt.kind(kinds))) ||
         (par==IsParameter::Yes && !lt.kind(kinds).has(&rt.kind(kinds))) {
         return Err(());
      }
      match (lt,rt) {
         //wildcard match
         //only unify left wildcards when they are returned from a function
         (Type::Any,r) if par!=IsParameter::Top => Ok(r.substitute(subs)),
         (l,Type::Any) => Ok(l.substitute(subs)),
         (Type::Ident(lv,_lps),rt) if lv.chars().all(char::is_uppercase) => {
            for (sl,sr) in subs.clone().iter() {
               if lt==sl { return rt.unify_impl_par(kinds,subs,sr,par); }
            }
            let rt = rt.narrow(kinds, &lt.kind(kinds));
            subs.insert(lt.clone(),rt.clone());
            Ok(rt.clone())
         },
         (lt,Type::Ident(rv,_rps)) if rv.chars().all(char::is_uppercase) => {
            for (sl,sr) in subs.clone().iter() {
               eprintln!("try unify rhs parameter {} (x) {}", sr.print(kinds), lt.print(kinds));
               if rt==sl { return lt.unify_impl_par(kinds,subs,sr,par); }
            }
            let lt = lt.narrow(kinds, &rt.kind(kinds));
            subs.insert(rt.clone(),lt.clone());
            Ok(lt.clone())
         },

         //conjunctive normal form takes precedence
         (_,Type::And(rts)) if rts.len()==0 => { Ok(rt.clone()) },
         (Type::And(_lts),Type::And(rts)) => {
            //lt => rt
            let mut mts = Vec::new();
            for rt in rts.iter() {
               match lt.unify_impl_par(kinds,subs,rt,par) {
                  Ok(Type::And(mut tts)) => { mts.append(&mut tts); },
                  Ok(tt) => { mts.push(tt); },
                  Err(()) => {},
               }
            }
            mts.sort(); mts.dedup();
            if mts.len()==0 { Err(()) }
            else if mts.len()==1 { Ok(mts[0].clone()) }
            else { Ok(Type::And(mts)) }
         },
         (Type::And(lts),rt) => {
            let mut mts = Vec::new();
            for ltt in lts.iter() {
               if let Ok(nt) = ltt.unify_impl_par(kinds,subs,rt,par) {
                  match nt {
                     Type::And(mut tts) => { mts.append(&mut tts); },
                     tt => { mts.push(tt); },
                  }
               }
            }
            mts.sort(); mts.dedup();
            if mts.len()==0 { Err(()) }
            else if mts.len()==1 { Ok(mts[0].clone()) }
            else { Ok(Type::And(mts)) }
         },
         (lt,Type::And(rts)) => {
            let mut mts = Vec::new();
            for rt in rts.iter() {
               if let Ok(nt) = lt.unify_impl_par(kinds,subs,rt,par) {
                  match nt {
                     Type::And(mut tts) => { mts.append(&mut tts); },
                     tt => { mts.push(tt); },
                  }
               } else { //the rhs can't be narrowed here
                  return Err(())
               }
            }
            mts.sort(); mts.dedup();
            if mts.len()==0 { Err(()) }
            else if mts.len()==1 { Ok(mts[0].clone()) }
            else { Ok(Type::And(mts)) }
         }

         //ratio Typees have next precedence
         (Type::Ratio(pl,bl),Type::Ratio(pr,br)) => {
            let pt = pl.unify_impl_par(kinds,subs,pr,par)?;
            let bt = bl.unify_impl_par(kinds,subs,br,par)?;
            Ok(Type::Ratio(Box::new(pt),Box::new(bt)))
         },
         (lt,Type::Ratio(pr,br)) => {
            match **br {
               Type::Tuple(ref bs) if bs.len()==0 => {
                  lt.unify_impl_par(kinds,subs,pr,par)
               }, _ => Err(())
            }
         },

         //everything else is a mixed bag
         (Type::Ident(lv,lps),Type::Ident(rv,rps))
         if lv==rv && lps.len()==rps.len() => {
            let mut tps = Vec::new();
            for (lp,rp) in std::iter::zip(lps,rps) {
               tps.push(lp.unify_impl_par(kinds,subs,rp,par)?);
            }
            Ok(Type::Ident(lv.clone(),tps))
         }
         (Type::Arrow(pl,bl),Type::Arrow(pr,br)) => {
            if let Type::And(ref ps) = **pr {
            if ps.len() == 0 {
               return Err(());
            }}
            let pt = pl.unify_impl_par(kinds,subs,pr,IsParameter::Yes)?;
            let bt = bl.unify_impl_par(kinds,subs,br,IsParameter::No)?;
            Ok(Type::Arrow(Box::new(pt),Box::new(bt)))
         },
         (Type::Product(la),Type::Product(ra)) if la.len()==ra.len() => {
            let mut ts = Vec::new();
            for (lt,rt) in std::iter::zip(la,ra) {
               ts.push(lt.unify_impl_par(kinds,subs,rt,par)?);
            }
            Ok(Type::Product(ts))
         },
         (Type::Tuple(la),Type::Tuple(ra)) if la.len()==ra.len() => {
            let mut ts = Vec::new();
            for (lt,rt) in std::iter::zip(la,ra) {
               eprintln!("try unify tuple item {} (x) {}", lt.print(kinds), rt.print(kinds));
               ts.push(lt.unify_impl_par(kinds,subs,rt,par)?);
               eprintln!("unified tuple item {} (x) {}", lt.print(kinds), rt.print(kinds));
            }
            Ok(Type::Tuple(ts))
         },

         (Type::Constant(lv,lc),Type::Constant(rv,rc)) => {
            //unify_impl is only capable of comparing term equality
            //constants need to reduce to actually be the SAME term
            if lc.id == rc.id {
               Ok(Type::Constant(*lv || *rv, *lc))
            //only unify left constants when they parameterize a function
            } else if par==IsParameter::Yes && *lv {
               subs.insert(lt.clone(), rt.clone());
               Ok(rt.clone())
            } else if *rv {
               subs.insert(rt.clone(), lt.clone());
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

