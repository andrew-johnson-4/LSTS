use pest::Parser;
use pest::iterators::{Pair,Pairs};
use pest::error::{ErrorVariant,InputLocation,LineColLocation};

#[derive(Parser)]
#[grammar = "tlc.pest"]
struct TlcParser;

pub struct TLC;

pub struct TlcError {
   error_type: String,
   rule: String,
   filename: String,
   start: (usize,usize),
   end: (usize,usize),
   snippet: String
}
impl std::fmt::Debug for TlcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}, expected {}, in {} --> {},{}{}", self.error_type, self.rule, self.filename,
               self.start.0, self.start.1, self.snippet)
    }
}

#[derive(Clone)]
pub enum TlcExpr {
   Nil,
   Ident(String),
   Block(Vec<TlcExpr>),
   Ascript(Box<TlcExpr>,Box<TlcExpr>),
}

impl TLC {
   pub fn normalize_file(ps: Pairs<crate::syntax::tlc::Rule>) -> Result<TlcExpr,TlcError> {
      TLC::normalize_ast(ps.peek().unwrap())
   }
/*
ident_term = { ident }
tuple_term = { paren_atom }
ascript_term = { term_atom ~ (":" ~ typ)? }

paren_atom = { "(" ~ (term ~ ("," ~ term)*)? ~ ")" }
term_atom = { (ident_term | tuple_term) ~ (paren_atom)* }

ident_typ = { ident }
paren_typ = { "(" ~ (typ ~ ("," ~ typ)*)? ~ ")" }
angle_typ = { "<" ~ (typ ~ ("," ~ typ)*)? ~ ">" }
brack_typ = { "[" ~ (typ ~ ("," ~ typ)*)? ~ "]" }
atom_typ  = { paren_typ | ident_typ }
suffix_typ = { atom_typ? ~ (brack_typ | angle_typ)* }
arrow_typ = { suffix_typ ~ ("->" ~ suffix_typ)* }
and_typ    = { arrow_typ ~ ("+" ~ arrow_typ)* }
or_typ    = { and_typ ~ ("|" ~ and_typ)* }
typ       = { or_typ }

let_stmt = { "let" ~ ident ~ ("=" ~ term)? ~ (":" ~ typ)? }
*/
   pub fn normalize_ast(p: Pair<crate::syntax::tlc::Rule>) -> Result<TlcExpr,TlcError> {
      match p.as_rule() {
         //entry point rule
         Rule::file => {
            let es = p.into_inner().map(|e|TLC::normalize_ast(e).expect("TLC Grammar Error in rule [file]"))
                      .collect::<Vec<TlcExpr>>();
            if es.len()==1 {
               Ok(es[0].clone())
            } else {
               Ok(TlcExpr::Block(es))
            }
         },

         //passthrough rules
         Rule::stmt => TLC::normalize_ast(p.into_inner().peek().unwrap()),
         Rule::term => TLC::normalize_ast(p.into_inner().peek().unwrap()),

         //literal value rules
         Rule::ident => Ok(TlcExpr::Ident(p.into_inner().concat())),

         //complex rules
         Rule::ascript_term => {
            let mut es = p.into_inner();
            match (es.next(),es.next()) {
               (Some(e),None) => TLC::normalize_ast(e),
               (Some(e),Some(tt)) => Ok(TlcExpr::Ascript(
                  Box::new(TLC::normalize_ast(e)?), //term
                  Box::new(TLC::normalize_ast(tt)?) //type
               )),
               _ => panic!("TLC Grammar Error in rule [ascript_term]")
            }
         }

         //Rule::paren_atom => TLC::normalize_ast(p.into_inner()),
         //Rule::term_atom => TLC::normalize_ast(p.into_inner()),
         //Rule::ident_term => TLC::normalize_ast(p.into_inner()),
         //Rule::tuple_term => TLC::normalize_ast(p.into_inner()),
         //Rule::ascript_term => TLC::normalize_ast(p.into_inner()),
         rule => panic!("unexpected rule: {:?}", rule)
      }
   }
   pub fn typecheck(e: TlcExpr) -> Result<(),TlcError> {
      Ok(())
   }
   pub fn check(src:&str) -> Result<(),TlcError> {
      let ast = TLC::parse(src)?;
      TLC::typecheck(ast)
   }
   pub fn parse(src:&str) -> Result<TlcExpr,TlcError> {
      let parse_result = TlcParser::parse(Rule::file, src);
      match parse_result {
        Ok(parse_ast) => TLC::normalize_file(parse_ast),
        Err(pe) => {
          let (start,end) = match pe.line_col {
             LineColLocation::Pos(s) => (s,s),
             LineColLocation::Span(s,e) => (s,e),
          };
          let (istart,iend) = match pe.location {
             InputLocation::Pos(s) => (s,s),
             InputLocation::Span((s,e)) => (s,e),
          };
          let rule = match pe.variant {
             ErrorVariant::ParsingError {
                positives:p,
                negatives:n
             } => {
                p.iter().map(|r|{format!("{:?}",r)}).collect::<Vec<String>>().join(" or ")
             }, _ => {format!("")}
          };
          Err(TlcError { 
             error_type: "Parse Error".to_string(),
             rule: rule,
             filename:"[string]".to_string(),
             start:start, end:end,
             snippet: if iend>istart { format!("\n{}", &src[istart..iend]) }
                      else { format!(" {:?}", &src[istart..std::cmp::min(src.len(),istart+1)])}
          })
        }
      } 
   }
}
