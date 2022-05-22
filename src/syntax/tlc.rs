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
pub enum TlcExpr {
   Nil,
   Ident(String),
}

impl TLC {
   pub fn normalize_ast(ps: Pairs<crate::syntax::tlc::Rule>) -> Result<TlcExpr,TlcError> {
      let p = ps.peek().unwrap();
      match p.as_rule() {
         Rule::term => TLC::normalize_ast(p.into_inner()),
         Rule::ident => Ok(TlcExpr::Ident(Pairs::single(p).concat())),
         Rule::paren_atom => TLC::normalize_ast(p.into_inner()),
         Rule::term_atom => TLC::normalize_ast(p.into_inner()),
         Rule::ident_term => TLC::normalize_ast(p.into_inner()),
         Rule::tuple_term => TLC::normalize_ast(p.into_inner()),
         Rule::ascript_term => TLC::normalize_ast(p.into_inner()),
         _ => panic!("unexpected rule: {:?}", p.as_rule())
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
        Ok(parse_ast) => TLC::normalize_ast(parse_ast),
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
