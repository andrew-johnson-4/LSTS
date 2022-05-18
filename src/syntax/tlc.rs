use pest::Parser;
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
   Empty,
}

impl TLC {
   pub fn check(src:&str) -> Result<(),TlcError> {
      TLC::parse(src).map(|e|{})
   }
   pub fn parse(src:&str) -> Result<TlcExpr,TlcError> {
      let pr = TlcParser::parse(Rule::file, src);
      match pr {
        Ok(_) => Ok(TlcExpr::Empty),
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
