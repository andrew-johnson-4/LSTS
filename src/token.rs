use crate::debug::Error;
use std::rc::Rc;

#[derive(Clone)]
pub struct Span {
   pub filename: Rc<String>,
   pub offset_start: usize,
   pub offset_end: usize,
   pub linecol_start: (usize,usize),
   pub linecol_end: (usize,usize),
}

pub fn span_of(ts: &Vec<Token>) -> Span {
   if ts.len()==0 {
      Span {
         filename: Rc::new("NULL String".to_string()),
         offset_start: 0,
         offset_end: 0,
         linecol_start: (0,0),
         linecol_end: (0,0),
      }
   } else {
      Span {
         filename: ts[0].span.filename.clone(),
         offset_start: ts[0].span.offset_start,
         offset_end: ts[ts.len()-1].span.offset_end,
         linecol_start: ts[0].span.linecol_start.clone(),
         linecol_end: ts[ts.len()-1].span.linecol_end.clone(),
      }
   }
}

#[derive(Clone)]
pub struct Token {
   pub symbol: Symbol,
   pub span: Span,
}

#[derive(Clone)]
pub enum Symbol {
   Ident(String),
   Typename(String),
   Value(String),
   Regex(String),
   Is,
   Equal,
   NotEqual,
   GreaterThan,
   GreaterThanOrEqual,
   LessThan,
   LessThanOrEqual,
   Ascript,
   KAscript,
   And,
   Or,
   Bar,
   Div,
   Mul,
   Mod,
   Plus,
   Minus,
   Pow,
   Dot,
   Comma,
   SemiColon,
   BackSlash,
   LeftBracket,
   RightBracket,
   LeftParen,
   RightParen,
   LeftBrace,
   RightBrace,
   Typeof,
   As,
   If,
   Else,
   Let,
   Forall,
   Type,
   Normal,
   Where,
}
impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           Symbol::Ident(s)           => write!(f, r#"$"{}""#, s),
           Symbol::Typename(s)        => write!(f, "{}", s),
           Symbol::Regex(s)           => write!(f, "{}", s),
           Symbol::Value(s)           => write!(f, "'{}'", s),
           Symbol::Ascript            => write!(f, ":"),
           Symbol::KAscript           => write!(f, "::"),
           Symbol::Is                 => write!(f, "="),
           Symbol::Equal              => write!(f, "=="),
           Symbol::NotEqual           => write!(f, "!="),
           Symbol::GreaterThan        => write!(f, ">"),
           Symbol::GreaterThanOrEqual => write!(f, ">="),
           Symbol::LessThan           => write!(f, "<"),
           Symbol::LessThanOrEqual    => write!(f, "<="),

           Symbol::And                => write!(f, "&&"),
           Symbol::Or                 => write!(f, "||"),
           Symbol::Bar                => write!(f, "|"),
           Symbol::Div                => write!(f, "/"),
           Symbol::Mul                => write!(f, "*"),
           Symbol::Mod                => write!(f, "%"),
           Symbol::Plus               => write!(f, "+"),
           Symbol::Minus              => write!(f, "-"),
           Symbol::Pow                => write!(f, "^"),
           Symbol::Dot                => write!(f, "."),
           Symbol::Comma              => write!(f, ","),
           Symbol::SemiColon          => write!(f, ";"),
           Symbol::BackSlash          => write!(f, "\\"),

           Symbol::LeftBracket        => write!(f, "["),
           Symbol::RightBracket       => write!(f, "]"),
           Symbol::LeftParen          => write!(f, "("),
           Symbol::RightParen         => write!(f, ")"),
           Symbol::LeftBrace          => write!(f, "{{"),
           Symbol::RightBrace         => write!(f, "}}"),

           Symbol::Typeof             => write!(f, "typeof"),
           Symbol::As                 => write!(f, "as"),
           Symbol::If                 => write!(f, "if"),
           Symbol::Else               => write!(f, "else"),
           Symbol::Let                => write!(f, "let"),
           Symbol::Forall             => write!(f, "forall"),
           Symbol::Type               => write!(f, "type"),
           Symbol::Normal             => write!(f, "normal"),
           Symbol::Where              => write!(f, "where"),
        }
    }
}

pub fn is_ident_char(source: &str, index: usize) -> bool {
   let c = source.as_bytes()[index] as char;
   c == '_' || c.is_ascii_alphanumeric()
}

pub fn is_value_char(source: &str, index: usize) -> bool {
   let c = source.as_bytes()[index] as char;
   c == 'e' || c == 'E' || c == '+' || c == '-' || c == 'i' ||
   c.is_ascii_digit()
}

pub fn tokenize(source_name:String, source: &str) -> Result<Vec<Token>,Error> {
   let filename = Rc::new(source_name);
   let mut tokens = Vec::new();
   let mut si = 0;
   let mut line = 1;
   let mut column = 1;
   let operators: Vec<(&str,Symbol)> = vec![
      ("::",Symbol::KAscript),
      (":", Symbol::Ascript),
      ("==",Symbol::Equal),
      ("=", Symbol::Is),
      ("!=",Symbol::NotEqual),
      (">=",Symbol::GreaterThanOrEqual),
      (">", Symbol::GreaterThan),
      ("<=",Symbol::LessThanOrEqual),
      ("<", Symbol::LessThan),
      ("&&",Symbol::And),
      ("||",Symbol::Or),
      ("|", Symbol::Bar),
      ("/", Symbol::Div),
      ("*", Symbol::Mul),
      ("%", Symbol::Mod),
      ("+", Symbol::Plus),
      ("-", Symbol::Minus),
      ("^", Symbol::Pow),
      (".", Symbol::Dot),
      (",", Symbol::Comma),
      (";", Symbol::SemiColon),
      ("\\",Symbol::BackSlash),
      ("[", Symbol::LeftBracket),
      ("]", Symbol::RightBracket),
      ("(", Symbol::LeftParen),
      (")", Symbol::RightParen),
      ("{", Symbol::LeftBrace),
      ("}", Symbol::RightBrace),
   ];
   fn span_of(filename:&Rc<String>, start_index:usize, token_length:usize, line:usize, column:usize) -> Span {
      Span {
         filename: filename.clone(),
         offset_start: start_index,
         offset_end: start_index + token_length,
         linecol_start: (line,column),
         linecol_end: (line,column + token_length),
      }
   }
   while si < source.len() {
      match source.as_bytes()[si] as char {
         ' ' => { column += 1; si += 1; continue; },
         '\n' => { column = 1; line += 1; si += 1; continue; },
         '0'..='9' => {
            let mut ci = si + 1;
            while ci<source.len() && is_value_char(source, ci) {
               ci += 1;
            }
            let span = span_of(&filename, si, ci - si, line, column);
            column += ci - si;
            let value = std::str::from_utf8(&source.as_bytes()[si..ci]).unwrap();
            tokens.push(Token { symbol: Symbol::Value(value.to_string()), span: span, });
            si = ci;
         },
         'A'..='Z' => {
            let mut ci = si + 1;
            while ci<source.len() && is_ident_char(source, ci) {
               ci += 1;
            }
            let span = span_of(&filename, si, ci - si, line, column);
            column += ci - si;
            let tname = std::str::from_utf8(&source.as_bytes()[si..ci]).unwrap();
            tokens.push(Token { symbol: Symbol::Typename(tname.to_string()), span: span, });
            si = ci;
         },
         'a'..='z' => {
            let mut ci = si + 1;
            while ci<source.len() && is_ident_char(source, ci) {
               ci += 1;
            }
            let span = span_of(&filename, si, ci - si, line, column);
            column += ci - si;
            let ident = std::str::from_utf8(&source.as_bytes()[si..ci]).unwrap();
            match ident {
               "typeof" => { tokens.push(Token { symbol: Symbol::Typeof, span: span, }); },
               "as" => { tokens.push(Token { symbol: Symbol::As, span: span, }); },
               "if" => { tokens.push(Token { symbol: Symbol::If, span: span, }); },
               "else" => { tokens.push(Token { symbol: Symbol::Else, span: span, }); },
               "let" => { tokens.push(Token { symbol: Symbol::Let, span: span, }); },
               "forall" => { tokens.push(Token { symbol: Symbol::Forall, span: span, }); },
               "type" => { tokens.push(Token { symbol: Symbol::Type, span: span, }); },
               "normal" => { tokens.push(Token { symbol: Symbol::Normal, span: span, }); },
               "where" => { tokens.push(Token { symbol: Symbol::Where, span: span, }); },
               _ => { tokens.push(Token { symbol: Symbol::Ident(ident.to_string()), span: span, }); },
            }
            si = ci;
         },
         _ => {
            let ri = std::cmp::min( si+2, source.len() );
            if &source.as_bytes()[si..ri] == "/^".as_bytes() {
               let mut ci = si + 1;
               while ci<source.len() && (source.as_bytes()[ci] as char) != '/' {
                  ci += 1;
               }
               if ci<source.len() { ci += 1; }
               let span = span_of(&filename, si, ci - si, line, column);
               column += ci - si;
               let regex = std::str::from_utf8(&source.as_bytes()[si..ci]).unwrap();
               tokens.push(Token { symbol: Symbol::Regex(regex.to_string()), span: span, });
               si = ci;
               continue;
            }
            if &source.as_bytes()[si..ri] == "//".as_bytes() {
               let mut ci = si + 2;
               while ci<source.len() && (source.as_bytes()[ci] as char) != '\n' {
                  ci += 1;
               }
               si = ci;
               continue;
            }
            if &source.as_bytes()[si..ri] == "/*".as_bytes() {
               let mut ci = si + 2;
               while ci<source.len() && &source.as_bytes()[ci..std::cmp::min(source.len(),ci+2)] != "*/".as_bytes() {
                  ci += 1;
               }
               si = ci + 2;
               continue;
            }

            let mut found_operator = false;
            for (tok,sym) in operators.iter() {
               let ri = std::cmp::min( si+tok.len(), source.len() );
               if &source.as_bytes()[si..ri] == tok.as_bytes() {
                  let span = span_of(&filename, si, tok.len(), line, column);
                  tokens.push(Token { symbol: sym.clone(), span: span, });
                  column += tok.len();
                  si += tok.len();
                  found_operator = true;
                  break;
               }
            }
            if !found_operator { return Err(Error{
               kind: "Tokenization Error".to_string(),
               rule: format!("Unexpected character '{}'", if si<source.len() 
                            { (source.as_bytes()[si] as char).to_string() }
                       else {"EOF".to_string()} ),
               span: Span {
                  filename: filename.clone(),
                  offset_start: si,
                  offset_end: si+1,
                  linecol_start: (line,column),
                  linecol_end: (line,column+1),
               },
            }); }
         },
      };
      eprintln!("push symbol {:?}", tokens[tokens.len()-1].symbol);
   }
   Ok(tokens)
}