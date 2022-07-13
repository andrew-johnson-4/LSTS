use crate::debug::Error;
use std::rc::Rc;
use std::io::{BufReader};
use std::io::prelude::*;
use std::fs::File;

#[derive(Clone)]
pub struct Span {
   pub filename: Rc<String>,
   pub offset_start: usize,
   pub offset_end: usize,
   pub linecol_start: (usize,usize),
   pub linecol_end: (usize,usize),
}

pub fn span_of<R: Read>(ts: &mut TokenReader<R>) -> Span {
   if let Ok(Some(t)) = ts.peek() {
      Span {
         filename: t.span.filename.clone(),
         offset_start: t.span.offset_start,
         offset_end: t.span.offset_end,
         linecol_start: t.span.linecol_start.clone(),
         linecol_end: t.span.linecol_end.clone(),
      }
   } else {
      Span {
         filename: Rc::new("NULL String".to_string()),
         offset_start: 0,
         offset_end: 0,
         linecol_start: (0,0),
         linecol_end: (0,0),
      }
   }
}

#[derive(Clone)]
pub struct Token {
   pub symbol: Symbol,
   pub span: Span,
}

#[derive(Clone,Eq,PartialEq)]
pub enum Symbol {
   EOF,
   Ident(String),
   Typename(String),
   Value(String),
   Regex(String),
   Question,
   Imply,
   Is,
   Equal,
   NotEqual,
   GreaterThan,
   GreaterThanOrEqual,
   LessThan,
   LessThanOrEqual,
   Ascript,
   KAscript,
   Arrow,
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
   AndAlso,
   Typeof,
   As,
   If,
   Then,
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
           Symbol::EOF                => write!(f, "EOF"),
           Symbol::Ident(s)           => write!(f, r#"$"{}""#, s),
           Symbol::Typename(s)        => write!(f, "{}", s),
           Symbol::Regex(s)           => write!(f, "{}", s),
           Symbol::Value(s)           => write!(f, "'{}'", s),
           Symbol::Ascript            => write!(f, ":"),
           Symbol::KAscript           => write!(f, "::"),
           Symbol::Imply              => write!(f, "=>"),
           Symbol::Is                 => write!(f, "="),
           Symbol::Equal              => write!(f, "=="),
           Symbol::NotEqual           => write!(f, "!="),
           Symbol::GreaterThan        => write!(f, ">"),
           Symbol::GreaterThanOrEqual => write!(f, ">="),
           Symbol::LessThan           => write!(f, "<"),
           Symbol::LessThanOrEqual    => write!(f, "<="),

           Symbol::Question           => write!(f, "?"),
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
           Symbol::Arrow              => write!(f, "->"),

           Symbol::LeftBracket        => write!(f, "["),
           Symbol::RightBracket       => write!(f, "]"),
           Symbol::LeftParen          => write!(f, "("),
           Symbol::RightParen         => write!(f, ")"),
           Symbol::LeftBrace          => write!(f, "{{"),
           Symbol::RightBrace         => write!(f, "}}"),

           Symbol::AndAlso            => write!(f, "and"),
           Symbol::Typeof             => write!(f, "typeof"),
           Symbol::As                 => write!(f, "as"),
           Symbol::If                 => write!(f, "if"),
           Symbol::Then               => write!(f, "then"),
           Symbol::Else               => write!(f, "else"),
           Symbol::Let                => write!(f, "let"),
           Symbol::Forall             => write!(f, "forall"),
           Symbol::Type               => write!(f, "type"),
           Symbol::Normal             => write!(f, "normal"),
           Symbol::Where              => write!(f, "where"),
        }
    }
}
impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           Symbol::EOF                => write!(f, "EOF"),
           Symbol::Ident(s)           => write!(f, "{}", s),
           Symbol::Typename(s)        => write!(f, "{}", s),
           Symbol::Regex(s)           => write!(f, "{}", s),
           Symbol::Value(s)           => write!(f, "{}", s),
           Symbol::Ascript            => write!(f, ":"),
           Symbol::KAscript           => write!(f, "::"),
           Symbol::Imply              => write!(f, "=>"),
           Symbol::Is                 => write!(f, "="),
           Symbol::Equal              => write!(f, "=="),
           Symbol::NotEqual           => write!(f, "!="),
           Symbol::GreaterThan        => write!(f, ">"),
           Symbol::GreaterThanOrEqual => write!(f, ">="),
           Symbol::LessThan           => write!(f, "<"),
           Symbol::LessThanOrEqual    => write!(f, "<="),

           Symbol::Question           => write!(f, "?"),
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
           Symbol::Arrow              => write!(f, "->"),

           Symbol::LeftBracket        => write!(f, "["),
           Symbol::RightBracket       => write!(f, "]"),
           Symbol::LeftParen          => write!(f, "("),
           Symbol::RightParen         => write!(f, ")"),
           Symbol::LeftBrace          => write!(f, "{{"),
           Symbol::RightBrace         => write!(f, "}}"),

           Symbol::AndAlso            => write!(f, "and"),
           Symbol::Typeof             => write!(f, "typeof"),
           Symbol::As                 => write!(f, "as"),
           Symbol::If                 => write!(f, "if"),
           Symbol::Then               => write!(f, "then"),
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
   c == 'e' || c == 'E' || c == '+' || c == '-' || c == 'i' || c == '.' ||
   c.is_ascii_digit()
}

pub struct TokenReader<R: Read> {
   cbuf: [u8; 1],
   source_name: Rc<String>,
   offset_start: usize,
   line: usize,
   column: usize,
   peek: Option<Token>,
   buf: BufReader<R>,
}
impl<R: Read> TokenReader<R> {
   pub fn peek(&mut self) -> Result<Option<Token>,Error> {
      if self.peek.is_some() {
         Ok(self.peek.clone())
      } else {
         self.peek = self.take()?;
         Ok(self.peek.clone())
      }
   }

   pub fn is_operator(&self, ts: &[u8]) -> Option<(usize,Symbol)> {
      match ts {
         [b':', b':', ..] => Some((2,Symbol::KAscript)),
         [b'=', b'=', ..] => Some((2,Symbol::Equal)),
         [b'=', b'>', ..] => Some((2,Symbol::Imply)),
         [b'!', b'=', ..] => Some((2,Symbol::NotEqual)),
         [b'>', b'=', ..] => Some((2,Symbol::GreaterThanOrEqual)),
         [b'<', b'=', ..] => Some((2,Symbol::LessThanOrEqual)),
         [b'&', b'&', ..] => Some((2,Symbol::And)),
         [b'|', b'|', ..] => Some((2,Symbol::Or)),
         [b'-', b'>', ..] => Some((2,Symbol::Arrow)),
         [b'?', ..] => Some((1,Symbol::Question)),
         [b':', ..] => Some((1,Symbol::Ascript)),
         [b'=', ..] => Some((1,Symbol::Is)),
         [b'>', ..] => Some((1,Symbol::GreaterThan)),
         [b'<', ..] => Some((1,Symbol::LessThan)),
         [b'|', ..] => Some((1,Symbol::Bar)),
         [b'/', ..] => Some((1,Symbol::Div)),
         [b'*', ..] => Some((1,Symbol::Mul)),
         [b'%', ..] => Some((1,Symbol::Mod)),
         [b'+', ..] => Some((1,Symbol::Plus)),
         [b'-', ..] => Some((1,Symbol::Minus)),
         [b'^', ..] => Some((1,Symbol::Pow)),
         [b'.', ..] => Some((1,Symbol::Dot)),
         [b',', ..] => Some((1,Symbol::Comma)),
         [b';', ..] => Some((1,Symbol::SemiColon)),
         [b'\\', ..] => Some((1,Symbol::BackSlash)),
         [b'[', ..] => Some((1,Symbol::LeftBracket)),
         [b']', ..] => Some((1,Symbol::RightBracket)),
         [b'(', ..] => Some((1,Symbol::LeftParen)),
         [b')', ..] => Some((1,Symbol::RightParen)),
         [b'{', ..] => Some((1,Symbol::LeftBrace)),
         [b'}', ..] => Some((1,Symbol::RightBrace)),
         _ => None,
      }
   }

   pub fn span_of(&self, token_length:usize) -> Span {
      Span {
         filename: self.source_name.clone(),
         offset_start: self.offset_start,
         offset_end: self.offset_start + token_length,
         linecol_start: (self.line,self.column),
         linecol_end: (self.line,self.column + token_length),
      }
   }
   pub fn takec(&mut self) -> u8 {
      if self.cbuf[0]==0 {
      if let Err(_) = self.buf.read(&mut self.cbuf) {
         return 0;
      }}
      let c = self.cbuf[0];
      self.cbuf[0] = 0;
      c
   }
   pub fn take(&mut self) -> Result<Option<Token>,Error> {
      if self.peek.is_some() {
         let t = self.peek.clone();
         self.peek = None;
         return Ok(t);
      }
      let c = self.takec();
      todo!("TokenReader.take")
   }
   pub fn peek_symbol(&mut self) -> Result<Option<Symbol>,Error> {
      let t = self.peek();
      if let Ok(Some(t)) = t {
         Ok(Some(t.symbol.clone()))
      } else if let Ok(None) = t {
         Ok(None)
      } else if let Err(err) = t {
         Err(err)
      } else { unreachable!("peek symbol") }
   }
   pub fn take_symbol(&mut self) -> Result<Option<Symbol>,Error> {
      let t = self.take();
      if let Ok(Some(t)) = t {
         Ok(Some(t.symbol.clone()))
      } else if let Ok(None) = t {
         Ok(None)
      } else if let Err(err) = t {
         Err(err)
      } else { unreachable!("peek symbol") }
   }
}

pub fn tokenize_file(source_name: &str) -> Result<TokenReader<File>,Error> {
   let buf = if let Ok(f) = File::open(source_name) {
      BufReader::new(f)
   } else {
      return Err(Error{
         kind: "Tokenization Error".to_string(),
         rule: format!("Could not open file: {}", source_name),
         span: Span {
            filename: Rc::new(source_name.to_string()),
            offset_start: 0,
            offset_end: 0,
            linecol_start: (1,1),
            linecol_end: (1,1),
         }
      });
   };
   Ok(TokenReader {
      source_name:Rc::new(source_name.to_string()),
      offset_start: 0, line: 1, column: 1,
      buf:buf, peek:None, cbuf: [0;1]
   })
}

pub fn tokenize_string<'a>(source_name: &str, src: &'a str) -> Result<TokenReader<&'a [u8]>,Error> {
   let buf = BufReader::new(src.as_bytes());
   Ok(TokenReader {
      source_name:Rc::new(source_name.to_string()),
      offset_start: 0, line: 1, column: 1,
      buf:buf, peek:None, cbuf: [0;1]
   })
}

/*
pub fn tokenize(source_name:String, source: &str) -> Result<Vec<Token>,Error> {
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
               "and" => { tokens.push(Token { symbol: Symbol::AndAlso, span: span, }); },
               "typeof" => { tokens.push(Token { symbol: Symbol::Typeof, span: span, }); },
               "as" => { tokens.push(Token { symbol: Symbol::As, span: span, }); },
               "if" => { tokens.push(Token { symbol: Symbol::If, span: span, }); },
               "then" => { tokens.push(Token { symbol: Symbol::Then, span: span, }); },
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
            if &source.as_bytes()[si..ri] == r#"$""#.as_bytes() {
               let mut ci = si + 2;
               while ci<source.len() && (source.as_bytes()[ci] as char) != '"' {
                  ci += 1;
               }
               if ci<source.len() { ci += 1; }
               let span = span_of(&filename, si, ci - si, line, column);
               column += ci - si;
               let ident = std::str::from_utf8(&source.as_bytes()[si+2..ci-1]).unwrap();
               tokens.push(Token { symbol: Symbol::Ident(ident.to_string()), span: span, });
               si = ci;
               continue;
            }
            if &source.as_bytes()[si..ri] == "/^".as_bytes() {
               let mut ci = si + 2;
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
   }
   let span = span_of(&filename, si, 0, line, column);
   tokens.push(Token { symbol: Symbol::EOF, span: span, });
   Ok(tokens)
}
*/
