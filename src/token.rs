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

pub fn is_ident_char(c: u8) -> bool {
   let c = c as char;
   c == '_' || c.is_ascii_alphanumeric()
}

pub fn is_value_char(c: u8) -> bool {
   let c = c as char;
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
      let mut c = self.takec();

      while c > 0 {
      match c {
         b' ' => { self.column += 1; self.offset_start += 1; c = self.takec(); },
         b'\n' => { self.column = 1; self.line += 1; self.offset_start += 1; c = self.takec(); },
         b'0'..=b'9' => {
            let mut token = Vec::new();
            while is_value_char(c) {
               token.push(c);
               c = self.takec();
            }
            self.cbuf[0] = c; //push last char back onto cbuf
            let span = self.span_of(token.len());
            self.column += token.len();
            let value = std::str::from_utf8(&token).unwrap();
            return Ok(Some(Token {
               symbol: Symbol::Value(value.to_string()),
               span: span,
            }));
         },
         b'A'..=b'Z' => {
            let mut token = Vec::new();
            while is_ident_char(c) {
               token.push(c);
               c = self.takec();
            }
            self.cbuf[0] = c; //push last char back onto cbuf
            let span = self.span_of(token.len());
            self.column += token.len();
            let tname = std::str::from_utf8(&token).unwrap();
            return Ok(Some(Token {
               symbol: Symbol::Typename(tname.to_string()),
               span: span,
            }));
         },
         b'a'..=b'z' => {
            let mut token = Vec::new();
            while is_ident_char(c) {
               token.push(c);
               c = self.takec();
            }
            self.cbuf[0] = c; //push last char back onto cbuf
            let span = self.span_of(token.len());
            self.column += token.len();
            let ident = std::str::from_utf8(&token).unwrap();
            match ident {
               "and" => { return Ok(Some(Token { symbol: Symbol::AndAlso, span: span, })); },
               "typeof" => { return Ok(Some(Token { symbol: Symbol::Typeof, span: span, })); },
               "as" => { return Ok(Some(Token { symbol: Symbol::As, span: span, })); },
               "if" => { return Ok(Some(Token { symbol: Symbol::If, span: span, })); },
               "then" => { return Ok(Some(Token { symbol: Symbol::Then, span: span, })); },
               "else" => { return Ok(Some(Token { symbol: Symbol::Else, span: span, })); },
               "let" => { return Ok(Some(Token { symbol: Symbol::Let, span: span, })); },
               "forall" => { return Ok(Some(Token { symbol: Symbol::Forall, span: span, })); },
               "type" => { return Ok(Some(Token { symbol: Symbol::Type, span: span, })); },
               "normal" => { return Ok(Some(Token { symbol: Symbol::Normal, span: span, })); },
               "where" => { return Ok(Some(Token { symbol: Symbol::Where, span: span, })); },
               _ => { return Ok(Some(Token { symbol: Symbol::Ident(ident.to_string()), span: span, })); },
            }
         },
         _ => {
            let mut c2 = self.takec();
            match [c, c2] {
               [b'$', b'"'] => {
                  let mut token = Vec::new();
                  c = self.takec();
                  while c != b'"' {
                     token.push(c);
                     c = self.takec();
                  }
                  let span = self.span_of(token.len()+3);
                  self.column += token.len()+3;
                  let ident = std::str::from_utf8(&token).unwrap();
                  return Ok(Some(Token {
                     symbol: Symbol::Ident(ident.to_string()),
                     span: span,
                  }));
               }, 
               [b'/', b'^'] => {
                  todo!("tokenize regex")
                  /*
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
                  */
               },
               [b'/', b'/'] => {
                  while c2>0 && c2!=b'\n' {
                     self.column += 1; self.offset_start += 1;
                     c = c2;
                     c2 = self.takec();
                  }
                  self.column += 1; self.offset_start += 1;
                  c = c2;
               },
               [b'/', b'*'] => {
                  while c2>0 && !(c==b'*' && c2==b'/') {
                     if c==b'\n' { self.column = 1; self.line += 1; self.offset_start += 1;
                     } else { self.column += 1; self.offset_start += 1; };
                     c = c2;
                     c2 = self.takec();
                  }
                  if c==b'\n' { self.column = 1; self.line += 1; self.offset_start += 1;
                  } else { self.column += 1; self.offset_start += 1; };
                  if c2==b'\n' { self.column = 1; self.line += 1; self.offset_start += 1;
                  } else { self.column += 1; self.offset_start += 1; };
                  c = self.takec();
               },
               _ => {
                  if let Some((len,sym)) = self.is_operator(&[c,c2]) {
                     let t = Token {
                        symbol: sym.clone(),
                        span: self.span_of(len)
                     };
                     self.column += len;
                     if len==1 { self.cbuf[0] = c2; }
                     return Ok(Some(t));
                  } else {
                     return Err(Error{
                        kind: "Tokenization Error".to_string(),
                        rule: format!("Unexpected character '{}'", c as char),
                        span: Span {
                           filename: self.source_name.clone(),
                           offset_start: self.offset_start,
                           offset_end: self.offset_start+1,
                           linecol_start: (self.line,self.column),
                           linecol_end: (self.line,self.column+1),
                        },
                     });
                  }
               }
            }
         }
      }}
      Ok(Some(Token {
         symbol: Symbol::EOF,
         span: self.span_of(0),
      }))
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
