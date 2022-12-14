use crate::debug::Error;
use crate::tlc::TLC;
use regex::Regex;
use std::rc::Rc;
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
impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{},{}\n", self.filename, self.linecol_start.0, self.linecol_start.1)
    }
}

pub fn span_of(ts: &mut TokenReader) -> Span {
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
   At,
   As,
   Match,
   If,
   Then,
   Else,
   Let,
   Axiom,
   Forall,
   Type,
   Normal,
   Where,
   Loop,
   For,
   While,
   In,
   Yield,
   Fn,
   Literal,
   Fail,
   LiteralV(String),
   LiteralC(char,String),
   LiteralS(String,String),
   LiteralR(Vec<(char,char)>,String),
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
           Symbol::At                 => write!(f, "@"),

           Symbol::LeftBracket        => write!(f, "["),
           Symbol::RightBracket       => write!(f, "]"),
           Symbol::LeftParen          => write!(f, "("),
           Symbol::RightParen         => write!(f, ")"),
           Symbol::LeftBrace          => write!(f, "{{"),
           Symbol::RightBrace         => write!(f, "}}"),

           Symbol::AndAlso            => write!(f, "and"),
           Symbol::Typeof             => write!(f, "typeof"),
           Symbol::As                 => write!(f, "as"),
           Symbol::Match              => write!(f, "match"),
           Symbol::If                 => write!(f, "if"),
           Symbol::Then               => write!(f, "then"),
           Symbol::Else               => write!(f, "else"),
           Symbol::Let                => write!(f, "let"),
           Symbol::Axiom              => write!(f, "axiom"),
           Symbol::Forall             => write!(f, "forall"),
           Symbol::Type               => write!(f, "type"),
           Symbol::Normal             => write!(f, "normal"),
           Symbol::Where              => write!(f, "where"),
           Symbol::Loop               => write!(f, "loop"),
           Symbol::For                => write!(f, "for"),
           Symbol::While              => write!(f, "while"),
           Symbol::In                 => write!(f, "in"),
           Symbol::Yield              => write!(f, "yield"),
           Symbol::Fn                 => write!(f, "fn"),
           Symbol::Literal            => write!(f, "literal"),
           Symbol::Fail               => write!(f, "fail"),

           Symbol::LiteralV(v)        => write!(f, "{}", v),
           Symbol::LiteralC(c,v)      => write!(f, "'{}'{}", c, v),
           Symbol::LiteralS(s,v)      => write!(f, r#""{}"{}"#, s, v),
           Symbol::LiteralR(_r,v)     => write!(f, "[?]{}", v),
        }
    }
}

pub fn is_ident_char(c: u8) -> bool {
   let c = c as char;
   c == '_' || c.is_ascii_alphanumeric()
}

pub fn is_value_char(c: u8) -> bool {
   let c = c as char;
   c.is_ascii_digit()
}

pub struct TokenReader {
   //prelex defined tokens require infinite look-ahead 
   source_name: Rc<String>,
   offset_start: usize,
   line: usize,
   column: usize,
   peek: Option<Token>,
   buf: Vec<u8>,
   buf_at: usize,
   values: Vec<(String,Regex)>,
   in_literal: bool,
}
impl TokenReader {
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
         [b'@', ..] => Some((1,Symbol::At)),
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
      if self.buf_at >= self.buf.len() {
         return 0;
      }
      let c = self.buf[self.buf_at];
      self.buf_at += 1;
      c
   }
   pub fn peekc(&mut self) -> u8 {
      if self.buf_at >= self.buf.len() {
         return 0;
      }
      self.buf[self.buf_at]
   }
   pub fn take(&mut self) -> Result<Option<Token>,Error> {
      match self.take_impl() {
         Ok(Some(tok)) => {
            Ok(Some(tok))
         },
         t => t
      }
   }
   pub fn take_impl(&mut self) -> Result<Option<Token>,Error> {
      if self.peek.is_some() {
         let t = self.peek.clone();
         self.peek = None;
         return Ok(t);
      }

      let mut c = self.takec();

      if self.in_literal {
         while c > 0 {
         match c {
            b' ' => { self.column += 1; self.offset_start += 1; c = self.takec(); },
            b'\n' => { self.column = 1; self.line += 1; self.offset_start += 1; c = self.takec(); },
            b'\'' => {
               let e = self.takec();
               if e == b'\'' { return self.error(e as char); }
               let close = self.takec();
               if close != b'\'' { return self.error(close as char); }
               let mut n = Vec::new();
               while [b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm',
                      b'n', b'o', b'p', b'q', b'r', b's', b't', b'u', b'v', b'w', b'x', b'y', b'z'].contains(&self.peekc()) {
                  n.push(self.takec());
               }
               let span = self.span_of(n.len()+3);
               let n = std::str::from_utf8(&n).unwrap();
               return Ok(Some(Token {
                  symbol: Symbol::LiteralC(e as char, n.to_string()),
                  span: span,
               }));
            },
            b'"' => {
               let mut e = Vec::new();
               while self.peekc() != b'"' {
                  e.push(self.takec());
               }
               let e = std::str::from_utf8(&e).unwrap();
               self.takec(); // '"'
               let mut n = Vec::new();
               while [b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm',
                      b'n', b'o', b'p', b'q', b'r', b's', b't', b'u', b'v', b'w', b'x', b'y', b'z'].contains(&self.peekc()) {
                  n.push(self.takec());
               }
               let span = self.span_of(e.len()+n.len()+2);
               let n = std::str::from_utf8(&n).unwrap();
               return Ok(Some(Token {
                  symbol: Symbol::LiteralS(e.to_string(), n.to_string()),
                  span: span,
               }));
            },
            b'[' => {
               let mut rs = Vec::new();
               let mut rc = 0;
               while self.peekc() != b']' {
                  let ra = self.takec(); rc += 1;
                  if self.peekc() == b'-' {
                     self.takec(); rc += 1;
                     let rb = self.takec(); rc += 1;
                     rs.push((ra as char, rb as char));
                  } else {
                     rs.push((ra as char, ra as char));
                  }
               }
               self.takec(); //]
               let mut n = Vec::new();
               while [b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm',
                      b'n', b'o', b'p', b'q', b'r', b's', b't', b'u', b'v', b'w', b'x', b'y', b'z'].contains(&self.peekc()) {
                  n.push(self.takec());
               }
               let span = self.span_of(rc+n.len()+2);
               let n = std::str::from_utf8(&n).unwrap();
               return Ok(Some(Token {
                  symbol: Symbol::LiteralR(rs, n.to_string()),
                  span: span,
               }));
            },
            b'a'..=b'z' | b'_' => {
               let mut n = vec![c];
               while [b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm',
                      b'n', b'o', b'p', b'q', b'r', b's', b't', b'u', b'v', b'w', b'x', b'y', b'z'].contains(&self.peekc()) {
                  n.push(self.takec());
               }
               let span = self.span_of(n.len());
               let n = std::str::from_utf8(&n).unwrap();
               return Ok(Some(Token {
                  symbol: Symbol::LiteralV(n.to_string()),
                  span: span,
               }));
            },
            _ => { self.in_literal = false; break; },
         }}
      }

      while c > 0 {
      match c {
         b' ' => { self.column += 1; self.offset_start += 1; c = self.takec(); },
         b'\n' => { self.column = 1; self.line += 1; self.offset_start += 1; c = self.takec(); },
         b'A'..=b'Z' => {
            let mut token = vec![c];
            while is_ident_char(self.peekc()) {
               token.push(self.takec());
            }
            let span = self.span_of(token.len());
            self.column += token.len();
            let tname = std::str::from_utf8(&token).unwrap();
            return Ok(Some(Token {
               symbol: Symbol::Typename(tname.to_string()),
               span: span,
            }));
         },
         b'a'..=b'z' | b'_' => {
            let mut token = vec![c];
            while is_ident_char(self.peekc()) {
               token.push(self.takec());
            }
            let span = self.span_of(token.len());
            self.column += token.len();
            let ident = std::str::from_utf8(&token).unwrap();
            match ident {
               "in" => { return Ok(Some(Token { symbol: Symbol::In, span: span, })); },
               "while" => { return Ok(Some(Token { symbol: Symbol::While, span: span, })); },
               "loop" => { return Ok(Some(Token { symbol: Symbol::Loop, span: span, })); },
               "for" => { return Ok(Some(Token { symbol: Symbol::For, span: span, })); },
               "and" => { return Ok(Some(Token { symbol: Symbol::AndAlso, span: span, })); },
               "typeof" => { return Ok(Some(Token { symbol: Symbol::Typeof, span: span, })); },
               "as" => { return Ok(Some(Token { symbol: Symbol::As, span: span, })); },
               "match" => { return Ok(Some(Token { symbol: Symbol::Match, span: span, })); },
               "if" => { return Ok(Some(Token { symbol: Symbol::If, span: span, })); },
               "then" => { return Ok(Some(Token { symbol: Symbol::Then, span: span, })); },
               "else" => { return Ok(Some(Token { symbol: Symbol::Else, span: span, })); },
               "let" => { return Ok(Some(Token { symbol: Symbol::Let, span: span, })); },
               "forall" => { return Ok(Some(Token { symbol: Symbol::Forall, span: span, })); },
               "axiom" => { return Ok(Some(Token { symbol: Symbol::Axiom, span: span, })); },
               "type" => { return Ok(Some(Token { symbol: Symbol::Type, span: span, })); },
               "normal" => { return Ok(Some(Token { symbol: Symbol::Normal, span: span, })); },
               "where" => { return Ok(Some(Token { symbol: Symbol::Where, span: span, })); },
               "yield" => { return Ok(Some(Token { symbol: Symbol::Yield, span: span, })); },
               "fn" => { return Ok(Some(Token { symbol: Symbol::Fn, span: span, })); },
               "literal" => { self.in_literal=true; return Ok(Some(Token { symbol: Symbol::Literal, span: span, })); },
               "fail" => { return Ok(Some(Token { symbol: Symbol::Fail, span: span, })); },
               _ => { return Ok(Some(Token { symbol: Symbol::Ident(ident.to_string()), span: span, })); },
            }
         },
         _ => {
            let mut c2 = self.peekc();
            match [c, c2] {
               [b'$', b'"'] => {
                  //discard prefix $" and suffix "
                  self.takec(); //discard c2
                  let mut token = Vec::new();
                  c = self.takec();
                  while c>0 && c != b'"' {
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
                  let mut token = vec![c, c2];
                  self.takec(); //discard c2
                  c = self.takec();
                  while c>0 && c != b'/' {
                     token.push(c);
                     c = self.takec();
                  }
                  token.push(c);
                  let span = self.span_of(token.len());
                  self.column += token.len();
                  let rgx = std::str::from_utf8(&token).unwrap();
                  return Ok(Some(Token {
                     symbol: Symbol::Regex(rgx.to_string()),
                     span: span,
                  }));
               },
               [b'/', b'/'] => {
                  while c2>0 && c2!=b'\n' {
                     self.column += 1; self.offset_start += 1;
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

                  unsafe {
                     let buf_at = self.buf_at - 1; //untake c
                     let substring = std::str::from_utf8_unchecked(&self.buf[buf_at..]);
                     let mut longest_match = "".to_string();
                     for (_p,r) in self.values.iter() {
                        if let Some(m) = r.find(substring) {
                           let ms = m.as_str().to_string();
                           if ms.len() > longest_match.len() {
                              longest_match = ms;
                           }
                        }
                     }
                     if longest_match.len() > 0 {
                        let byte_len = longest_match.as_bytes().len();
                        for _ in 1..byte_len { self.takec(); }
                        self.column += longest_match.len();
                        return Ok(Some(Token {
                           symbol: Symbol::Value(longest_match.clone()),
                           span: self.span_of(longest_match.len()),
                        }))
                     }
                  }

                  if let Some((len,sym)) = self.is_operator(&[c,c2]) {
                     let t = Token {
                        symbol: sym.clone(),
                        span: self.span_of(len)
                     };
                     self.column += len;
                     if len==2 { self.takec(); }
                     return Ok(Some(t));
                  } else {
                     return self.error(c as char);
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
   pub fn error(&mut self, c: char) -> Result<Option<Token>,Error> {
      Err(Error{
         kind: "Tokenization Error".to_string(),
         rule: format!("Unexpected character '{}'", c),
         span: Span {
            filename: self.source_name.clone(),
            offset_start: self.offset_start,
            offset_end: self.offset_start+1,
            linecol_start: (self.line,self.column),
            linecol_end: (self.line,self.column+1),
         },
      })
   }
}

pub fn tokenize_file<'a>(tlc: &mut TLC, source_name: &str) -> Result<TokenReader,Error> {
   if let Ok(mut f) = File::open(source_name) {
      let mut line = Vec::new();
      if let Ok(_len) = f.read_to_end(&mut line) {
         tokenize_bytes(tlc, source_name, line)
      } else {
         Err(Error{
            kind: "Tokenization Error".to_string(),
            rule: format!("Could not read file: {}", source_name),
            span: Span {
               filename: Rc::new(source_name.to_string()),
               offset_start: 0,
               offset_end: 0,
               linecol_start: (1,1),
               linecol_end: (1,1),
            }
         })
      }
   } else {
      Err(Error{
         kind: "Tokenization Error".to_string(),
         rule: format!("Could not open file: {}", source_name),
         span: Span {
            filename: Rc::new(source_name.to_string()),
            offset_start: 0,
            offset_end: 0,
            linecol_start: (1,1),
            linecol_end: (1,1),
         }
      })
   }
}

pub fn tokenize_string(tlc: &mut TLC, source_name: &str, buf: &str) -> Result<TokenReader,Error> {
   let buf = buf.as_bytes().to_vec();
   tokenize_bytes(tlc, source_name, buf)
}

pub fn tokenize_bytes<'a>(tlc: &mut TLC, source_name: &str, buf: Vec<u8>) -> Result<TokenReader,Error> {

   let mut buf_at = 0;
   while buf_at < buf.len() {
      if buf_at+1 < buf.len() {
      if buf[buf_at]==b'/' && buf[buf_at+1]==b'^' {
         let mut end_at = buf_at + 2;
         while end_at < buf.len() {
            if end_at+1 < buf.len() {
            if buf[end_at]==b'$' && buf[end_at+1]==b'/' {
               end_at += 2; break;
            }}
            end_at += 1;
         }
         let rs = std::str::from_utf8(&buf[buf_at+1..end_at-2]).unwrap();
         if let Ok(r) = Regex::new(&rs) {
            tlc.value_regexes.push((rs.to_string(),r));
         } else {
            panic!("invalid regex: {}", rs)
         }
         buf_at += rs.len() + 2; continue;
      }}
      buf_at += 1;
   }

   Ok(TokenReader {
      source_name:Rc::new(source_name.to_string()),
      offset_start: 0, line: 1, column: 1,
      buf:buf, buf_at:0, peek: None,
      values: tlc.value_regexes.clone(),
      in_literal: false,
   })
}
