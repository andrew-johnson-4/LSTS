
#[derive(Clone)]
pub enum LiteralPattern {
   Char(char),
   String(String),
   Range(String),
   Variable(String),
}

#[derive(Clone)]
pub struct DFA {
   start: usize,
   states: Vec<bool>, //bool = is accept
   transitions: Vec<(usize,char,usize)>,
}
