
#[derive(Clone)]
pub enum LiteralPattern {
   Char(char),
   String(String),
   Range(String),
   Variable(String),
}

#[derive(Clone)]
pub struct DFA {
   pub start: usize,
   pub states: Vec<bool>, //bool = is accept
   pub transitions: Vec<(usize,char,usize)>,
}
