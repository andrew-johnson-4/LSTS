use crate::token::{Span};

pub struct Error {
   pub kind: String,
   pub rule: String,
   pub span: Span,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\n{}, {}, in {} --> {},{}\n", self.kind, self.rule, self.span.filename,
               self.span.linecol_start.0, self.span.linecol_start.1)
    }
}
