//! Large Scale Type Systems (LSTS)
//! LSTS implements a categorical view of typed lambda calculus with flexible soundness guarantees

extern crate pest;
#[macro_use]
extern crate pest_derive;


/// The TLC module provides a context object for parsing, typing, and evaluating expressions
pub mod tlc;

/// The Typ module provides abstractions over Types
pub mod typ;

/// The Kind module provides abstractions over Kinds
/// A Term has at least the kind Term
/// A Type has precisely one Kind unless that Type is an And
/// A Kind can always be written as K1<K2,K3> for Kinds K1, K2, and K3
/// There are three intrinsic Kinds: Nil, Term, and Constant
pub mod kind;

/// The Term module defines lambda calculus expressions
pub mod term;

/// The Scope module defines the shape of a typing and evaluation context
pub mod scope;
