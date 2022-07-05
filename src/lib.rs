extern crate pest;
#[macro_use]
extern crate pest_derive;

//! Large Scale Type Systems (LSTS)
//! LSTS implements a categorical view of typed lambda calculus with flexible soundness guarantees

/// The TLC module provides a context object for parsing, typing, and evaluating expressions
pub mod tlc;

/// The Typ module provides abstractions over Types
pub mod typ;

/// The Kind module provides abstractions over Kinds
/// A Term may have multiple Kinds
/// A Type has precisely one Kind
/// A Kind can always be written as K1<K2,K3> for Kinds K1, K2, and K3
pub mod kind;

/// The Term module defines lambda calculus expressions
pub mod term;

/// The Scope module defines the shape of a typing and evaluation context
pub mod scope;
