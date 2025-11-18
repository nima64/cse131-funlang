// Library module for shared code between runtime and compiler

pub mod common;
pub mod types;
mod typechecker;

// Re-export all items from common (no external dependencies)
pub use common::*;

// Re-export compiler-specific types
pub use types::*;