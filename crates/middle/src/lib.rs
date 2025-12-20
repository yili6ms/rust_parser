pub mod passes;

pub use passes::hir::{HirProgram, lower_to_hir};
pub use passes::lowir::{LowIrProgram, lower_to_lowir};
pub use passes::mir::{MirProgram, lower_to_mir};
pub use passes::typecheck::{FunctionSig, Type, TypeError, type_from_ast, typecheck};
