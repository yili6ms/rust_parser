pub mod driver;

pub use driver::{Compilation, Compiler, CompilerError};
pub use dsl_codegen::{CodegenError, emit_llvm};
pub use dsl_front::{
    ast,
    lexer::{self, LexError},
    parser::{self, ParseError},
    span, token,
};
pub use dsl_middle::{
    HirProgram, LowIrProgram, MirProgram, Type, TypeError, TypeInfo, lower_to_hir, lower_to_lowir,
    lower_to_mir, typecheck,
};
