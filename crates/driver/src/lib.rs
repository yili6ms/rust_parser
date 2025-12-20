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
    HirProgram, LowIrProgram, MirProgram, Type, TypeError, lower_to_hir, lower_to_lowir,
    lower_to_mir, typecheck,
};

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"
fn add(x: i32, y: i32) -> i32 {
    let sum: i32 = x + y;
    sum
}

fn main_dummy() -> i32 {
    if true then add(1, 2) else 0
}
"#;

    #[test]
    fn sample_program_typechecks() {
        let tokens = lexer::lex(SAMPLE).expect("lexing sample");
        let program = parser::parse(&tokens).expect("parsing sample");
        typecheck(&program).expect("typechecking sample");
    }

    #[test]
    fn detects_type_mismatch() {
        let src = "fn bad(x: i32) -> bool { x }";
        let tokens = lexer::lex(src).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = typecheck(&program).unwrap_err();
        match err {
            TypeError::Mismatch { .. } => {}
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn emits_llvm_ir() {
        let tokens = lexer::lex(SAMPLE).expect("lex sample");
        let program = parser::parse(&tokens).expect("parse sample");
        typecheck(&program).expect("typecheck sample");
        let hir = lower_to_hir(&program);
        let mir = lower_to_mir(&hir);
        let low_ir = lower_to_lowir(&mir);
        let ir = emit_llvm(&low_ir).expect("codegen sample");
        assert!(ir.contains("define i32 @add"));
        assert!(ir.contains("call i32 @add(i32 1, i32 2)"));
    }

    #[test]
    fn compiler_driver_runs_full_pipeline() {
        let compiler = Compiler::default();
        let compilation = compiler.compile(SAMPLE).expect("compile sample");
        assert!(!compilation.tokens.is_empty());
        assert_eq!(compilation.program.items.len(), 2);
        let ir = compiler.emit_llvm(&compilation).expect("emit llvm");
        assert!(ir.contains("define i32 @add"));
    }
}
