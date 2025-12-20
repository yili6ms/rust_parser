use dsl_codegen::{CodegenError, emit_llvm as lower_to_llvm};
use dsl_front::{
    ast::Program,
    lexer::{self, LexError},
    parser::{self, ParseError},
    token::Token,
};
use dsl_middle::{
    HirProgram, LowIrProgram, MirProgram, TypeError, lower_to_hir, lower_to_lowir, lower_to_mir,
    typecheck,
};
use thiserror::Error;

#[derive(Debug, Default)]
pub struct Compiler;

#[derive(Debug)]
pub struct Compilation {
    pub tokens: Vec<Token>,
    pub program: Program,
    pub hir: HirProgram,
    pub mir: MirProgram,
    pub low_ir: LowIrProgram,
}

impl Compilation {
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn program(&self) -> &Program {
        &self.program
    }

    pub fn dump_tokens(&self) -> String {
        self.tokens
            .iter()
            .map(|token| format!("{:?} @ {}", token.kind, token.span))
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn dump_ast(&self) -> String {
        format!("{:#?}", self.program)
    }

    pub fn hir(&self) -> &HirProgram {
        &self.hir
    }

    pub fn dump_hir(&self) -> String {
        self.hir.dump()
    }

    pub fn mir(&self) -> &MirProgram {
        &self.mir
    }

    pub fn dump_mir(&self) -> String {
        self.mir.dump()
    }

    pub fn low_ir(&self) -> &LowIrProgram {
        &self.low_ir
    }

    pub fn dump_low_ir(&self) -> String {
        self.low_ir.dump()
    }
}

impl Compiler {
    pub fn compile(&self, source: &str) -> Result<Compilation, CompilerError> {
        let tokens = lexer::lex(source)?;
        let program = parser::parse(&tokens)?;
        typecheck(&program)?;
        let hir = lower_to_hir(&program);
        let mir = lower_to_mir(&hir);
        let low_ir = lower_to_lowir(&mir);
        Ok(Compilation {
            tokens,
            program,
            hir,
            mir,
            low_ir,
        })
    }

    pub fn emit_llvm(&self, compilation: &Compilation) -> Result<String, CompilerError> {
        Ok(lower_to_llvm(&compilation.low_ir)?)
    }
}

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error(transparent)]
    Lex(#[from] LexError),
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error(transparent)]
    Type(#[from] TypeError),
    #[error(transparent)]
    Codegen(#[from] CodegenError),
}
