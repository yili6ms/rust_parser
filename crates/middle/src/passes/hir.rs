use dsl_front::ast::Program;

#[derive(Debug, Clone)]
pub struct HirProgram {
    inner: Program,
}

impl HirProgram {
    pub fn program(&self) -> &Program {
        &self.inner
    }

    pub fn dump(&self) -> String {
        format!("HIR:\n{:#?}", self.inner)
    }
}

pub fn lower_to_hir(program: &Program) -> HirProgram {
    HirProgram {
        inner: program.clone(),
    }
}
