use super::hir::HirProgram;

#[derive(Debug, Clone)]
pub struct MirProgram {
    inner: HirProgram,
}

impl MirProgram {
    pub fn hir(&self) -> &HirProgram {
        &self.inner
    }

    pub fn dump(&self) -> String {
        format!("MIR:\n{:#?}", self.inner.program())
    }
}

pub fn lower_to_mir(hir: &HirProgram) -> MirProgram {
    MirProgram { inner: hir.clone() }
}
