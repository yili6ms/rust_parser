use super::mir::MirProgram;

#[derive(Debug, Clone)]
pub struct LowIrProgram {
    inner: MirProgram,
}

impl LowIrProgram {
    pub fn mir(&self) -> &MirProgram {
        &self.inner
    }

    pub fn dump(&self) -> String {
        format!("LowIR:\n{:#?}", self.inner.hir().program())
    }
}

pub fn lower_to_lowir(mir: &MirProgram) -> LowIrProgram {
    LowIrProgram { inner: mir.clone() }
}
