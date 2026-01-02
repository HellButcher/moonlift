use abfall::{Trace, Tracer};

pub type GcFunc = GcFuncObject;
pub struct GcFuncObject;

unsafe impl Trace for GcFuncObject {
    const NO_TRACE: bool = true;
    #[inline]
    fn trace(&self, _t: &Tracer) {}
}
