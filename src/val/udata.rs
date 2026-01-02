use abfall::{Trace, Tracer};

pub struct GcUdata;

impl GcUdata {
    pub fn len(&self) -> usize {
        0
    }
}

unsafe impl Trace for GcUdata {
    const NO_TRACE: bool = true;
    #[inline]
    fn trace(&self, _tracer: &Tracer) {}
}
