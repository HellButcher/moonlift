use abfall::{Trace, Tracer};

pub type GcUpval = GcUpvalObject;
pub struct GcUpvalObject;

unsafe impl Trace for GcUpvalObject {
    #[inline]
    fn trace(&self, _t: &Tracer) {}
}
