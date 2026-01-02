use abfall::{Trace, Tracer};

pub struct GcTable;

impl GcTable {
    pub fn len(&self) -> usize {
        0
    }
}

unsafe impl Trace for GcTable {
    #[inline]
    fn trace(&self, _t: &Tracer) {
        // TODO: trace table entries
    }
}
