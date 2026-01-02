use abfall::{Trace, Tracer};
use std::ops::Deref;

pub struct GcString([u8]);

impl GcString {
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}

impl Deref for GcString {
    type Target = [u8];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

unsafe impl Trace for GcString {
    const NO_TRACE: bool = true;
    #[inline]
    fn trace(&self, _t: &Tracer) {}
}
