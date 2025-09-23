/*
** Internel format of gc references from lua-jit:
**
** Format for 32 bit GC references:
**
** Internal tags overlap the MSW of a number object (must be a double).
** Interpreted as a double these are special NaNs. The FPU only generates
** one type of NaN (0xfff8_0000_0000_0000). So MSWs > 0xfff80000 are available
** for use as internal tags. Small negative numbers are used to shorten the
** encoding of type comparisons (reg/mem against sign-ext. 8 bit immediate).
**
**                  ---MSW---.---LSW---
** primitive types |  itype  |         |
** lightuserdata   |  itype  |  void * |  (32 bit platforms)
** lightuserdata   |ffff|seg|    ofs   |  (64 bit platforms)
** GC objects      |  itype  |  GCRef  |
** int (LJ_DUALNUM)|  itype  |   int   |
** number           -------double------
**
** Format for 64 bit GC references (LJ_GC64):
**
** The upper 13 bits must be 1 (0xfff8...) for a special NaN. The next
** 4 bits hold the internal tag. The lowest 47 bits either hold a pointer,
** a zero-extended 32 bit integer or all bits set to 1 for primitive types.
**
**                     ------MSW------.------LSW------
** primitive types    |1..1|itype|1..................1|
** GC objects         |1..1|itype|-------GCRef--------|
** lightuserdata      |1..1|itype|seg|------ofs-------|
** int (LJ_DUALNUM)   |1..1|itype|0..0|-----int-------|
** number              ------------double-------------
**
** ORDER LJ_T
** Primitive types nil/false/true must be first, lightuserdata next.
** GC objects are at the end, table/userdata must be lowest.
** Also check lj_ir.h for similar ordering constraints.
*/
#[non_exhaustive]
#[repr(u8)]
pub enum TypeTag {
    Nil = !0,
    False = !1,
    True = !2,
    LightUData = !3,
    String = !4,
    Upval = !5,
    Thread = !6,
    Proto = !7,
    Func = !8,
    Trace = !9,
    Cdata = !10,
    Tab = !11,
    UData = !12,
    Int = !13,   // start of numbers
    Float = !15, // not tagged
}

impl TypeTag {
    #[inline]
    pub fn is_primitive(self) -> bool {
        self as u8 >= Self::True as u8
    }
    #[inline]
    pub fn is_bool(self) -> bool {
        matches!(self, Self::True | Self::False)
    }
    #[inline]
    pub fn is_num(self) -> bool {
        self as u8 <= Self::True as u8
    }
    #[inline]
    pub fn is_gc_value(self) -> bool {
        let v = self as u8;
        Self::String as u8 >= v && v > Self::Int as u8
    }
}

#[derive(Copy,Clone)] // TODO: check this
#[repr(C, align(8))]
pub union Value {
    i: i64,
    u: u64,
    f: f64,
}

#[repr(transparent)]
pub struct GCRef(u64);

impl Value {
    pub const NIL: Self = Self::primitive(TypeTag::Nil);
    pub const FALSE: Self = Self::primitive(TypeTag::False);
    pub const TRUE: Self = Self::primitive(TypeTag::True);

    pub const NAN: Self = Self { u: 0xfff8_0000_0000_0000 }; 
    pub const INFTY: Self = Self { u: 0x7ff0_0000_0000_0000 }; 
    pub const MINUS_INFTY: Self = Self { u: 0xfff0_0000_0000_0000 }; 

    #[inline]
    const fn tagged(tag: TypeTag, v: u64) -> Self {
        Self {
            u: 0xfff8_0000_0000_0000 | v | (tag as u8 as u64) << 47,
        }
    }

    #[inline]
    const fn primitive(tag: TypeTag) -> Self {
        Self::tagged(tag, 0x7fff_ffff_ffff)
    }

    #[inline]
    pub const fn f64(f: f64) -> Self {
        let i: i64 = unsafe { f64::to_bits(f).cast_signed() };
        assert!(((i >> 47) as u32) < !13);
        Self{ f }
    }
    
    #[inline]
    pub const fn u32(u: u32) -> Self {
        Self::tagged(TypeTag::Int, u as u64)
    }

    pub const fn type_tag(self) -> TypeTag {
        let t = unsafe { (self.i >> 47) as u32 };
        if t < !13 {
            TypeTag::Float
        } else {
            unsafe { std::mem::transmute(t as u8) }
        }
    }
}
