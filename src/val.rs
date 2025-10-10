use std::fmt;

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
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

#[derive(Copy, Clone)] // TODO: check this
#[repr(C, align(8))]
pub union Value {
    i: i64,
    u: u64,
    f: f64,
}

#[repr(transparent)]
pub struct GCRef(u64);

impl Value {
    pub const NIL: Self = Self::primitive(TypeTag::Nil); // -1i64 or !0u64
    pub const FALSE: Self = Self::primitive(TypeTag::False);
    pub const TRUE: Self = Self::primitive(TypeTag::True);

    pub const NAN: Self = Self {
        u: 0xfff8_0000_0000_0000,
    };
    pub const INFTY: Self = Self {
        u: 0x7ff0_0000_0000_0000,
    };
    pub const MINUS_INFTY: Self = Self {
        u: 0xfff0_0000_0000_0000,
    };

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
    pub const fn nil() -> Self {
        Self::NIL
    }

    #[inline]
    pub const fn from_bool(b: bool) -> Self {
        if b {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }

    #[inline]
    pub const fn from_f64(f: f64) -> Self {
        let i: i64 = f.to_bits().cast_signed();
        assert!(((i >> 47) as u32) < !13);
        Self { f }
    }

    #[inline]
    pub const fn from_u32(u: u32) -> Self {
        Self::tagged(TypeTag::Int, u as u64)
    }

    #[inline]
    pub const fn from_i32(i: i32) -> Self {
        Self::tagged(TypeTag::Int, (i as i64) as u64 & (!0u64 >> 17))
    }

    #[inline]
    pub const fn from_u64(u: u64) -> Option<Self> {
        if u > (!0u64 >> 18) {
            None
        } else {
            Some(Self::tagged(TypeTag::Int, u))
        }
    }

    #[inline]
    pub const fn from_i64(i: i64) -> Option<Self> {
        if i < (!(!0u64 >> 18)) as i64 || i > ((!0u64 >> 18) as i64) {
            None
        } else {
            Some(Self::tagged(TypeTag::Int, i as u64 & (!0u64 >> 17)))
        }
    }

    #[inline]
    pub fn is_nil(&self) -> bool {
        self.type_tag() == TypeTag::Nil
    }

    pub fn is_truthy(&self) -> bool {
        match self.type_tag() {
            TypeTag::Nil | TypeTag::False => false,
            TypeTag::Float => self.f64() != 0.0,
            TypeTag::Int => self.u64() != 0,
            _ => true,
        }
    }

    #[inline]
    pub fn is_falsy(&self) -> bool {
        !self.is_truthy()
    }

    #[inline]
    pub const fn f64(self) -> f64 {
        unsafe { self.f }
    }

    #[inline]
    pub const fn i64(self) -> i64 {
        (unsafe { self.i } << 17) >> 17
    }

    #[inline]
    pub const fn u64(self) -> u64 {
        (unsafe { self.u }) & (!0u64 >> 17)
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

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.u == other.u }
    }
}

impl Eq for Value {}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.type_tag() {
            TypeTag::Nil => write!(f, "nil"),
            TypeTag::False => write!(f, "false"),
            TypeTag::True => write!(f, "true"),
            TypeTag::Int => write!(f, "{}i", unsafe { self.i as i32 }),
            TypeTag::Float => write!(f, "{}f", unsafe { self.f }),
            tag => write!(f, "<{:?} {:p}>", tag, self as *const _),
        }
    }
}
