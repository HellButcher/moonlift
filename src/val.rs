//! Dynamic value representation for the Moonlift Lua VM.
//!
//! This module implements a NaN-packed value system that efficiently represents
//! all Lua value types in a single 64-bit word. The design is inspired by 
//! LuaJIT's value representation and similar systems used in modern JavaScript
//! engines like V8 and SpiderMonkey.
//!
//! # Key Benefits
//!
//! - **Memory efficiency**: All values fit in 64 bits, reducing memory usage
//! - **Cache performance**: Uniform size improves cache locality
//! - **Type checking speed**: Type discrimination via fast bit operations
//! - **Integer fast path**: Common integer operations avoid heap allocation
//!
//! # Value Types
//!
//! The system supports all Lua 5.4 value types:
//! - Primitives: `nil`, `true`, `false`
//! - Numbers: 47-bit integers and IEEE 754 doubles
//! - Objects: Strings, tables, functions, userdata, etc. (via GC references)
//!
//! See the [`Value`] type for detailed documentation on the NaN-packing scheme.

use std::fmt;

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

/// A NaN-tagged dynamic value representation for the Moonlift Lua VM.
///
/// This union uses IEEE 754 double-precision floating-point NaN space to pack
/// type information and values into a single 64-bit word. This technique is
/// commonly used in dynamic language VMs (like LuaJIT, V8, SpiderMonkey) for
/// efficient value representation.
///
/// # Memory Layout
///
/// The 64-bit value is interpreted differently based on the upper bits:
///
/// ```text
/// Float values (non-NaN):
/// ┌─────────────────────────────────────────────────────────────────┐
/// │                    IEEE 754 double (f64)                       │
/// └─────────────────────────────────────────────────────────────────┘
/// 63                                                               0
///
/// Tagged values (using NaN space):
/// ┌─────┬────────┬─────────────────────────────────────────────────┐
/// │ NaN │  Type  │                  Payload                        │
/// │1111 │  Tag   │                                                 │
/// └─────┴────────┴─────────────────────────────────────────────────┘
/// 63  60 59    47 46                                               0
/// ```
///
/// # Type Encoding
///
/// - **Float values**: When the upper 17 bits don't match the NaN pattern
///   (`0xfff8`), the entire 64 bits represent an IEEE 754 double.
/// - **Tagged values**: When upper 17 bits are `0xfff8`, bits 59-47 contain
///   the type tag, and bits 46-0 contain the payload.
///
/// # Value Types
///
/// - **Primitives** (`nil`, `true`, `false`): Use special payload values
/// - **Integers**: 47-bit signed integers stored in the payload
/// - **Floats**: Full IEEE 754 doubles (when not in NaN space)
/// - **GC Objects**: References to garbage-collected objects like strings,
///   tables, functions, etc.
///
/// # Performance Benefits
///
/// 1. **Cache efficiency**: All values fit in 64 bits
/// 2. **Branch reduction**: Type checking via bit operations
/// 3. **NaN safety**: Real NaN values are preserved as floats
/// 4. **Integer fast path**: Common integer operations avoid boxing
///
/// # Safety
///
/// This union uses `unsafe` code internally but provides safe APIs. The
/// invariants are maintained by the constructor functions and type checking
/// methods.
#[derive(Copy, Clone)]
#[repr(C, align(8))]
pub union Value {
    /// Signed integer view of the raw bits
    i: i64,
    /// Unsigned integer view of the raw bits  
    u: u64,
    /// IEEE 754 double-precision floating-point view
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

    /// Creates a tagged value by combining a type tag with a payload.
    ///
    /// This is the core function for creating non-float values. It constructs
    /// the NaN-packed representation by:
    /// 1. Starting with the NaN prefix (`0xfff8_0000_0000_0000`)
    /// 2. Encoding the type tag in bits 59-47
    /// 3. Storing the payload in the lower 47 bits
    ///
    /// # Arguments
    /// * `tag` - The type tag identifying the value type
    /// * `v` - The payload value (must fit in 47 bits)
    #[inline]
    const fn tagged(tag: TypeTag, v: u64) -> Self {
        Self {
            u: 0xfff8_0000_0000_0000 | v | (tag as u8 as u64) << 47,
        }
    }

    /// Creates a primitive value (nil, true, false).
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

    /// Creates a `Value` from a 64-bit float.
    ///
    /// This stores the float directly in the union without any NaN-packing.
    /// Regular IEEE 754 floats (including actual NaN values) can be distinguished
    /// from tagged values because their upper 17 bits don't match the NaN-packing
    /// pattern used for tagged values.
    ///
    /// # Assertion
    /// The assertion ensures that this float value won't be confused with a
    /// tagged value by checking that the upper bits don't match our tagging
    /// pattern (values >= `!13` in the upper bits indicate tagged values).
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

    /// Creates a `Value` from a 64-bit unsigned integer, if it fits in the payload.
    ///
    /// Since the payload area is only 47 bits, this can only represent values
    /// up to `2^47 - 1`. Larger values return `None` and should be represented
    /// as floats instead.
    ///
    /// # Range
    /// - Valid: `0..=0x7FFF_FFFF_FFFF` (47-bit unsigned)
    /// - Invalid: Values requiring more than 47 bits
    #[inline]
    pub const fn from_u64(u: u64) -> Option<Self> {
        if u > (!0u64 >> 18) {
            None
        } else {
            Some(Self::tagged(TypeTag::Int, u))
        }
    }

    /// Creates a `Value` from a 64-bit signed integer, if it fits in the payload.
    ///
    /// The payload area can represent 47-bit signed integers using two's complement.
    /// Values outside this range return `None` and should be represented as floats.
    ///
    /// # Range  
    /// - Valid: `-0x4000_0000_0000..=0x3FFF_FFFF_FFFF` (47-bit signed)
    /// - Invalid: Values requiring more than 47 bits of precision
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

    /// Extracts the 64-bit float value, assuming this `Value` contains a float.
    ///
    /// # Safety
    /// This should only be called when `type_tag()` returns `TypeTag::Float`.
    #[inline]
    pub const fn f64(self) -> f64 {
        unsafe { self.f }
    }

    /// Extracts a signed integer from the NaN-packed payload.
    ///
    /// For tagged integer values, this method:
    /// 1. Takes the raw 64-bit representation
    /// 2. Shifts left by 17 bits to clear the type tag and NaN prefix
    /// 3. Shifts right by 17 bits with sign extension to get the original value
    ///
    /// This effectively extracts a 47-bit signed integer from the payload area.
    #[inline]
    pub const fn i64(self) -> i64 {
        (unsafe { self.i } << 17) >> 17
    }

    /// Extracts an unsigned integer from the NaN-packed payload.
    ///
    /// This masks out the upper 17 bits (NaN prefix + type tag) to extract
    /// the 47-bit payload as an unsigned integer.
    #[inline]
    pub const fn u64(self) -> u64 {
        (unsafe { self.u }) & (!0u64 >> 17)
    }

    /// Extracts the type tag from this value.
    ///
    /// This method implements the core type discrimination logic for NaN-packed values:
    ///
    /// 1. **Float detection**: If the upper 17 bits don't form a NaN pattern
    ///    (i.e., the value when shifted right by 47 bits is less than `!13`),
    ///    then this is a regular IEEE 754 float.
    ///
    /// 2. **Tagged value**: If it's in NaN space, bits 59-47 contain the type tag
    ///    which is extracted and converted to a `TypeTag` enum variant.
    ///
    /// # Bit Layout Analysis
    /// ```text
    /// For tagged values:    0xfff8_xxxx_xxxx_xxxx (where x = type tag + payload)
    /// After >> 47:          0x1fff1 + type_tag_bits
    /// Since type tags are >= !13 (0xfffffff2), this is always >= !13
    ///
    /// For regular floats:   Various patterns, but upper 17 bits < !13
    /// ```
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
