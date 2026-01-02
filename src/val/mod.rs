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
use std::ops::Deref;

mod func;
mod str;
mod tab;
mod udata;
mod upval;

use abfall::{GcPtr, Trace};

use crate::ast::Number;

pub use self::func::GcFunc;
pub use self::str::GcString;
pub use self::tab::GcTable;
pub use self::udata::GcUdata;
pub use self::upval::GcUpval;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[non_exhaustive]
#[repr(u8)]
pub enum TypeTag {
    Nil = !0,
    False = !1,
    True = !2,
    LightUData = !3,
    LightString = !4,
    String = !5,
    //Upval = !6,
    //Thread = !7,
    //Proto = !8,
    Func = !9,
    //Trace = !10,
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
    /// bytes
    bytes: [u8; 8],
}

impl Value {
    pub const NIL: Self = Self::from_primitive(TypeTag::Nil); // -1i64 or !0u64
    pub const FALSE: Self = Self::from_primitive(TypeTag::False);
    pub const TRUE: Self = Self::from_primitive(TypeTag::True);

    pub const NAN: Self = Self {
        u: 0xfff8_0000_0000_0000,
    };
    pub const INFTY: Self = Self {
        u: 0x7ff0_0000_0000_0000,
    };
    pub const MINUS_INFTY: Self = Self {
        u: 0xfff0_0000_0000_0000,
    };

    pub const EMPTY_STRING: Self = Self::from_tagged(TypeTag::LightString, 0);

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
    const fn from_tagged(tag: TypeTag, v: u64) -> Self {
        Self {
            u: 0xfff8_0000_0000_0000 | v | (tag as u8 as u64) << 47,
        }
    }

    #[inline]
    fn from_tagged_gc_val<T: Trace + ?Sized>(tag: TypeTag, ptr: GcPtr<T>) -> Self {
        let ptr = ptr.as_box_ptr();
        // SAFETY: transmute ptr to pointer (but in const)
        let addr = ptr.addr() as u64;
        debug_assert!(addr & 0x3 == 0, "GC pointers must be 4-byte aligned");
        let ptr_val = addr >> 2;
        debug_assert!(
            ptr_val <= (!0u64 >> 18),
            "Pointer value too large to fit in payload"
        );
        Self::from_tagged(tag, ptr_val)
    }

    /// UNSAFE: must ensure object is of type T and valid
    #[inline]
    const unsafe fn to_gc_ptr<T>(self) -> GcPtr<T> {
        let ptr_val = (self.u64() << 2) as *const _;
        unsafe { GcPtr::from_box_ptr_unchecked(ptr_val) }
    }

    /// Creates a primitive value (nil, true, false).
    #[inline]
    const fn from_primitive(tag: TypeTag) -> Self {
        Self::from_tagged(tag, 0x7fff_ffff_ffff)
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
        Self::from_tagged(TypeTag::Int, u as u64)
    }

    #[inline]
    pub const fn from_i32(i: i32) -> Self {
        Self::from_tagged(TypeTag::Int, (i as i64) as u64 & (!0u64 >> 17))
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
            Some(Self::from_tagged(TypeTag::Int, u))
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
            Some(Self::from_tagged(TypeTag::Int, i as u64 & (!0u64 >> 17)))
        }
    }

    #[inline]
    pub const fn from_u64_wrapped(u: u64) -> Self {
        Some(Self::from_tagged(TypeTag::Int, u & (!0u64 >> 17)))
    }

    #[inline]
    pub const fn from_i64_wrapped(i: i64) -> Self {
        Some(Self::from_tagged(TypeTag::Int, i as u64 & (!0u64 >> 17)))
    }

    /// Creates a `Value` from a light string. A Light string is a string that is
    /// stored directly in the payload area of the `Value`, rather than as a
    /// reference to a garbage-collected string object.
    /// It must be less than or equal to 5 bytes in length (40 bits) to fit in the payload.
    /// The length is stored in the highest byte of the payload (5 bits for length, 2 bits unused).
    #[inline]
    pub const fn from_light_string(s: &[u8]) -> Option<Self> {
        let len = s.len();
        if len > 5 {
            return None;
        }
        let mut payload = [0u8; 8];
        if cfg!(target_endian = "big") {
            payload[2] = len as u8;
            for i in 0..len {
                payload[3 + i] = s[i];
            }
        } else {
            for i in 0..len {
                payload[i] = s[i];
            }
            payload[6] = len as u8;
        }
        Some(Self::from_tagged(
            TypeTag::LightString,
            u64::from_ne_bytes(payload),
        ))
    }

    #[inline]
    pub const fn as_light_string(&self) -> Option<&[u8]> {
        if self.type_tag() != TypeTag::LightString {
            return None;
        }
        let bytes = unsafe {
            if cfg!(target_endian = "big") {
                let len = (self.bytes[2] & 0x7).min(5) as usize;
                &self.bytes[3..3 + len]
            } else {
                let len = (self.bytes[6] & 0x7).min(5) as usize;
                &self.bytes[0..len]
            }
        };
        Some(bytes)
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

    #[inline]
    pub const fn is_same(self, other: Self) -> bool {
        unsafe { self.u == other.u }
    }

    #[inline]
    pub fn is_raw_equal(self, other: Self) -> bool {
        match (self.type_tag(), other.type_tag()) {
            (TypeTag::Float, TypeTag::Float) => self.f64() == other.f64(),
            (TypeTag::Int, TypeTag::Float) => self.i64() as f64 == other.f64(),
            (TypeTag::Float, TypeTag::Int) => self.f64() == other.i64() as f64,
            _ => self.is_same(other),
        }
    }
}

impl PartialEq for Value {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.is_same(*other)
    }
}

impl Eq for Value {}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.type_tag() {
            TypeTag::Nil => write!(f, "nil"),
            TypeTag::False => write!(f, "false"),
            TypeTag::True => write!(f, "true"),
            TypeTag::Int => write!(f, "{}i", self.i64()),
            TypeTag::Float => write!(f, "{}f", self.f64()),
            tag => write!(f, "<{:?} {:x}>", tag, self.u64()),
        }
    }
}

/*
#[repr(u8)]
pub(crate) enum GcValue {
    String(GcString) = !4,
    Upval(GcUpval) = !5,
    //Thread = !6,
    //Proto = !7,
    Func(GcFunc) = !8,
    //Trace = !9,
    //Cdata = !10,
    Tab(GcTable) = !11,
    UData(GcUdata) = !12,
}

impl GcValue {
    pub fn type_tag(&self) -> TypeTag {
        match self {
            GcValue::String(_) => TypeTag::String,
            GcValue::Upval(_) => TypeTag::Upval,
            GcValue::Func(_) => TypeTag::Func,
            GcValue::Tab(_) => TypeTag::Tab,
            GcValue::UData(_) => TypeTag::UData,
        }
    }

    pub fn as_value(self) -> Value {
        match self {
            GcValue::String(s) => Value::from_tagged_gc(TypeTag::String, &**s),
            GcValue::Upval(u) => Value::from_tagged_gc(TypeTag::Upval, &**u),
            GcValue::Func(f) => Value::from_tagged_gc(TypeTag::Func, &**f),
            GcValue::Tab(t) => Value::from_tagged_gc(TypeTag::Tab, &**t),
            GcValue::UData(u) => Value::from_tagged_gc(TypeTag::UData, &**u),
        }
    }
}
*/

macro_rules! impl_raw_arith_op {
    ($name:ident, $op:tt) => {
        pub fn $name(self, other: Self) -> Option<Number> {
            let a = self.coerce_to_number()?;
            let b = other.coerce_to_number()?;
            match (a, b) {
                (Number::Float(a)  , Number::Float(b)  ) => Some(Number::Float(a $op b)),
                (Number::Integer(a), Number::Float(b)  ) => Some(Number::Float(a $op b)),
                (Number::Float(a)  , Number::Integer(b)) => Some(Number::Float(a $op b)),
                (Number::Integer(a), Number::Integer(b)) => Some(Number::Integer(a $op b)),
            }
        }
    };
}

macro_rules! impl_raw_bit_op {
    ($name:ident, $op:tt) => {
        pub fn $name(self, other: Self) -> Option<i64> {
            let a = self.coerce_to_integer()?;
            let b = other.coerce_to_integer()?;
            Some(a $ op b)
        }
    };
}

macro_rules! impl_try_meta_op {
    ($name:ident, $raw:ident, $meta:ident) => {
        pub fn $name(self, mut other: Self) -> Option<Value> {
            if let Some(res) = self.$raw(other) {
                return Some(res.into());
            }
            // TODO: implement meta method lookup
            // - check if a has meta method, and call it with (a, b)
            // - or check if b has meta method, and call it with (b, a)
            todo!("meta method lookup for {}", stringify!($meta));
        }
    };
}

macro_rules! impl_try_meta_unary_op {
    ($name:ident, $raw:ident, $meta:ident) => {
        pub fn $name(self) -> Option<Value> {
            if let Some(res) = self.$raw() {
                return Some(res.into());
            }
            // TODO: implement meta method lookup
            // - check if a has meta method, and call it with (a, b)
            // - or check if b has meta method, and call it with (b, a)
            todo!("meta method lookup for {}", stringify!($meta));
        }
    };
}

impl<'root> Rooted<'root, Value> {
    #[inline]
    pub const fn as_value(&self) -> Value {
        self.into_inner()
    }

    /// UNSAFE: must ensure object is of type T and valid
    #[inline]
    unsafe fn as_gc_val<T: Trace>(self) -> GcRef<'root, T> {
        Rooted::new_unchecked(self.as_value().to_gc_val())
    }

    #[inline]
    pub const fn as_string(&self) -> Option<&[u8]> {
        match self.type_tag() {
            TypeTag::LightString => self.as_light_string(),
            TypeTag::String => {
                let gc_val: GcRef<GcString> = unsafe { self.as_gc_val() };
                Some(gc_val.as_bytes())
            }
            _ => None,
        }
    }

    #[inline]
    pub fn raw_len(self) -> usize {
        match self.type_tag() {
            TypeTag::LightString | TypeTag::String => self.as_string().map_or(0, |s| s.len()),
            TypeTag::Tab => {
                let gc_val: GcRef<GcTable> = unsafe { self.as_gc_val() };
                gc_val.len()
            }
            TypeTag::UData => {
                let gc_val: GcRef<GcUdata> = unsafe { self.as_gc_val() };
                gc_val.len()
            }
            _ => 0,
        }
    }

    #[inline]
    fn coerce_to_number(self) -> Option<Number> {
        match self.as_value().type_tag() {
            TypeTag::String | TypeTag::LightString => {
                self.as_string().and_then(Number::coerce_from_string)
            }
            TypeTag::Float => Some(Number::Float(self.as_value().f64())),
            TypeTag::Int => Some(Number::Integer(self.as_value().i64())),
            _ => None,
        }
    }

    #[inline]
    fn coerce_to_integer(self) -> Option<i64> {
        match self.coerce_to_number()? {
            Number::Integer(i) => Some(i),
            Number::Float(f) => Some(f as i64),
        }
    }

    #[inline]
    fn coerce_to_float(self) -> Option<f64> {
        match self.coerce_to_number()? {
            Number::Integer(i) => Some(i as f64),
            Number::Float(f) => Some(f),
        }
    }

    impl_raw_arith_op!(raw_add, +);
    impl_raw_arith_op!(raw_sub, -);
    impl_raw_arith_op!(raw_mul, *);
    impl_raw_arith_op!(raw_mod, %);
    impl_raw_bit_op!(raw_idiv, /); // integer division
    impl_raw_bit_op!(raw_bit_and, &);
    impl_raw_bit_op!(raw_bit_or, |);
    impl_raw_bit_op!(raw_bit_xor, ^);
    impl_raw_bit_op!(raw_bit_shl, <<);
    impl_raw_bit_op!(raw_bit_shr, >>);

    pub fn raw_div(self, other: Self) -> Option<f64> {
        let a = self.coerce_to_float()?;
        let b = other.coerce_to_float()?;
        Some(a / b)
    }

    pub fn raw_pow(&self, other: Self) -> Option<f64> {
        let a = self.coerce_to_float()?;
        let b = other.coerce_to_float()?;
        Some(a.powf(b))
    }

    pub fn raw_minus(self) -> Option<Number> {
        let a = self.coerce_to_number()?;
        match a {
            Number::Float(f) => Some(Number::Float(-f)),
            Number::Integer(i) => Some(Number::Integer(-i)),
        }
    }

    pub fn raw_bit_not(&self) -> Option<i64> {
        let a = self.coerce_to_integer()?;
        Some(!a)
    }

    impl_try_meta_op!(meta_add, raw_add, __add);
    impl_try_meta_op!(meta_sub, raw_sub, __sub);
    impl_try_meta_op!(meta_mul, raw_mul, __mul);
    impl_try_meta_op!(meta_div, raw_div, __div);
    impl_try_meta_op!(meta_idiv, raw_idiv, __idiv);
    impl_try_meta_op!(meta_mod, raw_mod, __mod);
    impl_try_meta_op!(meta_pow, raw_pow, __pow);
    impl_try_meta_op!(meta_bit_and, raw_bit_and, __band);
    impl_try_meta_op!(meta_bit_or, raw_bit_or, __bor);
    impl_try_meta_op!(meta_bit_xor, raw_bit_xor, __bxor);
    impl_try_meta_op!(meta_bit_shl, raw_bit_shl, __shl);
    impl_try_meta_op!(meta_bit_shr, raw_bit_shr, __shr);

    impl_try_meta_unary_op!(meta_minus, raw_minus, __unm);
    impl_try_meta_unary_op!(meta_bit_not, raw_bit_not, __bnot);
    impl_try_meta_unary_op!(meta_len, raw_len, __len);

    pub fn is_raw_lt(self, other: Value) -> Option<bool> {
        let a = self.coerce_to_number()?;
        let b = other.coerce_to_number()?;
        match (a, b) {
            (Number::Float(af), Number::Float(bf)) => Some(af < bf),
            (Number::Integer(ai), Number::Integer(bi)) => Some(ai < bi),
            (Number::Integer(ai), Number::Float(bf)) => Some((ai as f64) < bf),
            (Number::Float(af), Number::Integer(bi)) => Some(af < (bi as f64)),
        }
    }

    pub fn is_raw_le(self, other: Value) -> Option<bool> {
        let a = self.coerce_to_number()?;
        let b = other.coerce_to_number()?;
        match (a, b) {
            (Number::Float(af), Number::Float(bf)) => Some(af <= bf),
            (Number::Integer(ai), Number::Integer(bi)) => Some(ai <= bi),
            (Number::Integer(ai), Number::Float(bf)) => Some((ai as f64) <= bf),
            (Number::Float(af), Number::Integer(bi)) => Some(af <= (bi as f64)),
        }
    }

    pub fn is_meta_equal(self, other: Self) -> bool {
        if self.is_raw_equal(other) {
            return true;
        }
        // TODO: implement meta method lookup
        // __eq: the equal (==) operation. Behavior similar to the addition operation, except that Lua will try a metamethod only when the values being compared are either both tables or both full userdata and they are not primitively equal. The result of the call is always converted to a boolean.
        todo!("meta method lookup for __eq");
    }

    pub fn is_meta_lt(self, other: Self) -> Option<bool> {
        if let Some(res) = self.is_raw_lt(other) {
            return Some(res);
        }
        // TODO: implement meta method lookup
        // __lt: the less than (<) operation. Behavior similar to the addition operation, except that Lua will try a metamethod only when the values being compared are neither both numbers nor both strings. The result of the call is always converted to a boolean.
        todo!("meta method lookup for __lt");
    }

    pub fn is_meta_le(self, other: Value) -> Option<bool> {
        if let Some(res) = self.is_raw_le(other) {
            return Some(res);
        }
        // TODO: implement meta method lookup
        // __le: the less equal (<=) operation. Unlike other operations, the less-equal operation can use two different events. First, Lua looks for the __le metamethod in both operands, like in the less than operation. If it cannot find such a metamethod, then it will try the __lt metamethod, assuming that a <= b is equivalent to not (b < a). As with the other comparison operators, the result is always a boolean. (This use of the __lt event can be removed in future versions; it is also slower than a real __le metamethod.)
        todo!("meta method lookup for __le");
    }
}

impl<'root> Deref for Rooted<'root, Value> {
    type Target = Value;
    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl From<i64> for Value {
    #[inline]
    fn from(i: i64) -> Self {
        Value::from_i64_wrapped(i)
    }
}

impl From<u64> for Value {
    #[inline]
    fn from(i: u64) -> Self {
        Value::from_u64_wrapped(i as i64)
    }
}

impl From<i32> for Value {
    #[inline]
    fn from(i: i32) -> Self {
        Value::from_i32(i)
    }
}

impl From<u32> for Value {
    #[inline]
    fn from(i: u32) -> Self {
        Value::from_u32(i)
    }
}

impl From<f64> for Value {
    #[inline]
    fn from(f: f64) -> Self {
        Value::from_f64(f)
    }
}

impl From<f32> for Value {
    #[inline]
    fn from(f: f32) -> Self {
        Value::from_f64(f as f64)
    }
}

impl From<Number> for Value {
    #[inline]
    fn from(n: Number) -> Self {
        match n {
            Number::Integer(i) => Value::from_i64_wrapped(i),
            Number::Float(f) => Value::from_f64(f),
        }
    }
}

impl From<bool> for Value {
    #[inline]
    fn from(b: bool) -> Self {
        Value::from_bool(b)
    }
}

impl From<()> for Value {
    #[inline]
    fn from(_: ()) -> Self {
        Value::nil()
    }
}

impl<T: Into<Value>> From<Option<T>> for Value {
    #[inline]
    fn from(v: Option<T>) -> Self {
        if let Some(v) = v {
            v.into()
        } else {
            Value::nil()
        }
    }
}
