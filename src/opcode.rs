use std::fmt;

macro_rules! __mkop {
    (is_a_dst_impl [dst $(, $x:ident)*]) => (true);
    (is_a_dst_impl [$($x:ident),*]) => (false);

    (is_ad_impl [$d:ident,]) => (true);
    (is_ad_impl [$a:ident, $d:ident]) => (true);
    (is_ad_impl [$($x:ident),*]) => (false);

    (is_base_and_num_impl [base, $x:ident, num]) => (true);
    (is_base_and_num_impl [base, num]) => (true);
    (is_base_and_num_impl [$($x:ident),*]) => (false);

    // Argument modes:
    // - dst: variable slot number, used as a destination
    (typeitem_d dst) => { u8 };
    // - uvdst: upvalue number, used as a destination
    (typeitem_d uvdst) => { u8 };
    // - var: variable slot number
    (typeitem_d var) => { u8 };
    // - base: base slot number, read-write
    (typeitem_d base) => { u8 };
    // - rbase: base slot number, read-only
    (typeitem_d rbase) => { u8 };
    // - uv: upvalue number
    (typeitem_d uv) => { u8 };
    // - str: string constant, negated index into constant table
    (typeitem_d str) => { u16 };
    // // - tab: template table, negated index into constant table
    // (typeitem tab) => { }; // TODO
    // - num: number constant, index into constant table
    (typeitem_d num) => { u16 };
    // // - cdata: cdata constant, negated index into constant table
    // (typeitem cdata) => { }; // TODO
    // - lit: literal
    (typeitem_d lit) => { u16 };
    // - lits: signed literal
    (typeitem_d lits) => { i16 };
    // - pri: primitive type (0 = nil, 1 = false, 2 = true)
    (typeitem_d pri) => { u8 };
    // - jump: branch target, relative to next instruction, biased with 0x8000
    (typeitem_d jump) => { i16 };
    // - func: function prototype, negated index into constant table
    (typeitem_d func) => { u16 };

    // Special handling of argument types, when used in abc positions:
    (typeitem_abc str) => { u8 };
    (typeitem_abc num) => { u8 };
    (typeitem_abc lit) => { u8 };
    (typeitem_abc lits) => { i8 };
    (typeitem_abc jump) => { compile_error!("`jump` not supported in positions a, b or c"); };
    (typeitem_abc func) => { compile_error!("`func` not supported in positions a, b or c"); };
    (typeitem_abc $fallback:ident) => { __mkop!(typeitem_d $fallback) };


    (tupletype ($d:ident)) => ( (__mkop!(typeitem_d $d),) );
    (tupletype ($a:ident, $d:ident)) => ( (__mkop!(typeitem_abc $a), __mkop!(typeitem_d $d),) );
    (tupletype ($a:ident, $b:ident, $c:ident)) => ( (__mkop!(typeitem_abc $a), __mkop!(typeitem_abc $b), __mkop!(typeitem_abc $c),) );

    (intotuple ($d:ident) = $args:expr) => ({
      let args: &Args = $args;
      (args.d() as _,)
    });
    (intotuple ($a:ident, $d:ident) = $args:expr) => ({
      let args: &Args = $args;
      (args.a() as _, args.d() as _)
    });
    (intotuple ($a:ident, $b:ident, $c:ident) = $args:expr) => ({
      let args: &Args = $args;
      (args.a() as _, args.b() as _, args.c() as _)
    });

    (fromtuple ($d:ident)) => (Args::new_ad(0, $d as _));
    (fromtuple ($a:ident, $d:ident)) => (Args::new_ad($a as _, $d as _));
    (fromtuple ($a:ident, $b:ident, $c:ident)) => (Args::new_abc($a as _, $b as _, $c as _));
}

macro_rules! define_opcodes {
    {
        $vm:ident =>

        $($id:ident ($($arg:ident : $mode:ident),*) {$($body:tt)*} ),* $(,)?
    } => {
        #[repr(u8)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum OpCode {
            $(
                #[doc = concat!("Signature: `", stringify!($id ($($arg : $mode),*)), "`")]
                ///
                /// This opcode is equivalent to the following Rust code:
                /// ```rust
                #[doc = stringify!($($body)*)]
                /// ```
                $id
            ),*
        }

        impl OpCode {
            pub const MAX: u8 = {
                let mut v = 0;
                const fn max(a: u8, b: u8) -> u8 {
                    if a > b { a } else { b }
                }
                $(v = max(v, Self::$id as u8);)*
                v
            };
            pub const MIN: u8 = {
                let mut v = u8::MAX;
                const fn min(a: u8, b: u8) -> u8 {
                    if a < b { a } else { b }
                }
                $(v = min(v, Self::$id as u8);)*
                v
            };
        }

        $(
          impl OpArgs<{OpCode::$id as u8}> for Args {
              type Args = __mkop!(tupletype ($($mode),*));
              fn get(&self) -> Self::Args {
                  __mkop!(intotuple ($($arg),*) = self)
              }
              fn from(($($arg,)*): Self::Args) -> Self {
                  __mkop!(fromtuple ($($arg),*))
              }
          }
        )*

        impl OpCode {
          #[inline]
          pub const fn is_a_dst(&self) -> bool {
              match self {
                  $(
                      OpCode::$id => __mkop!(is_a_dst_impl [$($mode),*]),
                  )*
              }
          }

          #[inline]
          pub const fn is_base_and_num(&self) -> bool {
              match self {
                  $(
                      OpCode::$id => __mkop!(is_base_and_num_impl [$($mode),*]),
                  )*
              }
          }

          #[inline]
          pub const fn is_ad(&self) -> bool {
              match self {
                  $(
                      OpCode::$id => __mkop!(is_ad_impl [$($mode),*]),
                  )*
              }
          }
        }

        #[cfg(false)]
        impl Op {
            fn exec(&self, $vm: &mut VM) {
                match_op!((self) {
                    $(
                        $id ($($arg),*) => {
                            $($body)*
                        },
                    )*
                });
            }
        }

        impl fmt::Debug for Op {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.0 {
                    $(
                        OpCode::$id => {
                            let tup = OpArgs::<{OpCode::$id as u8}>::get(&self.1);
                            write!(f, "{}{:?}", stringify!($id), tup)
                        },
                    )*
                }
            }
        }

    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct Args(u8, u8, u8);

impl Args {
    pub const ZERO: Self = Self(0, 0, 0);

    #[inline]
    pub const fn new_abc(a: u8, b: u8, c: u8) -> Self {
        Self(a, b, c)
    }

    #[inline]
    pub const fn new_ad(a: u8, d: u16) -> Self {
        let [b, c] = d.to_le_bytes();
        Self(a, b, c)
    }

    pub const fn a(&self) -> u8 {
        self.0
    }
    #[inline]
    pub const fn b(&self) -> u8 {
        self.1
    }
    #[inline]
    pub const fn c(&self) -> u8 {
        self.2
    }
    #[inline]
    pub const fn d(&self) -> u16 {
        u16::from_le_bytes([self.1, self.2])
    }
    #[inline]
    pub const fn set_a(&mut self, a: u8) {
        self.0 = a;
    }
    #[inline]
    pub const fn set_b(&mut self, b: u8) {
        self.1 = b;
    }
    #[inline]
    pub const fn set_c(&mut self, c: u8) {
        self.2 = c;
    }
    #[inline]
    pub const fn set_d(&mut self, d: u16) {
        let [b, c] = d.to_le_bytes();
        self.1 = b;
        self.2 = c;
    }
}

#[allow(unused)]
pub trait OpArgs<const OP: u8> {
    type Args: Copy;
    fn get(&self) -> Self::Args;
    fn from(tuple: Self::Args) -> Self;

    fn set(&mut self, args: Self::Args)
    where
        Self: Sized,
    {
        *self = Self::from(args);
    }

    fn get_view_mut(&mut self) -> ArgsViewMut<'_, Self, OP>
    where
        Self: Sized,
    {
        ArgsViewMut::new(self)
    }
}

pub struct ArgsViewMut<'a, Args, const OP: u8>
where
    Args: OpArgs<OP>,
{
    args: &'a mut Args,
    pub mapped: <Args as OpArgs<OP>>::Args,
}

impl<'a, Args, const OP: u8> ArgsViewMut<'a, Args, OP>
where
    Args: OpArgs<OP>,
{
    pub fn new(args: &'a mut Args) -> Self {
        let mapped = args.get();
        Self { args, mapped }
    }
}

impl<'a, Args, const OP: u8> Drop for ArgsViewMut<'a, Args, OP>
where
    Args: OpArgs<OP>,
{
    fn drop(&mut self) {
        self.args.set(self.mapped);
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct Op(pub OpCode, pub Args);

const _: () = {
    assert!(std::mem::size_of::<Op>() == 4);
    assert!(std::mem::align_of::<Op>() == 1);
};

/// Macro to match an [`Op`] and destructure its arguments.
///
/// # Examples
/// ```rust
/// # use moonlift::opcode::{op, match_op};
/// let mut op = op!(AddVV(1, 2, 3));
/// match_op!{(&mut op) {
///   AddVV (a, ref mut b, c) => {
///     assert_eq!(a, 1);
///     assert_eq!(*b, 2);
///     assert_eq!(c, 3);
///     // modify b
///     *b += 10;
/// #   assert_eq!(*b, 12);
///   },
///   SuvVV => {
///     // Simple check for OpCode only
///   },
///   _ => panic!("unexpected opcode"),
/// }};
/// match_op!{(op) {
///   AddVV (a, b, c) => {
///     assert_eq!(a, 1);
///     assert_eq!(b, 12);
///     assert_eq!(c, 3);
///   },
///   _ => panic!("unexpected opcode"),
/// }};
/// # assert_eq!(op, op!(AddVV(1, 12, 3)));
/// //Simple check if an Op is of a certain OpCode
/// assert!(match_op!(op, AddVV));
/// ```
macro_rules! match_op {
    (#getargs($src:expr ; $code:ident)) => {
      {
        let src: &crate::opcode::Args = &$src;
        $crate::opcode::OpArgs::<{$crate::opcode::OpCode::$code as u8}>::get(src)
      }
    };
    (#getargsviewmut($src:expr ; $code:ident)) => {
      {
        let src: &mut crate::opcode::Args = $src;
        $crate::opcode::OpArgs::<{$crate::opcode::OpCode::$code as u8}>::get_view_mut(src)
      }
    };
    (#ifrefmut ()                                 {$($ismut:tt)*} $(else {$($isnonmut:tt)*})?) => {
      $( $($isnonmut)* )?
    };
    (#ifrefmut (ref mut $cur:pat, $($rest:tt)*) {$($ismut:tt)*} $(else {$($isnonmut:tt)*})?) => {
      $($ismut)*
    };
    (#ifrefmut (ref $cur:pat, $($rest:tt)*)     {$($ismut:tt)*} $(else {$($isnonmut:tt)*})?) => {
      match_op!(#ifrefmut ($($rest)*) { $($ismut)* });
    };
    (#ifrefmut ($cur:pat, $($rest:tt)*)         {$($ismut:tt)*} $(else {$($isnonmut:tt)*})?) => {
      match_op!(#ifrefmut ($($rest)*) { $($ismut)* } $(else {$($isnonmut)*})?);
    };

    (op:expr, $opcode:ident) => {
        match op.0 {
            $crate::opcode::OpCode::$opcode => true,
            _ => false,
        }
    };

    (
      ($op:expr) {
        $($code:ident $(($($args:tt)*))? => $body:expr),*

        $(, _ => $else:expr)?

        $(,)?
      }
    ) => {
        match $op {
            $(
              $crate::opcode::Op($crate::opcode::OpCode::$code, _args) => {
                $(
                    match_op!(#ifrefmut ($($args)*,) {
                        let mut _mut_view = match_op!(#getargsviewmut(_args ; $code));
                        let ($($args)*) = _mut_view.mapped;
                    } else {
                        let _tup = match_op!(#getargs(_args ; $code));
                        let ($($args)*) = _tup;
                    });
                )?
                { {$body} }
              },
            )*

            $(_ => $else,)?
        }
    };
}

/// Macro to create an [`Op`] from an [`OpCode`] with arguments.
///
/// # Examples
/// ```rust,no_run
/// # use moonlift::opcode::op;
/// op![Mov(0, 1)] // OpCode::Mov with Args { a: 0, d: 1 }
/// op![IsTC(0, 1)] // OpCode::IsTC with Args { a: 0, d: 1 }
/// op![AddV(0, 1, 2)] // OpCode::AddV with Args { a: 0, b: 1, c: 2 }
/// ```
macro_rules! op {
    (#setargs $arg:expr; $src:expr; $code:ident) => {
      $crate::opcode::OpArgs::<{$crate::opcode::OpCode::$code as u8}>::set($src, $arg);
    };
    ($code:ident) => {
      $crate::opcode::Op($crate::opcode::OpCode::$code, $crate::opcode::Args::ZERO)
    };
    ($code:ident ($($args:expr),* $(,)?)) => {
      {
        let mut args = $crate::opcode::Args::ZERO;
        op!(#setargs ($($args,)*); &mut args; $code);
        $crate::opcode::Op($crate::opcode::OpCode::$code, args)
      }
    };
}

#[cfg(test)]
#[test]
fn test_match_op() {
    let mut op = op!(AddVV(1, 2, 3));
    match_op! {(&mut op) {
        AddVV (a, ref mut b, c) => {
            assert_eq!(a, 1);
            assert_eq!(*b, 2);
            assert_eq!(c, 3);
            // modify b
            *b += 10;

            assert_eq!(*b, 12);
        },
        _ => panic!("unexpected opcode"),
    }};
    match_op! {(op) {
        AddVV(a, b, c) => {
            assert_eq!(a, 1);
            assert_eq!(b, 12);
            assert_eq!(c, 3);
        },
        _ => panic!("unexpected opcode"),
    }};
    assert_eq!(op, op!(AddVV(1, 12, 3)));
}

struct VM {
    pc: usize,
}
type Value = u32;
impl VM {
    fn get(&self, reg: u8) -> Value {
        todo!()
    }
    fn set(&mut self, var: u8, val: Value) {
        todo!()
    }
    fn get_uv(&self, uv: u8) -> Value {
        todo!()
    }
    fn set_uv(&mut self, uv: u8, val: u32) {
        todo!()
    }
    fn get_table(&self, var: u8, key: Value) -> Value {
        todo!()
    }
    fn set_table(&mut self, var: u8, key: Value, val: Value) {
        todo!()
    }
    fn get_const(&self, k: u16) -> Value {
        todo!()
    }
    fn get_func(&self, p: u16) -> Value {
        todo!()
    }
}

// The suffix(es) of the instruction name distinguish variants of the same basic instruction:
//
// - V variable slot
// - S string constant
// - N number constant
// - P primitive type
// - B unsigned byte literal
// - M multiple arguments/results

// Here are the possible operand types:
//
// - (none): unused operand
// - var: variable slot number
// - dst: variable slot number, used as a destination
// - base: base slot number, read-write
// - rbase: base slot number, read-only
// - uv: upvalue number
// - uvdst: upvalue number, used as a destination
// - lit: literal
// - lits: signed literal
// - pri: primitive type (0 = nil, 1 = false, 2 = true)
// - num: number constant, index into constant table
// - str: string constant, negated index into constant table
// - tab: template table, negated index into constant table
// - func: function prototype, negated index into constant table
// - cdata: cdata constant, negated index into constant table
// - jump: branch target, relative to next instruction, biased with 0x8000

define_opcodes! {
    vm =>

    // Jump if A < D
    IsLt(a: var, d: var) {
        if vm.get(a) >= vm.get(d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≥ D
    IsGe(a: var, d: var) {
        if vm.get(a) < vm.get(d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≤ D
    IsLe(a: var, d: var) {
        if vm.get(a) > vm.get(d) {
            vm.pc += 1;
        }
    },
    // Jump if A > D
    IsGt(a: var, d: var) {
        if vm.get(a) <= vm.get(d) {
            vm.pc += 1;
        }
    },
    // Jump if A = D
    IsEqV(a: var, d: var) {
        if vm.get(a) != vm.get(d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≠ D
    IsNeV(a: var, d: var) {
        if vm.get(a) == vm.get(d) {
            vm.pc += 1;
        }
    },
    /*
    // Jump if A = D
    IsEqS(a: var, d: str) {
        if vm.get(a) != vm.get_const(d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≠ D
    IsNeS(a: var, d: str) {
        if vm.get(a) == vm.get_const(d) {
            vm.pc += 1;
        }
    },
    // Jump if A = D
    IsEqN(a: var, d: num) {
        if vm.get(a) != Value::from_num(d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≠ D
    IsNeN(a: var, d: num) {
        if vm.get(a) == Value::from_num(d) {
            vm.pc += 1;
        }
    },
    // Jump if A = D
    IsEqP(a: var, d: pri) {
        if vm.get(a) != Value::from_pri(d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≠ D
    IsNeP(a: var, d: pri) {
        if vm.get(a) == Value::from_pri(d) {
            vm.pc += 1;
        }
    },
    */

    // Copy D to A and jump, if D is true
    IsTC(a: dst, d: var) {
        // TODO
        let a = todo!();
    },
    // Copy D to A and jump, if D is false
    IsFC(a: dst, d: var) {
        // TODO
        let a = todo!();
    },
    // Jump if D is true
    IsT(a: var) {
        // TODO
        todo!();
    },
    // Jump if D is false
    IsF(a: var) {
        // TODO
        todo!();
    },

    // Copy D to A
    Mov(a: dst, d: var) {
        vm.set(a, vm.get(d));
    },
    // Set A to boolean not of D
    Not(a: dst, d: var) {
        vm.set(a, !vm.get(d));
    },
    // Set A to -D (unary minus)
    UNM(a: dst, d: var) {
        // TODO
        todo!();
    },
    // Set A to #D (object length)
    Len(a: dst, d: var) {
        // TODO
        todo!();
    },

    /*
    // A = B + C
    AddVN(a: dst, b: var, c: num) {
        let a = b + c;
    },
    // A = B - C
    SubVN(a: dst, b: var, c: num) {
        let a = b - c;
    },
    // A = B * C
    MulVN(a: dst, b: var, c: num) {
        let a = b * c;
    },
    // A = B / C
    DivVN(a: dst, b: var, c: num) {
        let a = b / c;
    },
    // A = B % C
    ModVN(a: dst, b: var, c: num) {
      let a = b % c;
    },

    //A = C + B
    AddNV(a: dst, b: var, c: num) {
        let a = c + b;
    },
    // A = C - B
    SubNV(a: dst, b: var, c: num) {
        let a = c - b;
    },
    // A = C * B
    MulNV(a: dst, b: var, c: num) {
        let a = c * b;
    },
    // A = C / B
    DivNV(a: dst, b: var, c: num) {
        let a = c / b;
    },
    // A = C % B
    ModNV(a: dst, b: var, c: num) {
      let a = c % b;
    },
    */

    // A = B + C
    AddVV(a: dst, b: var, c: var) {
      let a = b + c;
    },
    // A = B - C
    SubVV(a: dst, b: var, c: var) {
      let a = b - c;
    },
    // A = B * C
    MulVV(a: dst, b: var, c: var) {
      let a = b * c;
    },
    // A = B / C
    DivVV(a: dst, b: var, c: var) {
      let a = b / c;
    },
    // A = B // C (integer division)
    IDivVV(a: dst, b: var, c: var) {
      let a = b / c;
    },
    // A = B % C
    ModVV(a: dst, b: var, c: var) {
      let a = b % c;
    },

    // Set A bitwise not of D
    BNot(a: dst, d: var) {
        let a = !d;
    },
    // A = B & C (bit and)
    BAndVV(a: dst, b: var, c: var) {
      let a = b & c;
    },
    // A = B | C (bit or)
    BOrVV(a: dst, b: var, c: var) {
      let a = b & c;
    },
    // A = B ~ C (bit or)
    BXorVV(a: dst, b: var, c: var) {
      let a = b ^ c;
    },
    // A = B << C (shift left)
    ShLVV(a: dst, b: var, c: var) {
      let a = b << c;
    },
    // A = B >> C (shift right)
    ShRVV(a: dst, b: var, c: var) {
      let a = b >> c;
    },

    // A = B ^ C
    Pow(a: dst, b: var, c: var) {
      let a = b.pow(c);
    },
    // A = B .. ~ .. C
    Cat(a: dst, b: rbase, c: rbase) {
      // TODO
      let a = todo!();
    },

    // Set A to string constant D
    KStr(a: dst, d: str) {
      let a = d;
    },
    // // Set A to cdata constant D
    // KCData(a: dst, d: cdata) {
    //   // TODO
    //   let a = todo!();
    // },
    // Set A to 16 bit signed integer D
    KShort(a: dst, d: lits) {
      let a = d;
    },
    // Set A to number constant D
    KNum(a: dst, d: num) {
      let a = d;
    },
    // Set A to B to primitive C
    // switch (C) {
    //     case 0: A..B = nil;
    //     case 1: A..B = false;
    //     case 2: A..B = true;
    // }
    KPri(a: base, b: base, c: pri) {
        let val = Value::from_pri(c);
        for i in a..=b {
            vm.set(i, val);
        }
    },

    // Set A to upvalue D
    UGet(a: dst, d: uv) {
      let a = d;
    },
    // Set upvalue A to D
    USetV(a: uvdst, d: var) {
      let a = d;
    },
    // // Set upvalue A to string constant D
    // USetS(a: uvdst, d: str) {
    //   let a = d;
    // },
    // // Set upvalue A to number constant D
    // USetN(a: uvdst, d: num) {
    //   let a = d;
    // },
    // // Set upvalue A to 16bit signed  integer D
    // USetB(a: uvdst, d: lit) {
    //   let a = d;
    // },
    // // Set upvalue A to primitive D
    // USetP(a: uvdst, d: pri) {
    //   let a = d;
    // },
    // Close upvalues for slots >= rbase and jump to target D
    UClo(a: rbase, d: jump) {
      // TODO
      todo!();
    },
    // Create new closure from prototype D and store it in A
    FNew(a: dst, d: func) {
      // TODO
      let a = todo!();
    },

    // Set A to new table with size D
    TNew(a: dst, d: lit) {
      // TODO
      let a = todo!();
    },
    // // Set A to duplicated template table D
    // TDup(a: dst, d: tab) {
    //   // TODO
    //   let a = todo!();
    // },
    // A = G[D] (global get)
    GGet(a: dst, d: str) {
      // TODO
      let a = todo!();
    },
    // G[D] = A (global set)
    GSet(a: var, d: str) {
      // TODO
      todo!();
    },
    // A = B[C]
    TGetV(a: dst, b: var, c: var) {
      // TODO
      let a = todo!();
    },
    // A = B[C]
    TGetS(a: dst, b: var, c: str) {
      // TODO
      let a = todo!();
    },
    // A = B[C]
    TGetB(a: dst, b: var, c: lit) {
      // TODO
      let a = todo!();
    },
    // B[C] = A
    TSetV(a: var, b: var, c: var) {
      // TODO
      todo!();
    },
    // B[C] = A
    TSetS(a: var, b: var, c: str) {
      // TODO
      todo!();
    },
    // B[C] = A
    TSetB(a: var, b: var, c: lit) {
      // TODO
      todo!();
    },
    // (A-1)[D], (A-1)[D+1], ... = A, A+1, ...
    TSetM(a: base, d: num) {
      // TODO
      todo!();
    },

    // // Call: A, ..., A+B-2 = A(A+1, ..., A+C+MULTRES)
    // CallM(a: base, b: lit, c: lit) {
    //   // TODO
    //     todo!();
    // },
    // Call: A, ..., A+B-2 = A(A+1, ..., A+C-1)
    Call(a: base, b: lit, c: lit) {
      // TODO
        todo!();
    },
    // // Tail-Call: `return` A(A+1, ..., A+D+MULTRES)
    // CallMT(a: base, d: lit) {
    //   // TODO
    //     todo!();
    // },
    // Tail-Call: return A(A+1, ..., A+D-1)
    CallT(a: base, d: lit) {
      // TODO
        todo!();
    },

    // // Return A, \..., A+D+MULTRES-1
    // RetM(a: base, d: lit) {
    //   // TODO
    //     todo!();
    // },
    // Return A, ..., A+D-2
    Ret(a: base, d: lit) {
      // TODO
        todo!();
    },
    // Return
    Ret0(a: rbase, d: lit) {
      // TODO
      todo!();
    },
    // Return A
    Ret1(a: rbase, d: lit) {
      // TODO
        todo!();
    },

    // Numeric 'for' loop - init
    // check values and prepare counters; if not to run then pc+=D+1
    ForI(a: base, d: jump) {
        let i = vm.get(a);
        let limit = vm.get(a + 1);
        let step = vm.get(a + 2);
        let cond = if (step >= 0) {
            i > limit
        } else {
            i < limit
        }
        if cond {
            vm.pc += (d as isize + 1) as usize;
        } else {
            vm.set(a + 3, i);
        }
    },
    // Numeric 'for' loop - loop
    // update counters; if loop continues then pc-=D;
    ForL(a: base, d: jump) {
        let mut i = vm.get(a);
        let limit = vm.get(a + 1);
        let step = vm.get(a + 2);
        i += step;
        vm.set(a, i);
        let cond = if (step >= 0) {
            i > limit
        } else {
            i < limit
        }
        if cond {
            vm.pc += (d as isize) as usize;
        } else {
            vm.set(a + 3, i);
        }
    },
    // Call iterator: A, A+1, A+2 = A-3, A-2, A-1; A, ..., A+B-2 = A(A+1, A+2)
    IterC(a: base, b: lit, c: lit) {
        vm.set(a, vm.get(a - 3));
        vm.set(a + 1, vm.get(a - 2));
        vm.set(a + 2, vm.get(a - 1));
        // TODO: call(base=a, nargs=2, nret=b+2)
        todo!();
    },
    // Iterator loop jump: if(A != nil) (A-1) = A; else pc+=D;
    IterL(a: base, d: jump) {
        let val = vm.get(a);
        if val.is_nil() {
            vm.pc += (d as isize) as usize;
        } else {
            vm.set(a - 1, val);
        }
    },
    // Jump
    Jmp(a: rbase, d: jump) {
        vm.pc += (d as isize) as usize;
    },

    // Vararg: A, ..., A+B-2 = ...
    VArg(a: base, b: lit, c: lit) {
      todo!();
    },
}

impl OpCode {
    pub fn is_cond(self) -> bool {
        matches!(
            self,
            Self::IsLt |
        Self::IsGe |
        Self::IsLe |
        Self::IsGt |
        Self::IsEqV |
        Self::IsNeV |
        // Self::IsEqS |
        // Self::IsNeS |
        // Self::IsEqN |
        // Self::IsNeN |
        // Self::IsEqP |
        // Self::IsNeP |
        Self::IsTC |
        Self::IsFC |
        Self::IsT |
        Self::IsF
        )
    }

    pub fn negated(self) -> Option<Self> {
        match self {
            Self::IsLt => Some(Self::IsGe),
            Self::IsGe => Some(Self::IsLt),
            Self::IsLe => Some(Self::IsGt),
            Self::IsGt => Some(Self::IsLe),
            Self::IsEqV => Some(Self::IsNeV),
            Self::IsNeV => Some(Self::IsEqV),
            // Self::IsEqS => Some(Self::IsNeS),
            // Self::IsNeS => Some(Self::IsEqS),
            // Self::IsEqN => Some(Self::IsNeN),
            // Self::IsNeN => Some(Self::IsEqN),
            // Self::IsEqP => Some(Self::IsNeP),
            // Self::IsNeP => Some(Self::IsEqP),
            Self::IsTC => Some(Self::IsF),
            Self::IsFC => Some(Self::IsT),
            Self::IsT => Some(Self::IsF),
            Self::IsF => Some(Self::IsT),
            _ => None,
        }
    }
}

impl Op {
    #[inline]
    pub const fn opcode(&self) -> OpCode {
        self.0
    }

    #[inline]
    pub const fn set_opcode(&mut self, new_opcode: OpCode) {
        self.0 = new_opcode;
    }

    #[inline]
    pub const fn args(&self) -> Args {
        self.1
    }

    #[inline]
    pub const fn args_mut(&mut self) -> &mut Args {
        &mut self.1
    }

    #[inline]
    pub const fn set_a_dst(
        &mut self,
        dst: u8,
    ) -> Result<(), crate::codegen_state::CodeGenerationError> {
        if !self.opcode().is_a_dst() {
            return Err(crate::codegen_state::CodeGenerationError::InvalidOpCode(
                self.opcode(),
            ));
        }
        self.1.set_a(dst);
        Ok(())
    }

    #[inline]
    pub const fn set_base_num(
        &mut self,
        num: u8,
    ) -> Result<u8, crate::codegen_state::CodeGenerationError> {
        if !self.opcode().is_base_and_num() {
            return Err(crate::codegen_state::CodeGenerationError::InvalidOpCode(
                self.opcode(),
            ));
        }
        if self.opcode().is_ad() {
            self.1.set_d(num as u16);
        } else {
            self.1.set_c(num);
        }
        Ok(self.1.a())
    }

    #[inline]
    pub fn is_condition(&self) -> bool {
        self.opcode().is_cond()
    }

    #[inline]
    pub fn negate(&mut self) -> bool {
        let Some(new_opcode) = self.opcode().negated() else {
            return false;
        };
        self.set_opcode(new_opcode);
        true
    }
}
