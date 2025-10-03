macro_rules! __mkop {
    (type[$d:ident]) => (AD);
    (type[$a:ident, $d:ident]) => (AD);
    (type[$a:ident, $b:ident, $c:ident]) => (ABC);

    (set_a_dst_impl $arg:ident $new_reg:ident [dst $(, $x:ident)*]) => ({
      $arg.a = $new_reg;
      return Ok(())
    });
    (set_a_dst_impl $arg:ident $new_reg:ident [$($x:ident),*]) => ({});

    (set_base_num_impl $arg:ident $new_num:ident [base, $x:ident, num]) => ({
      $arg.c = $new_num;
      return Ok($arg.a)
    });
    (set_base_num_impl $arg:ident $new_num:ident [base, num]) => ({
      $arg.d = $new_num as u16;
      return Ok($arg.a)
    });
    (set_base_num_impl $arg:ident $new_num:ident [$($x:ident),*]) => ({});

    (mapargs $args:ident [$dname:ident: $dmode:ident] [$($rest:tt)*]) => {
        __mkop![$($rest)* [$dname: $dmode = $args.d]];
    };
    (mapargs $args:ident [$aname:ident: $amode:ident, $dname:ident: $dmode:ident] [$($rest:tt)*]) => {
        __mkop![$($rest)* [$aname: $amode = $args.a]];
        __mkop![$($rest)* [$dname: $dmode = $args.d]];
    };
    (mapargs $args:ident [$aname:ident: $amode:ident, $bname:ident: $bmode:ident, $cname:ident: $cmode:ident] [$($rest:tt)*]) => {
        __mkop![$($rest)* [$aname: $amode = $args.a]];
        __mkop![$($rest)* [$bname: $bmode = $args.b]];
        __mkop![$($rest)* [$cname: $cmode = $args.c]];
    };

    // Argument modes:
    // - dst: variable slot number, used as a destination
    (letitem $vm:ident [$n:ident: dst = $e:expr]) => { };
    // - uvdst: upvalue number, used as a destination
    (letitem $vm:ident [$n:ident: uvdst = $e:expr]) => { };
    // - var: variable slot number
    (letitem $vm:ident [$n:ident: var = $e:expr]) => { let $n = $vm.get_var($e as u16); };
    // - base: base slot number, read-write
    (letitem $vm:ident [$n:ident: base = $e:expr]) => {};
    // - rbase: base slot number, read-only
    (letitem $vm:ident [$n:ident: rbase = $e:expr]) => {};
    // - uv: upvalue number
    (letitem $vm:ident [$n:ident: uv = $e:expr]) => { let $n = $vm.get_uv($e as u16); };
    // - str: string constant, negated index into constant table
    (letitem $vm:ident [$n:ident: str = $e:expr]) => { let $n = $vm.get_str($e as u16); };
    // - tab: template table, negated index into constant table
    (letitem $vm:ident [$n:ident: tab = $e:expr]) => { let $n = $vm.get_tab($e as u16); };
    // - num: number constant, index into constant table
    (letitem $vm:ident [$n:ident: num = $e:expr]) => { let $n = $vm.get_num($e as u16); };
    // - cdata: cdata constant, negated index into constant table
    (letitem $vm:ident [$n:ident: cdata = $e:expr]) => { }; // TODO
    // - lit: literal
    (letitem $vm:ident [$n:ident: lit = $e:expr]) => { let $n = $e as Value; }; // TODO
    // - lits: signed literal
    (letitem $vm:ident [$n:ident: lits = $e:expr]) => { let $n = $e as Value; }; // TODO
    // - pri: primitive type (0 = nil, 1 = false, 2 = true)
    (letitem $vm:ident [$n:ident: pri = $e:expr]) => { let $n = $e as Value; }; // TODO
    // - jump: branch target, relative to next instruction, biased with 0x8000
    (letitem $vm:ident [$n:ident: jump = $e:expr]) => { let $n = Jump($e); };
    // - func: function prototype, negated index into constant table
    (letitem $vm:ident [$n:ident: func = $e:expr]) => { let $n = $vm.get_func($e as u16); };


    // - dst: variable slot number, used as a destination
    (postitem $vm:ident [$n:ident: dst = $e:expr]) => { $vm.set_var($e as u16, $n); };
    // - uvdst: upvalue number, used as a destination
    (postitem $vm:ident [$n:ident: uvdst = $e:expr]) => { $vm.set_uv($e as u16, $n); };
    // other
    (postitem $vm:ident [$n:ident: $other:ident = $e:expr]) => { };
}

macro_rules! define_opcodes {
    {
        $vm:ident =>

        $($id:ident ($($arg:ident : $mode:ident),*) {$($body:tt)*} ),* $(,)?
    } => {
        #[repr(u8)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum OpCode {
            $($id),*
        }

        #[repr(u8)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum Op {
            $(
                $id (__mkop!(type[$($mode),*])) = OpCode::$id as u8
            ),*
        }

        impl Op {

            pub fn set_a_dst(&mut self, dst: u8) -> Result<(), crate::codegen_state::CodeGenerationError> {
              match self {
                $(
                    Op::$id(_args) => __mkop!(set_a_dst_impl _args dst [$($mode),*]),
                )*
              }
              Err(crate::codegen_state::CodeGenerationError::InvalidOpCode(self.opcode()))
            }

            pub fn set_base_num(&mut self, num: u8) -> Result<u8, crate::codegen_state::CodeGenerationError> {
              match self {
                $(
                    Op::$id(_args) => __mkop!(set_base_num_impl _args num [$($mode),*]),
                )*
              }
              Err(crate::codegen_state::CodeGenerationError::InvalidOpCode(self.opcode()))
            }

            fn exec(&self, $vm: &mut VM) {
                match self {
                    $(
                        Op::$id(_args) => {
                            __mkop!(mapargs _args[$($arg: $mode),*] [letitem $vm]);
                            $($body)*
                            __mkop!(mapargs _args[$($arg: $mode),*] [postitem $vm]);
                        }
                    ),*
                }
            }
        }
    };
}

macro_rules! OP {
    ($code:ident) => (crate::opcode::Op::$code(crate::opcode::AD{
        a: 0,
        d: 0,
    }));
    ($code:ident ($d:expr)) => (crate::opcode::Op::$code(crate::opcode::AD{
        a: 0,
        d: $d as u16,
    }));
    ($code:ident ($a:expr, $d:expr)) => (crate::opcode::Op::$code(crate::opcode::AD{
        a: $a as u8,
        d: $d as u16,
    }));
    ($code:ident ($a:expr, $b:expr, $c:expr)) => (crate::opcode::Op::$code(crate::opcode::ABC{
        a: $a as u8,
        b: $b as u8,
        c: $c as u8,
    }));
}

struct VM {
    pc: usize
}
type Value = u32;
impl VM {
    fn get_var(&self, var: u16) -> Value {
        todo!()
    }
    fn get_str(&self, str: u16) -> Value {
        todo!()
    }
    fn get_tab(&self, tab: u16) -> Value {
        todo!()
    }
    fn get_func(&self, tab: u16) -> Value {
        todo!()
    }
    fn get_num(&self, num: u16) -> Value {
        todo!()
    }
    fn get_uv(&self, uv: u16) -> Value {
        todo!()
    }
    fn set_var(&mut self, var: u16, val: u32) {
        todo!()
    }
    fn set_uv(&mut self, uv: u16, val: u32) {
        todo!()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ABC{
    pub(crate) a: u8,
    pub(crate) b: u8,
    pub(crate) c: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct AD{
    pub(crate) a: u8,
    pub(crate) d: u16,
}

/// variable slot number, used as a destination
#[repr(transparent)]
struct Dst(u8);
/// variable slot number
#[repr(transparent)]
struct Var(u8);
/// string constant, negated index into constant table
#[repr(transparent)]
struct Str(u16);
/// number constant, index into constant table
#[repr(transparent)]
struct Num(u16);
/// literal
#[repr(transparent)]
struct Lit(u16);
/// signed literal
#[repr(transparent)]
struct LitS(i16);
/// cdata constant, negated index into constant table
#[repr(transparent)]
struct Cdata(u16);
/// branch target, relative to next instruction, biased with 0x8000
#[repr(transparent)]
struct Jump(u16);
/// function prototype, negated index into constant table
#[repr(transparent)]
struct Func(u16);
/// primitive type (0 = `nil`, 1 = `false`, 2 = `true`)
#[repr(transparent)]
struct Pri(u8);

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


define_opcodes!{
    vm =>

    // Jump if A < D
    IsLt(a: var, d: var) {
        if (a >= d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≥ D
    IsGe(a: var, d: var) {
        if (a < d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≤ D 	
    IsLe(a: var, d: var) {
        if (a > d) {
            vm.pc += 1;
        }
    },
    // Jump if A > D
    IsGt(a: var, d: var) {
        if (a <= d) {
            vm.pc += 1;
        }
    },
    // Jump if A = D
    IsEqV(a: var, d: var) {
        if (a != d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≠ D
    IsNeV(a: var, d: var) {
        if (a == d) {
            vm.pc += 1;
        }
    },
    /*
    // Jump if A = D
    IsEqS(a: var, d: str) {
        if (a != d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≠ D
    IsNeS(a: var, d: str) {
        if (a == d) {
            vm.pc += 1;
        }
    },
    // Jump if A = D
    IsEqN(a: var, d: num) {
        if (a != d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≠ D
    IsNeN(a: var, d: num) {
        if (a == d) {
            vm.pc += 1;
        }
    },
    // Jump if A = D
    IsEqP(a: var, d: pri) {
        if (a != d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≠ D
    IsNeP(a: var, d: pri) {
        if (a == d) {
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
        let a = d;
    },
    // Set A to boolean not of D
    Not(a: dst, d: var) {
        let a = !d;
    },
    // Set A to -D (unary minus) 	
    UNM(a: dst, d: var) {
        let a = (-(d as i32)) as u32;
    },
    // Set A to #D (object length)
    Len(a: dst, d: var) {
      // TODO
      let a = todo!();
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
    // Set A to cdata constant D
    KCData(a: dst, d: cdata) {
      // TODO
      let a = todo!();
    },
    // Set A to 16 bit signed integer D
    KShort(a: dst, d: lit) {
      let a = d;
    },
    // Set A to number constant D
    KNum(a: dst, d: num) {
      let a = d;
    },
    // Set A to primitive D
    // switch (D) {
    //     case 0: A = nil;
    //     case 1: A = false;
    //     case 2: A = true;
    // }
    KPri(a: dst, d: pri) {
      let a = d;
    },
    // Set slots A to D to nil
    KNil(a: base, d: base) {
      todo!();
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
    // Set A to duplicated template table D
    TDup(a: dst, d: tab) {
      // TODO
      let a = todo!();
    },
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
    // check values and prepare counters; f not to run then pc+=D+1
    ForI(a: base, d: jump) {
      // TODO
        todo!();
    },
    // Numeric 'for' loop - loop
    // update counters; if loop continues then pc-=D;
    ForL(a: base, d: jump) {      // TODO
        todo!();
    },
    // Call iterator: A, A+1, A+2 = A-3, A-2, A-1; A, ..., A+B-2 = A(A+1, A+2)
    IterC(a: base, b: lit, c: lit) { // TODO
        todo!();
    },
    // Jump
    Jmp(a: rbase, d: jump) {
      // TODO
        todo!();
    },

    // Vararg: A, ..., A+B-2 = ...
    VArg(a: base, b: lit, c: lit) {
      todo!();
    }, 
}

impl OpCode {
  pub fn is_cond(self) -> bool {
      matches!(self,
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
  pub fn opcode(&self) -> OpCode {
      // SAFETY: Because `Self` is marked `repr(u8)`, its layout is a `repr(C)` `union`
      // between `repr(C)` structs, each of which has the `u8` discriminant as its first
      // field, so we can read the discriminant without offsetting the pointer.
      // Also the discriminant value is the same as the `OpCode` value.
      unsafe { *<*const _>::from(self).cast::<OpCode>() }
  }

  #[inline]
  pub fn set_opcode(&mut self, new_opcode: OpCode) {
      // SAFETY: Because `Self` is marked `repr(u8)`, its layout is a `repr(C)` `union`
      // between `repr(C)` structs, each of which has the `u8` discriminant as its first
      // field, so we can read the discriminant without offsetting the pointer.
      // Also the discriminant value is the same as the `OpCode` value.
      unsafe {
        let mut_opcode = <*mut _>::from(self).cast::<OpCode>();
        mut_opcode.write(new_opcode);
      }
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
