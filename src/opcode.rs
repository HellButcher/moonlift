macro_rules! __mkop {
    (type[$d:ident]) => (AD);
    (type[$a:ident, $d:ident]) => (AD);
    (type[$a:ident, $b:ident, $c:ident]) => (ABC);


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

    (letitem $vm:ident [$n:ident: dst = $e:expr]) => { };
    (letitem $vm:ident [$n:ident: uvdst = $e:expr]) => { };
    (letitem $vm:ident [$n:ident: var = $e:expr]) => { let $n = $vm.get_var($e as u16); };
    (letitem $vm:ident [$n:ident: base = $e:expr]) => {};
    (letitem $vm:ident [$n:ident: rbase = $e:expr]) => {};
    (letitem $vm:ident [$n:ident: uv = $e:expr]) => { let $n = $vm.get_uv($e as u16); };
    (letitem $vm:ident [$n:ident: str = $e:expr]) => { let $n = $vm.get_str($e as u16); };
    (letitem $vm:ident [$n:ident: num = $e:expr]) => { let $n = $vm.get_num($e as u16); };
    (letitem $vm:ident [$n:ident: cdata = $e:expr]) => { }; // TODO
    (letitem $vm:ident [$n:ident: lit = $e:expr]) => { let $n = $e as Value; }; // TODO
    (letitem $vm:ident [$n:ident: pri = $e:expr]) => { let $n = $e as Value; }; // TODO
    (letitem $vm:ident [$n:ident: jump = $e:expr]) => { let $n = Jump($e); };

    (postitem $vm:ident [$n:ident: dst = $e:expr]) => { $vm.set_var($e as u16, $n); };
    (postitem $vm:ident [$n:ident: uvdst = $e:expr]) => { $vm.set_uv($e as u16, $n); };
    (postitem $vm:ident [$n:ident: $other:ident = $e:expr]) => { };
}

macro_rules! define_opcodes {
    {
        $vm:ident =>

        $($id:ident ($($arg:ident : $mode:ident),*) {$($body:tt)*} ),* $(,)?
    } => {
        #[repr(u8)]
        pub enum OpCode {
            $($id),*
        }

        #[repr(u8)]
        pub enum Op {
            $(
                $id (__mkop!(type[$($mode),*])) 
            ),*
        }

        impl Op {
            pub fn opcode(&self) -> OpCode {
                match self {
                    $(
                        Op::$id(_) => OpCode::$id
                    ),*
                }
            }

            fn exec(&self, $vm: &mut VM) {
                match self {
                    $(
                        Op::$id(args) => {
                            __mkop!(mapargs args[$($arg: $mode),*] [letitem $vm]);
                            $($body)*
                            __mkop!(mapargs args[$($arg: $mode),*] [postitem $vm]);
                        }
                    ),*
                }
            }
        }
    };
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

struct ABC{
    a: u8,
    b: u8,
    c: u8,
}
struct AD{
    a: u8,
    d: u16,
}

struct Dst(u8);
struct Var(u8);
struct Str(u16);
struct Num(u16);
struct Lit(u16);
struct Cdata(u16);
struct Jump(u16);
struct Pri(u8);

define_opcodes!{
    vm =>

    // Jump if A < D
    IsLt(a: var, d: var) {
        if !(a < d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≥ D
    IdGe(a: var, d: var) {
        if !(a >= d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≤ D 	
    IsLe(a: var, d: var) {
        if !(a <= d) {
            vm.pc += 1;
        }
    },
    // Jump if A > D
    IsGt(a: var, d: var) {
        if !(a > d) {
            vm.pc += 1;
        }
    },
    // Jump if A = D
    IsEqV(a: var, d: var) {
        if !(a == d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≠ D
    IsNeV(a: var, d: var) {
        if !(a != d) {
            vm.pc += 1;
        }
    },
    // Jump if A = D
    IsEqS(a: var, d: str) {
        if !(a == d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≠ D
    IsNeS(a: var, d: str) {
        if !(a != d) {
            vm.pc += 1;
        }
    },
    // Jump if A = D
    IsEqN(a: var, d: num) {
        if !(a == d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≠ D
    IsNeN(a: var, d: num) {
        if !(a != d) {
            vm.pc += 1;
        }
    },
    // Jump if A = D
    IsEqP(a: var, d: pri) {
        if !(a == d) {
            vm.pc += 1;
        }
    },
    // Jump if A ≠ D
    IsNeP(a: var, d: pri) {
        if !(a != d) {
            vm.pc += 1;
        }
    },

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
    // Jump if A is of type D
    IsType(a: var, d: lit) {
        // TODO
        todo!();
    },
    // Jump if A = D
    IsShort(a: var, d: lit) {
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
    // A = B % C
    ModVV(a: dst, b: var, c: var) {
      let a = b % c;
    },

    // A = B ^ C
    Pow(a: dst, b: var, c: var) {
      let a = b.pow(c);
    },
    // A = B .. ~ .. C
    Cat(a: dst, b: var, c: var) {
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
    // Set A to primitive D
    // switch (D) {
    //     case 0: A = nil;
    //     case 1: A = false;
    //     case 2: A = true;
    // }
    KPri(a: dst, d: pri) {
      let a = d;
    },

    // Set A to upvalue D
    UGet(a: dst, d: uv) {
      let a = d;
    },
    // Set upvalue A to D
    USet(a: uvdst, d: var) {
      let a = d;
    },
    // Set upvalue A to string constant D
    USetS(a: uvdst, d: str) {
      let a = d;
    },
    // Set upvalue A to number constant D
    USetN(a: uvdst, d: num) {
      let a = d;
    },
    // Set upvalue A to 16bit signed  integer D
    USetShort(a: uvdst, d: lit) {
      let a = d;
    },
    // Set upvalue A to primitive D
    USetP(a: uvdst, d: pri) {
      let a = d;
    },
    

    // Set A to new table with size D
    TNew(a: dst, d: lit) {
      // TODO
      let a = todo!();
    },
    // A = B[C]
    TGet(a: dst, b: var, c: var) {
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
    TSet(a: var, b: var, c: var) {
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


    // Call: A, ..., A+B-2 = A(A+1, ..., A+C-1)
    Call(a: base, b: lit, c: lit) {
      // TODO
        todo!();
    },
    // Tail-Call: return A(A+1, ..., A+D-1)
    CallT(a: base, d: lit) {
      // TODO
        todo!();
    },
    // Return A, ..., A+D-2
    Ret(a: base, d: lit) {
      // TODO
        todo!();
    },

    // Numeric 'for' loop
    ForI(a: base, d: jump) {
      // TODO
        todo!();
    },
    // Jump
    Jmp(a: rbase, d: jump) {
      // TODO
        todo!();
    }
}



