use std::collections::HashMap;

use crate::ast::{Expression, InfixOp, Number, Statement, UnaryOp};
use crate::Source;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};

pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,
}

impl JIT {
    pub fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);
        let builder_context = FunctionBuilderContext::new();
        let ctx = module.make_context();
        let data_description = DataDescription::new();
        Self {
            builder_context,
            ctx,
            data_description,
            module,
        }
    }

    pub fn compile(&mut self, source: &Source) -> Result<(), String> {
        // TODO: error handling
        self.compile_fn(&[], &source.block)?;
        Ok(())
    }

    fn compile_fn(&mut self, params: &[String], block: &[Statement]) -> Result<*const u8, String> {
        let int = self.module.target_config().pointer_type();

        // define the signature (currently only int, and always return
        for _ in params {
            self.ctx.func.signature.params.push(AbiParam::new(int));
        }
        //self.ctx.func.signature.returns.push(AbiParam::new(int));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();

        // Since this is the entry block, add block parameters corresponding to
        // the function's parameters.
        builder.append_block_params_for_function_params(entry_block);

        // Tell the builder to emit code in this block.
        builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further
        // predecessors. Since it's the entry block, it won't have any
        // predecessors.
        builder.seal_block(entry_block);

        let mut translator = Translator {
            int,
            builder,
            locals: HashMap::new(),
            module: &mut self.module,
        };

        // declar variables for params
        for (i, param) in params.iter().enumerate() {
            let val = translator.builder.block_params(entry_block)[i];
            let var = translator.declare_local(param);
            translator.builder.def_var(var, val);
        }

        // now translate the statements
        for stmt in block {
            translator.translate_statement(stmt);
        }

        // TODO: return value
        translator.builder.finalize();

        ////////////////////////////////////////////////////////////
        // declare the function
        let id = self
            .module
            .declare_function("main", Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;
        // define the function
        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| e.to_string())?;

        // compilation finished for the function, we can clear the context.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module.finalize_definitions().unwrap();

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        Ok(code)
    }
}
impl Default for JIT {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

struct Translator<'a> {
    int: types::Type,
    builder: FunctionBuilder<'a>,
    locals: HashMap<String, Variable>,
    module: &'a mut JITModule,
}

impl Translator<'_> {
    fn translate_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::If {
                ref ifcases,
                ref elsecase,
            } => {
                let merge_block = self.builder.create_block();
                for (cond, block) in ifcases {
                    let cond_value = self.translate_expr(cond);
                    let then_block = self.builder.create_block();
                    let else_block = self.builder.create_block();

                    self.builder
                        .ins()
                        .brif(cond_value, then_block, &[], else_block, &[]);

                    // fill the block
                    self.builder.switch_to_block(then_block);
                    self.builder.seal_block(then_block);
                    for stmt in block {
                        self.translate_statement(stmt);
                    }

                    // jump to the merge-block
                    self.builder.ins().jump(merge_block, &[]);

                    // now switch to the else-block
                    self.builder.switch_to_block(else_block);
                    self.builder.seal_block(else_block);
                }

                // handle the else block
                for stmt in elsecase {
                    self.translate_statement(stmt);
                }

                // jump to the merge-block
                self.builder.ins().jump(merge_block, &[]);
                // now switch to the merge-block
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
            }
            Statement::While {
                ref cond,
                ref block,
            } => {
                let header_block = self.builder.create_block();
                let body_block = self.builder.create_block();
                let exit_block = self.builder.create_block();

                self.builder.ins().jump(header_block, &[]);
                self.builder.switch_to_block(header_block); // not sealed (not yet all inbound paths defined)

                let cond_value = self.translate_expr(cond);
                self.builder
                    .ins()
                    .brif(cond_value, body_block, &[], exit_block, &[]);

                // now fill the body
                self.builder.switch_to_block(body_block);
                self.builder.seal_block(body_block);
                for stmt in block {
                    self.translate_statement(stmt);
                }
                // and jump back to the header
                self.builder.ins().jump(header_block, &[]);

                // jump to the exit block
                self.builder.switch_to_block(exit_block);
                self.builder.seal_block(header_block); // now, the header-block can be sealed
                self.builder.seal_block(exit_block);
            }
            Statement::Repeat {
                ref block,
                ref cond,
            } => {
                let body_block = self.builder.create_block();
                let exit_block = self.builder.create_block();

                self.builder.ins().jump(body_block, &[]);
                self.builder.switch_to_block(body_block); // not sealed yet

                for stmt in block {
                    self.translate_statement(stmt);
                }

                let cond_value = self.translate_expr(cond);
                self.builder
                    .ins()
                    .brif(cond_value, body_block, &[], exit_block, &[]);

                self.builder.switch_to_block(exit_block);
                self.builder.seal_block(body_block); // not, the body block can be sealed
                self.builder.seal_block(exit_block);
            }
            Statement::Local {
                ref vars,
                ref exprs,
            } => {
                let values: Vec<_> = exprs.iter().map(|e| self.translate_expr(e)).collect();
                for ((name, _attr), val) in vars.iter().zip(values.into_iter()) {
                    let var = self.declare_local(name.as_str());
                    self.builder.def_var(var, val);
                }
            }
            Statement::Assign {
                ref vars,
                ref exprs,
            } => {
                let values: Vec<_> = exprs.iter().map(|e| self.translate_expr(e)).collect();
                for (var, val) in vars.iter().zip(values.into_iter()) {
                    match var {
                        Expression::Var(name) => {
                            // TODO: also resolve global vars, + error handling
                            let var = self.locals.get(name).expect("not defined");
                            self.builder.def_var(*var, val)
                        }
                        _ => todo!("not implemented: {:?}", var),
                    }
                }
            }
            Statement::Return(e) => {
                // TODO: expr optional
                // TODO: return_call, if e is a function-call
                let values: Vec<_> = e.iter().map(|e| self.translate_expr(e)).collect();
                self.builder.ins().return_(&values);
            }
            _ => todo!("unimplemented {stmt:?}"),
        }
    }

    fn translate_expr(&mut self, expr: &Expression) -> Value {
        match expr {
            Expression::Nil => self.builder.ins().null(self.int),
            Expression::Boolean(v) => self.builder.ins().iconst(self.int, if *v { 1 } else { 0 }),
            Expression::Number(v) => match v {
                Number::Integer(i) => self.builder.ins().iconst(self.int, *i),
                Number::Float(f) => self.builder.ins().f64const(*f),
            },
            Expression::Var(name) => {
                // TODO: resolve+error handling
                let var = self.locals.get(name).expect("variable not defined");
                self.builder.use_var(*var)
            }
            Expression::Unary(op, expr) => {
                let val = self.translate_expr(expr);
                match op {
                    UnaryOp::Minus => self.builder.ins().ineg(val),
                    UnaryOp::BitNot => self.builder.ins().bnot(val),
                    // TODO: is there a better way?
                    UnaryOp::Not => self.builder.ins().icmp_imm(IntCC::Equal, val, 0),
                    UnaryOp::Len => todo!(),
                }
            }
            Expression::Infix(op, exprs) => {
                // TODO: lazy-eval for`and` & `or`
                // TODO: left and right assoc.
                // TODO: integer & float handling
                let values: Vec<_> = exprs.iter().map(|e| self.translate_expr(e)).collect();
                let mut values = values.into_iter();
                let mut e = values.next().unwrap(); // TODO: error
                while let Some(val) = values.next() {
                    match op {
                        InfixOp::Add => e = self.builder.ins().iadd(e, val),
                        InfixOp::Sub => e = self.builder.ins().isub(e, val),
                        InfixOp::Mul => e = self.builder.ins().imul(e, val),
                        InfixOp::Div => e = self.builder.ins().sdiv(e, val),
                        InfixOp::FloorDiv => e = self.builder.ins().udiv(e, val),
                        InfixOp::Mod => e = self.builder.ins().srem(e, val),
                        InfixOp::Less => e = self.builder.ins().icmp(IntCC::SignedLessThan, e, val),
                        InfixOp::LessEq => {
                            e = self
                                .builder
                                .ins()
                                .icmp(IntCC::SignedLessThanOrEqual, e, val)
                        }
                        InfixOp::Greater => {
                            e = self.builder.ins().icmp(IntCC::SignedGreaterThan, e, val)
                        }
                        InfixOp::GreaterEq => {
                            e = self
                                .builder
                                .ins()
                                .icmp(IntCC::SignedGreaterThanOrEqual, e, val)
                        }
                        InfixOp::Eq => e = self.builder.ins().icmp(IntCC::Equal, e, val),
                        InfixOp::NotEq => e = self.builder.ins().icmp(IntCC::NotEqual, e, val),
                        _ => todo!("unsupported operand {op:?}"),
                    }
                }
                e
            }
            _ => todo!("Unsupported expression {expr:?}"),
        }
    }

    fn declare_local(&mut self, name: &str) -> Variable {
        assert!(!self.locals.contains_key(name));

        let idx = self.locals.len();
        let var = Variable::new(idx);
        self.locals.insert(name.to_string(), var);
        self.builder.declare_var(var, self.int);
        var
    }
}
