
# Moonlift AI Coding Agent Instructions

## Project Overview
Moonlift is a Rust-based port of LuaJIT, extended with Lua 5.4 features. The architecture is modular, with clear separation between parsing, code generation, runtime, and VM execution. The goal is compatibility with LuaJIT semantics while modernizing and extending for Lua 5.4.

## Key Components & Data Flow
- **src/ast.rs, parser.rs, parser_ast.rs**: AST and parser logic. The parser produces an AST from Lua source code.
- **src/codegen.rs, codegen_state.rs**: Translates AST to bytecode. Bytecode format is documented in `docs/lua-53-opcodes.md` and uses custom opcodes inspired by LuaJIT.
- **src/opcode.rs**: Macro-Based Opcode definitions and operand types (e.g., Tab, Func).
- **src/vm.rs, runtime.rs**: Bytecode interpreter and runtime. Implements stack, call frames, upvalues, closures, and integrates Lua 5.4 features.
- **src/jit.rs**: JIT compiler (planned/partial), intended to use Cranelift.
- **src/val.rs**: Dynamic type system, including NaN-boxing and GC integration.
- **ffi.rs, ffi_impl.rs**: Foreign function interface (optional extension).

## Developer Workflows
- **Check**: Use Cargo (`cargo check`).
- **Build**: Use Cargo (`cargo build`).
- **Test**: Run Rust unit tests (`cargo test`). For Lua compatibility, use the Lua test suite in `lua/testes/` and Rust test harness in `tests/lua_suite.rs`.
- **Debug**: Focus on VM and codegen logic. Use Rust debugging tools. For bytecode issues, refer to opcode docs and `src/opcode.rs`.
- **Benchmarks**: Profiling is manual; First focus on correctness; only then add benchmarks as needed.

## Project-Specific Conventions
- **Opcode Naming**: CamelCase, semantically matching LuaJIT, but argument order and types may differ for Rust idioms.
- **AST/Parser**: AST is the central IR; all codegen and analysis operate on it.
- **Bytecode**: Inspired by LuaJIT, but extended for Lua 5.4. See `docs/lua-53-opcodes.md` for reference.
- **Testing**: Use both Rust and Lua test suites. Ensure compatibility with Lua 5.4 semantics.
- **Extensibility**: FFI, debugger, and external libraries are optional and modular.

## Integration Points
- **Lua Test Suite**: `lua/testes/` for compatibility testing.
- **Cranelift**: For JIT backend (planned).
- **FFI**: Via `ffi.rs` and `ffi_impl.rs`.

## Examples
- To add a new opcode: update `src/opcode.rs`, extend codegen in `src/codegen.rs`, and update VM logic in `src/vm.rs`.
- To extend AST: modify `src/ast.rs` and update parser/codegen accordingly.
- To run Lua tests: `cargo test` (Rust) and run scripts in `lua/testes/` (Lua).

## References
- `docs/lua-53-opcodes.md`: Bytecode format and opcode reference
- `src/`: Main Rust source code
- `lua/`: Reference Lua sources and tests
- `tests/`: Rust test harness

