use moonlift::{Ast, Bytecode};
use std::io::Read;
use std::{fs, io};
use std::path::Path;

fn parse_test(path: impl AsRef<Path>) -> Ast {
    let path = path.as_ref();
    eprintln!("parsing {}", path.display());
    match Ast::read(fs::OpenOptions::new().read(true).open(path).unwrap()) {
        Ok(s) => s,
        Err(e) => panic!("Error while parsing {}: {:?}", path.display(), e),
    }
}

fn parse_test_and_compile_to_bytecode(path: impl AsRef<Path>) -> Bytecode {
    let path = path.as_ref();
    eprintln!("parsing {}", path.display());
    match Bytecode::parse(fs::OpenOptions::new().read(true).open(path).unwrap()) {
        Ok(s) => s,
        Err(e) => panic!("Error while parsing {}: {:?}", path.display(), e),
    }
}

fn write_bytecode_snapshot(path: impl AsRef<Path>, bytecode: &Bytecode) {
    let path = path.as_ref();
    let mut f = io::BufWriter::new(fs::File::create(path).unwrap());
    bytecode.write_binary(&mut f).unwrap();
}

fn read_bytecode_snapshot(path: impl AsRef<Path>) -> Bytecode {
    let path = path.as_ref();
    let mut f = io::BufReader::new(fs::File::open(path).unwrap());
    Bytecode::read_binary(&mut f).unwrap()
}

// fn compile_test_jit(source: &Ast) {
//     let mut jit = moonlift::jit::JIT::new();
//     jit.compile(source).unwrap()
// }

#[test]
fn parse_and_compile_bc_test_all() {
    let bytecode = parse_test_and_compile_to_bytecode("lua/testes/all.lua");
    insta::assert_debug_snapshot!(bytecode);
    let f = "tests/bytecode/lua_suite__parse_and_compile_bc_test_all.moonliftbc";
    write_bytecode_snapshot(f, &bytecode);
    let read_back = read_bytecode_snapshot(f);
    similar_asserts::assert_eq!(bytecode, read_back);
}
#[test]
fn parse_test_api() {
    parse_test("lua/testes/api.lua");
}
#[test]
fn parse_test_attrib() {
    parse_test("lua/testes/attrib.lua");
}
#[test]
fn parse_test_big() {
    parse_test("lua/testes/big.lua");
}
#[test]
fn parse_test_bitwise() {
    parse_test("lua/testes/bitwise.lua");
}
#[test]
fn parse_test_bwcoercion() {
    parse_test("lua/testes/bwcoercion.lua");
}
#[test]
fn parse_test_calls() {
    parse_test("lua/testes/calls.lua");
}
#[test]
fn parse_test_closure() {
    parse_test("lua/testes/closure.lua");
}
#[test]
fn parse_test_code() {
    parse_test("lua/testes/code.lua");
}
#[test]
fn parse_test_constructs() {
    parse_test("lua/testes/constructs.lua");
}
#[test]
fn parse_test_coroutine() {
    parse_test("lua/testes/coroutine.lua");
}
#[test]
fn parse_test_cstack() {
    parse_test("lua/testes/cstack.lua");
}
#[test]
fn parse_test_db() {
    parse_test("lua/testes/db.lua");
}
#[test]
fn parse_test_errors() {
    parse_test("lua/testes/errors.lua");
}
#[test]
fn parse_test_events() {
    parse_test("lua/testes/events.lua");
}
#[test]
fn parse_test_giles() {
    parse_test("lua/testes/files.lua");
}
#[test]
fn parse_test_gc() {
    parse_test("lua/testes/gc.lua");
}
#[test]
fn parse_test_gengc() {
    parse_test("lua/testes/gengc.lua");
}
#[test]
fn parse_test_goto() {
    parse_test("lua/testes/goto.lua");
}
#[test]
fn parse_test_heavy() {
    parse_test("lua/testes/heavy.lua");
}
#[test]
fn parse_test_literals() {
    parse_test("lua/testes/literals.lua");
}
#[test]
fn parse_test_locals() {
    parse_test("lua/testes/locals.lua");
}
#[test]
fn parse_test_main() {
    parse_test("lua/testes/main.lua");
}
#[test]
fn parse_test_math() {
    parse_test("lua/testes/math.lua");
}
#[test]
fn parse_test_nextvar() {
    parse_test("lua/testes/nextvar.lua");
}
#[test]
fn parse_test_pm() {
    parse_test("lua/testes/pm.lua");
}
#[test]
fn parse_test_sort() {
    parse_test("lua/testes/sort.lua");
}
#[test]
fn parse_test_strings() {
    parse_test("lua/testes/strings.lua");
}
#[test]
fn parse_test_tpack() {
    parse_test("lua/testes/tpack.lua");
}
#[test]
fn parse_test_tracegc() {
    parse_test("lua/testes/tracegc.lua");
}
#[test]
fn parse_test_utf8() {
    parse_test("lua/testes/utf8.lua");
}
#[test]
fn parse_test_vararg() {
    parse_test("lua/testes/vararg.lua");
}
#[test]
fn parse_test_verybig() {
    parse_test("lua/testes/verybig.lua");
}
