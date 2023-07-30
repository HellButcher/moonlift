use moonlift::Source;
use std::fs;
use std::path::Path;

fn parse_test(path: impl AsRef<Path>) -> Source {
    let path = path.as_ref();
    eprintln!("parsing {}", path.display());
    match Source::read(fs::OpenOptions::new().read(true).open(path).unwrap()) {
        Ok(s) => s,
        Err(e) => panic!("Error while parsing {}: {:?}", path.display(), e),
    }
}

fn compile_test(source: &Source) {
    let mut jit = moonlift::jit::JIT::new();
    jit.compile(source).unwrap()
}

#[test]
fn parse_and_compile_test__all() {
    let s = parse_test("lua/testes/all.lua");
    compile_test(&s);
}
#[test]
fn parse_test__api() {
    parse_test("lua/testes/api.lua");
}
#[test]
fn parse_test__attrib() {
    parse_test("lua/testes/attrib.lua");
}
#[test]
fn parse_test__big() {
    parse_test("lua/testes/big.lua");
}
#[test]
fn parse_test__bitwise() {
    parse_test("lua/testes/bitwise.lua");
}
#[test]
fn parse_test__bwcoercion() {
    parse_test("lua/testes/bwcoercion.lua");
}
#[test]
fn parse_test__calls() {
    parse_test("lua/testes/calls.lua");
}
#[test]
fn parse_test__closure() {
    parse_test("lua/testes/closure.lua");
}
#[test]
fn parse_test__code() {
    parse_test("lua/testes/code.lua");
}
#[test]
fn parse_test__constructs() {
    parse_test("lua/testes/constructs.lua");
}
#[test]
fn parse_test__coroutine() {
    parse_test("lua/testes/coroutine.lua");
}
#[test]
fn parse_test__cstack() {
    parse_test("lua/testes/cstack.lua");
}
#[test]
fn parse_test__db() {
    parse_test("lua/testes/db.lua");
}
#[test]
fn parse_test__errors() {
    parse_test("lua/testes/errors.lua");
}
#[test]
fn parse_test__events() {
    parse_test("lua/testes/events.lua");
}
#[test]
fn parse_test__giles() {
    parse_test("lua/testes/files.lua");
}
#[test]
fn parse_test__gc() {
    parse_test("lua/testes/gc.lua");
}
#[test]
fn parse_test__gengc() {
    parse_test("lua/testes/gengc.lua");
}
#[test]
fn parse_test__goto() {
    parse_test("lua/testes/goto.lua");
}
#[test]
fn parse_test__heavy() {
    parse_test("lua/testes/heavy.lua");
}
#[test]
fn parse_test__literals() {
    parse_test("lua/testes/literals.lua");
}
#[test]
fn parse_test__locals() {
    parse_test("lua/testes/locals.lua");
}
#[test]
fn parse_test__main() {
    parse_test("lua/testes/main.lua");
}
#[test]
fn parse_test__math() {
    parse_test("lua/testes/math.lua");
}
#[test]
fn parse_test__nextvar() {
    parse_test("lua/testes/nextvar.lua");
}
#[test]
fn parse_test__pm() {
    parse_test("lua/testes/pm.lua");
}
#[test]
fn parse_test__sort() {
    parse_test("lua/testes/sort.lua");
}
#[test]
fn parse_test__strings() {
    parse_test("lua/testes/strings.lua");
}
#[test]
fn parse_test__tpack() {
    parse_test("lua/testes/tpack.lua");
}
#[test]
fn parse_test__tracegc() {
    parse_test("lua/testes/tracegc.lua");
}
#[test]
fn parse_test__utf8() {
    parse_test("lua/testes/utf8.lua");
}
#[test]
fn parse_test__vararg() {
    parse_test("lua/testes/vararg.lua");
}
#[test]
fn parse_test__verybig() {
    parse_test("lua/testes/verybig.lua");
}
