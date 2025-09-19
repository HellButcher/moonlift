use crate::ffi::*;

#[no_mangle]
pub extern "C" fn lua_newstate(f: lua_Alloc, ud: *mut ::std::os::raw::c_void) -> *mut lua_State {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn lua_close(L: *mut lua_State) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_newthread(L: *mut lua_State) -> *mut lua_State {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_closethread(
    L: *mut lua_State,
    from: *mut lua_State,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_resetthread(L: *mut lua_State) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_atpanic(L: *mut lua_State, panicf: lua_CFunction) -> lua_CFunction {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_version(L: *mut lua_State) -> lua_Number {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_absindex(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_gettop(L: *mut lua_State) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_settop(L: *mut lua_State, idx: ::std::os::raw::c_int) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_pushvalue(L: *mut lua_State, idx: ::std::os::raw::c_int) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_rotate(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
    n: ::std::os::raw::c_int,
) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_copy(
    L: *mut lua_State,
    fromidx: ::std::os::raw::c_int,
    toidx: ::std::os::raw::c_int,
) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_checkstack(
    L: *mut lua_State,
    n: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_xmove(from: *mut lua_State, to: *mut lua_State, n: ::std::os::raw::c_int) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_isnumber(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_isstring(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_iscfunction(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_isinteger(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_isuserdata(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_type(L: *mut lua_State, idx: ::std::os::raw::c_int) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_typename(
    L: *mut lua_State,
    tp: ::std::os::raw::c_int,
) -> *const ::std::os::raw::c_char {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_tonumberx(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
    isnum: *mut ::std::os::raw::c_int,
) -> lua_Number {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_tointegerx(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
    isnum: *mut ::std::os::raw::c_int,
) -> lua_Integer {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_toboolean(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_tolstring(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
    len: *mut usize,
) -> *const ::std::os::raw::c_char {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_rawlen(L: *mut lua_State, idx: ::std::os::raw::c_int) -> lua_Unsigned {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_tocfunction(L: *mut lua_State, idx: ::std::os::raw::c_int) -> lua_CFunction {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_touserdata(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
) -> *mut ::std::os::raw::c_void {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_tothread(L: *mut lua_State, idx: ::std::os::raw::c_int) -> *mut lua_State {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_topointer(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
) -> *const ::std::os::raw::c_void {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_arith(L: *mut lua_State, op: ::std::os::raw::c_int) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_rawequal(
    L: *mut lua_State,
    idx1: ::std::os::raw::c_int,
    idx2: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_compare(
    L: *mut lua_State,
    idx1: ::std::os::raw::c_int,
    idx2: ::std::os::raw::c_int,
    op: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_pushnil(L: *mut lua_State) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_pushnumber(L: *mut lua_State, n: lua_Number) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_pushinteger(L: *mut lua_State, n: lua_Integer) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_pushlstring(
    L: *mut lua_State,
    s: *const ::std::os::raw::c_char,
    len: usize,
) -> *const ::std::os::raw::c_char {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_pushstring(
    L: *mut lua_State,
    s: *const ::std::os::raw::c_char,
) -> *const ::std::os::raw::c_char {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_pushvfstring(
    L: *mut lua_State,
    fmt: *const ::std::os::raw::c_char,
    argp: *mut __va_list_tag,
) -> *const ::std::os::raw::c_char {
    unimplemented!()
}
#[no_mangle]
pub unsafe extern "C" fn lua_pushfstring(
    L: *mut lua_State,
    fmt: *const ::std::os::raw::c_char,
    ...
) -> *const ::std::os::raw::c_char {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_pushcclosure(
    L: *mut lua_State,
    fn_: lua_CFunction,
    n: ::std::os::raw::c_int,
) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_pushboolean(L: *mut lua_State, b: ::std::os::raw::c_int) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_pushlightuserdata(L: *mut lua_State, p: *mut ::std::os::raw::c_void) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_pushthread(L: *mut lua_State) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_getglobal(
    L: *mut lua_State,
    name: *const ::std::os::raw::c_char,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_gettable(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_getfield(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
    k: *const ::std::os::raw::c_char,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_geti(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
    n: lua_Integer,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_rawget(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_rawgeti(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
    n: lua_Integer,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_rawgetp(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
    p: *const ::std::os::raw::c_void,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_createtable(
    L: *mut lua_State,
    narr: ::std::os::raw::c_int,
    nrec: ::std::os::raw::c_int,
) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_newuserdatauv(
    L: *mut lua_State,
    sz: usize,
    nuvalue: ::std::os::raw::c_int,
) -> *mut ::std::os::raw::c_void {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_getmetatable(
    L: *mut lua_State,
    objindex: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_getiuservalue(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
    n: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_setglobal(L: *mut lua_State, name: *const ::std::os::raw::c_char) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_settable(L: *mut lua_State, idx: ::std::os::raw::c_int) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_setfield(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
    k: *const ::std::os::raw::c_char,
) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_seti(L: *mut lua_State, idx: ::std::os::raw::c_int, n: lua_Integer) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_rawset(L: *mut lua_State, idx: ::std::os::raw::c_int) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_rawseti(L: *mut lua_State, idx: ::std::os::raw::c_int, n: lua_Integer) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_rawsetp(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
    p: *const ::std::os::raw::c_void,
) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_setmetatable(
    L: *mut lua_State,
    objindex: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_setiuservalue(
    L: *mut lua_State,
    idx: ::std::os::raw::c_int,
    n: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_callk(
    L: *mut lua_State,
    nargs: ::std::os::raw::c_int,
    nresults: ::std::os::raw::c_int,
    ctx: lua_KContext,
    k: lua_KFunction,
) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_pcallk(
    L: *mut lua_State,
    nargs: ::std::os::raw::c_int,
    nresults: ::std::os::raw::c_int,
    errfunc: ::std::os::raw::c_int,
    ctx: lua_KContext,
    k: lua_KFunction,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_load(
    L: *mut lua_State,
    reader: lua_Reader,
    dt: *mut ::std::os::raw::c_void,
    chunkname: *const ::std::os::raw::c_char,
    mode: *const ::std::os::raw::c_char,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_dump(
    L: *mut lua_State,
    writer: lua_Writer,
    data: *mut ::std::os::raw::c_void,
    strip: ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_yieldk(
    L: *mut lua_State,
    nresults: ::std::os::raw::c_int,
    ctx: lua_KContext,
    k: lua_KFunction,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_resume(
    L: *mut lua_State,
    from: *mut lua_State,
    narg: ::std::os::raw::c_int,
    nres: *mut ::std::os::raw::c_int,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_status(L: *mut lua_State) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_isyieldable(L: *mut lua_State) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_setwarnf(
    L: *mut lua_State,
    f: lua_WarnFunction,
    ud: *mut ::std::os::raw::c_void,
) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_warning(
    L: *mut lua_State,
    msg: *const ::std::os::raw::c_char,
    tocont: ::std::os::raw::c_int,
) {
    unimplemented!()
}
#[no_mangle]
pub unsafe extern "C" fn lua_gc(
    L: *mut lua_State,
    what: ::std::os::raw::c_int,
    ...
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_error(L: *mut lua_State) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_next(L: *mut lua_State, idx: ::std::os::raw::c_int) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_concat(L: *mut lua_State, n: ::std::os::raw::c_int) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_len(L: *mut lua_State, idx: ::std::os::raw::c_int) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_stringtonumber(L: *mut lua_State, s: *const ::std::os::raw::c_char) -> usize {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_getallocf(
    L: *mut lua_State,
    ud: *mut *mut ::std::os::raw::c_void,
) -> lua_Alloc {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_setallocf(L: *mut lua_State, f: lua_Alloc, ud: *mut ::std::os::raw::c_void) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_toclose(L: *mut lua_State, idx: ::std::os::raw::c_int) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_closeslot(L: *mut lua_State, idx: ::std::os::raw::c_int) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_getstack(
    L: *mut lua_State,
    level: ::std::os::raw::c_int,
    ar: *mut lua_Debug,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_getinfo(
    L: *mut lua_State,
    what: *const ::std::os::raw::c_char,
    ar: *mut lua_Debug,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_getlocal(
    L: *mut lua_State,
    ar: *const lua_Debug,
    n: ::std::os::raw::c_int,
) -> *const ::std::os::raw::c_char {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_setlocal(
    L: *mut lua_State,
    ar: *const lua_Debug,
    n: ::std::os::raw::c_int,
) -> *const ::std::os::raw::c_char {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_getupvalue(
    L: *mut lua_State,
    funcindex: ::std::os::raw::c_int,
    n: ::std::os::raw::c_int,
) -> *const ::std::os::raw::c_char {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_setupvalue(
    L: *mut lua_State,
    funcindex: ::std::os::raw::c_int,
    n: ::std::os::raw::c_int,
) -> *const ::std::os::raw::c_char {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_upvalueid(
    L: *mut lua_State,
    fidx: ::std::os::raw::c_int,
    n: ::std::os::raw::c_int,
) -> *mut ::std::os::raw::c_void {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_upvaluejoin(
    L: *mut lua_State,
    fidx1: ::std::os::raw::c_int,
    n1: ::std::os::raw::c_int,
    fidx2: ::std::os::raw::c_int,
    n2: ::std::os::raw::c_int,
) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_sethook(
    L: *mut lua_State,
    func: lua_Hook,
    mask: ::std::os::raw::c_int,
    count: ::std::os::raw::c_int,
) {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_gethook(L: *mut lua_State) -> lua_Hook {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_gethookmask(L: *mut lua_State) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_gethookcount(L: *mut lua_State) -> ::std::os::raw::c_int {
    unimplemented!()
}
#[no_mangle]
pub extern "C" fn lua_setcstacklimit(
    L: *mut lua_State,
    limit: ::std::os::raw::c_uint,
) -> ::std::os::raw::c_int {
    unimplemented!()
}
