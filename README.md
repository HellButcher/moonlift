# `moonlift`

[![license: MIT/Apache 2.0](https://img.shields.io/badge/license-MIT%2FApache--2.0-blue.svg)](#license)
[![Rust CI](https://github.com/HellButcher/moonlift/actions/workflows/rust.yml/badge.svg)](https://github.com/HellButcher/moonlift/actions/workflows/rust.yml)

A pure-rust implementation of *lua* (5.4) with *just-in-time* (JIT) compilation using `cranelift`.

## WARNING

This project is in a _very early_ development stage.

## Differences to the original C implementation of Lua 5.4

- All string operations are UTF-8 by default.
  The original C implementation is encoding-transparent.
- Unicode-Escape Sequences (`\uXXX`) must produce valid [unicode scalar values](https://www.unicode.org/glossary/#unicode_scalar_value) / code-points.
  The original C implementation only requires the code-point to be less then 2^31
- ...

## License

[license]: #license

This project is licensed under either of

* MIT license ([LICENSE-MIT] or <http://opensource.org/licenses/MIT>)
* Apache License, Version 2.0, ([LICENSE-APACHE] or <http://www.apache.org/licenses/LICENSE-2.0>)

at your option

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

[LICENSE-MIT]: LICENSE-MIT
[LICENSE-APACHE]: LICENSE-APACHE
