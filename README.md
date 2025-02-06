# `moonlift`

[![license: MIT/Apache 2.0](https://img.shields.io/badge/license-MIT%2FApache--2.0-blue.svg)](#license)
[![Rust CI](https://github.com/HellButcher/moonlift/actions/workflows/rust.yml/badge.svg)](https://github.com/HellButcher/moonlift/actions/workflows/rust.yml)

A pure-rust implementation of *lua* (5.4) with *just-in-time* (JIT) compilation using `cranelift`.

---

## Installation Instructions

Follow these steps to install and set up the project:

1. **Install Rust:**

   Run the following command to install Rust using `rustup`:

   ```bash
   curl https://sh.rustup.rs -sSf | sh
   ```

   After the installation, configure Rust in your environment. This usually involves updating your shell configuration:

   ```bash
   # For sh/bash/zsh/ash/dash/pdksh users:
   . "$HOME/.cargo/env"
   
   # For fish users:
   source "$HOME/.cargo/env.fish"
   ```

2. **Initialize Submodules:**

   Update and initialize the git submodules:

   ```bash
   git submodule update --init --recursive
   ```
  
3. **Build the Project:**

   Compile the project using Cargo. You can choose between a debug build or a release build:

   ```bash
   # Debug build
   cargo build

   # Release build
   cargo build --release
   ```

4. **Run Tests:**

   Execute the tests to verify the installation:

   ```bash
   cargo test
   ```

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
