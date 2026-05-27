Init:

```
cargo init --bin rust
```

Compile and run it with `wasmtime`:

```
# Compile it to our WASI 0.2 target
# (remember to build with `--release` for production)
cargo build --manifest-path rust/Cargo.toml --target wasm32-wasip2

# Run it with wasmtime
wasmtime rust/target/wasm32-wasip2/debug/rust.wasm

# Serve and run in browser
python3 -m http.server 8080
```