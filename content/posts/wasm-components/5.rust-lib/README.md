Build

```
cargo build --manifest-path rust_lib/Cargo.toml --target wasm32-wasip2
jco transpile rust_lib/target/wasm32-wasip2/debug/rust_lib.wasm --instantiation async -o rust_lib_js
```

Run

```
python3 -m http.server 8080
# visit localhost:8080
```