To build:

```
wasm-tools component embed adder.wit adder.wat -o adder.wasm
wasm-tools component new adder.wasm -o adder.component.wasm
```

To run natively:

```
wasmtime run --invoke 'add(1,200)' adder.component.wasm
```

To run in browser, we generate Javascript bindings, then serve our HTML:

```
jco transpile adder.component.wasm -o adder_js
python3 -m http.server 8080
```

