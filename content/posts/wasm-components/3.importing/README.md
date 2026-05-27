Component components and running the result:

```
# Create component requiring import:
wasm-tools component embed importing.wit importing.wat -o importing.wasm
wasm-tools component new importing.wasm -o importing.component.wasm

# Create component satisfying the import:
wasm-tools component embed provider.wit provider.wat -o provider.wasm
wasm-tools component new provider.wasm -o provider.component.wasm

# Plug the latter into the former to satisfy import:
wac plug --plug provider.component.wasm importing.component.wasm -o complete.component.wasm

# now we can run the composed component:
wasmtime run --invoke 'add(1)' complete.component.wasm
```

Browser components using imports:

```
jco transpile importing.component.wasm \
    --map demo:importing/provider=../importing_provider.js \
    -o importing_js

python3 -m http.server 8080
```