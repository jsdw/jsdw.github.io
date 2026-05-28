+++
title = "Working with WebAssembly and WASI 0.2 components"
description = "This post introduces WebAssembly, looks at how we pass data in and out of it, and then looks to the WASI 0.2 standard and WebAssembly component modal as a way to pass more complex data back and forth"
date = 2026-05-28
draft = false
[extra]
created = "2026-05-27"
toc = 1
+++

WebAssembly (or WASM) is a stack-based assembly-like binary language format that is supported across modern browsers, can be executed at near-native speeds, and is a compilation target supported by languages like C/C++ and Rust, allowing native code to be compiled and executed in web browsers. As well as browsers, we have WASM runtimes like [Wasmtime][wasmtime] and [Wasmer](https://wasmer.io/) which can execute WASM binaries outside of browsers.

Before WebAssembly, we had tools like [Emscripten][emscripten] which could compile C into JavaScript to run in browsers, and languages like Elm, Coffeescript and Purescript which were compiled to JavaScript. Being a high level garbage collected language somewhat far from machine code, compiling to JavaScript isn't the ideal target for languages like C and Rust, which have no garbage collector. WebAssembly provides an alternative that is simpler, much closer to machine code, and offers more predictable performance (though not necessarily faster, given that JavaScript has had countless man hours put into optimising it).

I'll be looking at how WebAssembly allows us to interact with it, both in browsers and natively, and how WebAssembly components solve some of the key issues we'll run into.

# Prerequisites

If you'd like to follow along at home, these are the tools that I used in the following examples:

```sh
## Install a runtime for executing WASM (v44.0.1 at present):
cargo install wasmtime-cli

## Install tools for working with WASM (v1.248.0 at present):
cargo install --locked wasm-tools

## Used to transpile WASM component to JS (v1.19.0 at present):
npm install -g @bytecodealliance/jco

## Used to compose components together (v0.10.0 at present):
cargo install wac-cli
```

These assume that the NodeJS and Rust toolchains have been installed already ([go here][installing-node] to install NodeJS or [here][installing-rust] to install Rust). These provide the `npm` and `cargo` commands to install packages respectively.

# Talking to WebAssembly

Aside from having a binary WASM format, WebAssembly has a text based WAT format which is a more human friendly way to view WASM code. I'll use WAT in some examples below.

Below is a basic WebAssembly program printed as WAT. The focus here is on seeing how we can import and export things from WASM code, so I'm skimming over other aspects of the language.

```wat
(module
    ;; require that a function called "log" in the "console" namespace
    ;; is provided, which takes a single i32 and does not return 
    ;; anything.
    (import "console" "log" (func $log (param i32)))

    ;; Define a function to add two i32s
    (func $add (param $a i32) (param $b i32) (result i32)
        local.get $a ;; put $a onto the stack
        local.get $b ;; put $b onto the stack

        ;; take the top two items off the stack ($a and $b), and
        ;; add them together, putting the result back onto the stack
        i32.add      
    )

    ;; Define a function to add two i32s and log the result
    (func $addAndLog (param $a i32) (param $b i32)
        local.get $a ;; put $a onto the stack
        local.get $b ;; put $b onto the stack

        ;; $add takes two arguments from the top of the stack and
        ;; places one result back onto the stack.
        call $add

        ;; $log takes one argument from the top of the stack
        ;; (the result of $add being called) and hands it to
        ;; our imported "console" "log" function.
        call $log
    )

    ;; export (make available to the host) our $addAndLog function
    ;; under the name "addAndLog":
    (export "addAndLog" (func $addAndLog))
)
```

This program expects a function "log" in the "console" namespace to be provided to it, and exports a function called "addAndLog" which requires two 32bit integers and calls the imported function with the result.

Note that being a stack based language, we don't put things into registers (as is the case in many assembly languages); instead we are pushing items onto the top of a stack and then different instructions will pop values off the top as needed. For instance, the `i32.add` instruction will pop the top two values off the top of the stack and place the result of adding them together back onto the top of the stack.

To execute this code, we first need to convert it to the binary WASM format. We can convert between WAT and WASM like so:

```sh
# Convert WAT to WASM:
wasm-tools parse basic.wat > basic.wasm
# Convert WASM to WAT:
wasm-tools print basic.wasm > basic.wat
```

It's worth being aware that if you convert the above WAT to WASM and then back to WAT again the output will be a little different. In this case, when we run `wasm-tools print basic.wasm` to view the WASM that we have just created, we get back:

```wat
(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32 i32) (result i32)))
  (type (;2;) (func (param i32 i32)))
  (import "console" "log" (func $log (;0;) (type 0)))
  (export "addAndLog" (func $addAndLog))
  (func $add (;1;) (type 1) (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.add
  )
  (func $addAndLog (;2;) (type 2) (param $a i32) (param $b i32)
    local.get $a
    local.get $b
    call $add
    call $log
  )
)
```

This is semantically the same, but things like imports and exports will be ordered to the top of the module, functions will have their types defined at the top, and anything written using an S-expression format will be printed in the actual stack-based format that it represents.

For now we'll only try running our WASM file in a browser. For this, let's make a very simple `index.html` file that can run this `basic.wasm` file we've produced:

```html
<!DOCTYPE html>
<html>
<head>
<script type="text/javascript">
async function run() {
    // A Promise to fetch our WASM:
    const wasmPromise = fetch("basic.wasm");

    // The imports that our WASM needs to run:
    const imports = {
        console: {
            log: (arg) => console.log(arg)
        }
    };
    const res = await WebAssembly.instantiateStreaming(wasmPromise, imports)

    // Call our exported addAndLog function:
    res.instance.exports.addAndLog(100, 200);
}

run()
</script>
</head>
<body>
    Open the dev console.
</body>
</html>
```

Here, we provide our "log" function in a "console" namespace as required by our WebAssembly, and then we call the exported `addAndLog` function after instantiating it. We can serve this to try out with something like `python3 -m http.server 8080`.

Our code is just working with one of the basic WebAssembly types, `i32`; a 32bit integer (signed _or_ unsigned). WebAssembly has a few other value types we might use — for instance `i64`, `f32` and `f64` — but it has no types which map to more complex things like strings, lists, variants and structs. Languages which compile to WebAssembly will each decide how to represent these complex types as collections of WASM primitives. For example, a string could be represented as an i32 denoting the length of the string, and then an i32 for each character in the string. Or, it could be represented as an i32 denoting each 4 ASCII bytes in the string and end with a 0 byte to denote when it's finished. Different representations come with different tradeoffs.

# WebAssembly Components

One of the goals of WebAssembly components is to standardise the interface between WebAssembly and the host platform. This includes standardising how to pass complex types back and forth, and standardising the sorts of interfaces that a WebAssembly program can expect to be provided by the host.

There have been other attempts to create such a standard, but I'll be focusing on the [WASI][wasi] (WebAssembly System Interface) here as it appears to be the dominant standard. WASI 0.1 (or "WASI Preview 1") was the first iteration which I have not spent much time looking into. WASI 0.2 (or "WASI preview 2") came next, which introduces the idea of _components_, and represents a significant departure from WASI 0.1. WASI 0.3 is an upcoming release (possibly landing sometime in 2026 or 2027) which iterates on WASI 0.2 and is promised to be a much more incremental update. 

I will be focusing on WASI Preview 2 (or WASI 0.2) components here, as the tooling for this is available today.

WASI 0.2 and WebAssembly components do two things:
1. They define how more complex types will be imported and exported (see the [Canonical ABI][component-abi]).
2. They allow multiple WebAssembly programs to be combined into a single binary such that the exports of one program are used to satisfy the required imports of another.

The first point gives us a standard way to define how complex types like strings, variants and structs are passed in and out of our WebAssembly program. This allows any WASM binary which adheres to this component model to be used with any WASM runtime which supports the component model.

The second point gives us a building block which allows us to create more complex WASM components by composing smaller ones. This is most useful if we want to run WASM using native runtimes like [Wasmtime][wasmtime]; you combine different components to satisfy any required imports before running the final output.

## A Hello World Component

The first thing we need when we create a new WebAssembly component is a _WIT_ (WebAssembly Interface Type) file which defines what the expected imports and exports to our component will be:

```wit
package demo:addcomponent@0.1.0;

interface adder {
    add: func(a: u32, b: u32) -> u32;
}

world example {
    export adder;
}
```

Here, our component is in the "demo" namespace and is called "addcomponent", version 0.1.0. There is a single interface, `adder`, which defines a single add method that works on unsigned 32bit integers. The component itself (via the "world" annotation) exports only this adder interface and requires no imports.

We can satisfy this interface with the following WebAssembly program:

```wat
(module
    (func $add (param $a i32) (param $b i32) (result i32)
        local.get $a
        local.get $b
        i32.add
    )

    (export "demo:addcomponent/adder@0.1.0#add" (func $add))
)
```

This program exports a single _add_ method with a name which corresponds to the "add" method of the "adder" interface of our component.

With our interface and program defined, we now combine them into a single file, and then turn this file into a component.

First, we combine our WAT code (could also be in the form of a WASM binary) with our WIT definition:

```sh
wasm-tools component embed adder.wit adder.wat -o adder.wasm
```

If we were to view this new `adder.wasm` as WAT with `wasm-tools print adder.wasm` we would see that the WIT definitions are now encoded and embedded in a custom field:

```wat
(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (export "demo:addcomponent/adder@0.1.0#add" (func $add))
  (func $add (;0;) (type 0) (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.add
  )
  (@custom "component-type" (after code) "\00asm\0d\00\01\00\00\19\16wit-component-encoding\04\00\07b\01A\02\01A\02\01B\02\01@\02\01ay\01by\00y\04\00\03add\01\00\04\00\1ddemo:addcomponent/adder@0.1.0\05\00\04\00\1fdemo:addcomponent/example@0.1.0\04\00\0b\0d\01\00\07example\03\00\00\00/\09producers\01\0cprocessed-by\01\0dwit-component\070.248.0")
)
```

Next, we convert this WebAssembly core module with embedded WIT into a WebAssembly component:

```sh
wasm-tools component new adder.wasm -o adder.component.wasm
```

WebAssembly components have a different binary format which extends that of WebAssembly core modules and thus cannot be executed by programs expecting basic WebAssembly modules. We can see this if we try viewing our newly created `adder.component.wasm` as WAT with `wasm-tools print adder.component.wasm`:

```wat
(component
  (core module $main (;0;)
    (type (;0;) (func (param i32 i32) (result i32)))
    (export "demo:addcomponent/adder@0.1.0#add" (func $add))
    (func $add (;0;) (type 0) (param $a i32) (param $b i32) (result i32)
      local.get $a
      local.get $b
      i32.add
    )
    (@producers
      (processed-by "wit-component" "0.248.0")
    )
  )
  (core instance $main (;0;) (instantiate $main))
  (type (;0;) (func (param "a" u32) (param "b" u32) (result u32)))
  (alias core export $main "demo:addcomponent/adder@0.1.0#add" (core func $demo:addcomponent/adder@0.1.0#add (;0;)))
  (func $add (;0;) (type 0) (canon lift (core func $demo:addcomponent/adder@0.1.0#add)))
  (component $demo:addcomponent/adder@0.1.0-shim-component (;0;)
    (type (;0;) (func (param "a" u32) (param "b" u32) (result u32)))
    (import "import-func-add" (func (;0;) (type 0)))
    (type (;1;) (func (param "a" u32) (param "b" u32) (result u32)))
    (export (;1;) "add" (func 0) (func (type 1)))
  )
  (instance $demo:addcomponent/adder@0.1.0-shim-instance (;0;) (instantiate $demo:addcomponent/adder@0.1.0-shim-component
      (with "import-func-add" (func $add))
    )
  )
  (export $demo:addcomponent/adder@0.1.0 (;1;) "demo:addcomponent/adder@0.1.0" (instance $demo:addcomponent/adder@0.1.0-shim-instance))
  (@producers
    (processed-by "wit-component" "0.248.0")
  )
)
```

We can see that our new WebAssembly component contains the core WAT that we defined above (albeit tweaked, since the precise format is not preserved), but that it is wrapped in a bunch of additional component related code. One of the notable additions is that the WIT interface that was previously encoded into a custom field in our WASM is now a well defined part of the component binary format. For instance, we can see `u32` types being referenced which are not one of the core WebAssembly types.

Now that we have our component, we can run it using any native runtime that supports components, such as Wasmtime, like so:

```sh
$ wasmtime run --invoke 'add(1,200)' adder.component.wasm
201
```

Alternately, we can run our component in a browser. For this, we first use `jco` to transpile our WASM component back into a core WebAssembly module plus an interface (browsers cannot directly run WASM components):

```sh
jco transpile adder.component.wasm -o adder_js
```

Now we can put a small script in an `index.html` file to run this transpiled component code:

```html
<script type="module">
import { adder } from "./adder_js/adder.component.js"

// Run the add function exposed by our module
console.log("500 + 1 =", adder.add(500, 1));
console.log("123 + 6 =", adder.add(123, 6));
</script>
```

Noting that we have moved to using JavaScript _modules_ to allow us to use the `import` syntax without any extra build steps.

## Component Imports

What if we had a component which required an import as well as exporting something?

As above, let's define the interface we'll want:

```wit
package demo:importing@0.1.0;

interface provider {
    get: func() -> u32;
}

interface adder {
    add: func(other: u32) -> u32;
    mul: func(other: u32) -> u32;
}

world example {
    import provider;
    export adder;
}
```

Our component will require an import of the `provider` interface, which here contains a single `get` method for fetching a `u32`, and it will export an `adder` interface with `add` and `mul` methods. This simple program satisfies this interface:

```wat
(module
    (import "demo:importing/provider@0.1.0" "get" (func $get (result i32)))

    (func $add (param $other i32) (result i32)
        call $get
        local.get $other
        i32.add
    )
    (func $mul (param $other i32) (result i32)
        call $get
        local.get $other
        i32.mul
    )

    (export "demo:importing/adder@0.1.0#add" (func $add))
    (export "demo:importing/adder@0.1.0#mul" (func $mul))
)
```

Let's turn this into a component (we'll call it `importing.component.wasm`):

```sh
wasm-tools component embed importing.wit importing.wat -o importing.wasm
wasm-tools component new importing.wasm -o importing.component.wasm
```

All good so far, but when we try to call one of the exported methods:

```sh
wasmtime run --invoke 'add(1)' importing.component.wasm
```

We run into an error:

```
Error: failed to run main module `importing.component.wasm`

Caused by:
    0: component imports instance `demo:importing/provider@0.1.0`, but a matching implementation was not found in the linker
    1: instance export `get` has the wrong type
    2: function implementation is missing
```

This is because our component requires an import that Wasmtime doesn't know how to satisfy. To satisfy the import, we can create a component whose exports satisfy the missing import:

```wit
package demo:importing@0.1.0;

interface provider {
    get: func() -> u32;
}

world example {
    export provider;
}
```

Then the implementation:

```wat
(module
    (func $get (result i32)
        i32.const 123
    )

    (export "demo:importing/provider@0.1.0#get" (func $get))
)
```

This component exports exactly what we need as an import in our `importing.component.wasm`.

As before, we will turn these types & program into a component (I'll call these `provider.wit` and `provider.wat` respectively):

```sh
wasm-tools component embed provider.wit provider.wat -o provider.wasm
wasm-tools component new provider.wasm -o provider.component.wasm
```

Now, we can use the `wac` tool to plug our new `provider.component.wasm` component into our `importing.component.wasm` to fill in the interface:

```sh
wac plug --plug provider.component.wasm importing.component.wasm -o complete.component.wasm
```

And now we can run our `complete.component.wasm` with Wasmtime:

```sh
$ wasmtime run --invoke 'add(1)' complete.component.wasm
124
```

We can see that this new composed component uses the argument given on the command line, calls into the _provider_ export that we gave it (which returns 123) and adds the two values together.

If a component requires many imports, we can create multiple components which each satisfy some subset of those imports, and plug them all in to satisfy everything.

We've seen how imports can be satisfied in native runtimes like Wasmtime, but how are imports handled in the browser?

## Imports and browser components

As before, we can use the `jco` tool to transpile a component so that it can be used in the browser. We won't use our composed component for this since it no longer requires any imports, and will instead transpile our `importing.component.wasm`, which requires the `provider` interface (the single `get` function):

```sh
jco transpile importing.component.wasm -o importing_js
```

This default command generates Javascript which includes this import:

```js
import { get } from 'demo:importing/provider';
```

Fortunately the `jco` command allows us to map required import paths to something else, so that we can decide which files are imported, like so:

```sh
jco transpile importing.component.wasm \
    --map demo:importing/provider=../importing_provider.js \
    -o importing_js
```

This turns the generated import of `demo:importing/provider` into an import of `../importing_provider.js` (I used `../` because the path that the import is being called from is a folder of generated Javascript, but you can play around and point the imports wherever you need to).

Now, we can create this `importing_provider.js` file that provides the `get` method from our required interface:

```js
export const get = () => {
    return 123
}
```

And then finally we can create an `index.html` file containing this script to run our `importing.component.wasm` component:

```html
<script type="module">
// import the exports from our generated component Javascript:
import { adder } from "./importing_js/importing.component.js"

// Run exported functions:
console.log("123 + 1 =", adder.add(1));
console.log("123 * 2 =", adder.mul(2));
</script>
```

This approach of needing a separate file per imported interface can be cumbersome (especially here as I'd like to keep the examples as compact as possible), but fortunately we can also generate code which needs to be handed an object of imports rather than trying to import files itself:

```sh
jco transpile importing.component.wasm --instantiation async -o importing_js2
```

With this generated `importing_js2` folder, we can create an `index.html` file which handles all of the imports in one place:

```html
<script type="module">
import * as component from "./importing_js2/importing.component.js"

var a;

const { adder } = await component.instantiate(
    // Provide a means to fetch and compile our WASM. Sometimes
    // multiple WASM files are generated.
    (path) => {
        const wasmPromise = fetch(`./importing_js2/${path}`);
        return WebAssembly.compileStreaming(wasmPromise)
    },
    // Provide the expected imports
    {
        "demo:importing/provider": {
            get: () => a
        }
    }
);

a = 500
console.log("500 + 300 =", adder.add(300))
a = 1000
console.log("1000 * 2 =", adder.mul(2))
</script>
```

# Rust WebAssembly Components

Thus far, we've only looked at building very simple components in WAT. Now it's time to move to a higher level language, Rust, so that we can explore how to create and use more complex components.

Historically in Rust, the typical approach to create WebAssembly to use in a browser was:
1. Compile to the `wasm32-unknown-unknown` target (the `unknown`s here signify that we know nothing about the host platform or architecture, and so have no access to things like networking and the filesystem).
2. Use something like `wasm-bindgen` which makes it easier to define an interface between WASM and Javascript, and generates the required Javascript. Or, manually import and export things as if we were writing a C library, and writing the glue code ourselves to pass values in and out.

More recently, Rust now provides a couple of other WASM targets: `wasm32-wasip1` and `wasm32-wasip2`, which support WASI 0.1 and WASI 0.2 respectively. We'll use the `wasm32-wasip2` target alongside the `wit-bindgen` library. The `wasm32-wasip2` target means that Rust code will make use of WASI P2 interfaces as required for things like file and network access as well as CLI features like fetching the arguments or environment variables. This means that some (but not all) of the `std` library in Rust will now be available to programs compiled to WASM. `wit-bindgen` then makes it possible to define custom imports and exports beyond those interfaces provided by WASI 0.2, allowing us to write Rust libraries which can require and export custom interfaces with complex types.

## Hello World, again

The "hello world" example is actually very simple:

```sh
# Add our WASI preview 2 target
rustup target add wasm32-wasip2

# Create a new "hello world" rust binary
cargo init --bin rust
```

This creates a Rust binary crate containing this code:

```rust
fn main() {
    println!("Hello, world!");
}
```

Now we can compile and run it:

```sh
# Compile it to our WASI 0.2 target
# (remember to build with `--release` for production)
cargo build --manifest-path rust/Cargo.toml --target wasm32-wasip2

# Run it with wasmtime
wasmtime rust/target/wasm32-wasip2/debug/rust.wasm
```

Because WASI 0.2 defines stdin, stdout and stderr as a part of its `wasi:cli` interface, the `println` makes use of these interfaces to output via `stdout` as needed. Wasmtime provides implementations of these WASI 0.2 interfaces by default, so WASM components relying on them should _Just Work_.

If we want to see which interfaces some WASM code imports and exports, we can extract the WIT definition from it:

```sh
wasm-tools component wit rust/target/wasm32-wasip2/debug/rust.wasm
```

For this simple "hello world" binary, we see the following:

```wit
package root:component;

world root {
  import wasi:io/poll@0.2.6;
  import wasi:io/error@0.2.6;
  import wasi:io/streams@0.2.6;
  import wasi:cli/environment@0.2.6;
  import wasi:cli/exit@0.2.6;
  import wasi:cli/stdin@0.2.6;
  import wasi:cli/stdout@0.2.6;
  import wasi:cli/stderr@0.2.6;
  import wasi:cli/terminal-input@0.2.6;
  import wasi:cli/terminal-output@0.2.6;
  import wasi:cli/terminal-stdin@0.2.6;
  import wasi:cli/terminal-stdout@0.2.6;
  import wasi:cli/terminal-stderr@0.2.6;

  export wasi:cli/run@0.2.0;
}

package wasi:io@0.2.6 {
  interface poll {
    resource pollable {
      block: func();
    }
  }
  interface error {
    resource error;
  }
  interface streams {
    use error.{error};
    use poll.{pollable};

    resource input-stream;

    resource output-stream {
      check-write: func() -> result<u64, stream-error>;
      write: func(contents: list<u8>) -> result<_, stream-error>;
      blocking-flush: func() -> result<_, stream-error>;
      subscribe: func() -> pollable;
    }

    variant stream-error {
      last-operation-failed(error),
      closed,
    }
  }
}

package wasi:cli@0.2.6 {
  interface environment {
    get-environment: func() -> list<tuple<string, string>>;
  }
  interface exit {
    exit: func(status: result);
  }
  interface stdin {
    use wasi:io/streams@0.2.6.{input-stream};

    get-stdin: func() -> input-stream;
  }
  interface stdout {
    use wasi:io/streams@0.2.6.{output-stream};

    get-stdout: func() -> output-stream;
  }
  interface stderr {
    use wasi:io/streams@0.2.6.{output-stream};

    get-stderr: func() -> output-stream;
  }
  interface terminal-input {
    resource terminal-input;
  }
  interface terminal-output {
    resource terminal-output;
  }
  interface terminal-stdin {
    use terminal-input.{terminal-input};

    get-terminal-stdin: func() -> option<terminal-input>;
  }
  interface terminal-stdout {
    use terminal-output.{terminal-output};

    get-terminal-stdout: func() -> option<terminal-output>;
  }
  interface terminal-stderr {
    use terminal-output.{terminal-output};

    get-terminal-stderr: func() -> option<terminal-output>;
  }
}

package wasi:cli@0.2.0 {
  interface run {
    run: func() -> result;
  }
}
```

As we can see, even a very basic Rust program requires a bunch of different WASI interfaces to be provided. If the code did more complex things like open files or require randomness (for instance to initialise a `HashMap`), it would require more imports.

Wasmtime makes these WASI imports available by default, but we would have to make sure to provide them if we wanted to run this code in the browser. Let's see what that looks like.

## Hello World in the browser

As with our simpler examples, our first step is to transpile our "Hello World" Rust component to JavaScript:

```sh
jco transpile rust/target/wasm32-wasip2/debug/rust.wasm --instantiation async -o rust_js
```

We'll write just enough code to make the `println!` print via `console.log` and otherwise provide stubs for the imports that won't be used. For more complex programs, more of these will need actual implementations. While you can get a feeling for what you need by printing the WIT, viewing [the actual WIT files for WASI 0.2][wasi-wit] is recommended as they are better commented and give a better idea for what any implementations you write need to actually do.

```html
<script type="module">
import * as component from "./rust_js/rust.js"

// Define just enough to print to stdout:
class ConsoleLogStream {
  checkWrite() {
    // Returns how many bytes can be written.
    return BigInt(Number.MAX_SAFE_INTEGER);
  }
  write(contents) {
    // Log the bytes we're given.
    const decoder = new TextDecoder();
    console.log(decoder.decode(contents));
  }
  blockingFlush() {}
  subscribe() {
    // This would block if needed:
    return new Pollable();
  }
}

class Pollable {
    block() {}
}

// Instantiate our component, providing stubs for most of the
// required imports, since they aren't used.
const c = await component.instantiate(
    // Provide a means to fetch and compile our WASM
    (path) => {
        const wasmPromise = fetch(`./rust_js/${path}`);
        return WebAssembly.compileStreaming(wasmPromise)
    },
    // Provide the expected imports.
    {
        'wasi:cli/stdout': {
            getStdout: () => new ConsoleLogStream()
        },
        'wasi:io/poll': {
            Pollable: Pollable
        },
        'wasi:io/streams': {
            OutputStream: ConsoleLogStream,
            InputStream: {},
        },
        // Everything else is an unused stub:
        'wasi:cli/environment': {
            getEnvironment: () => []
        },
        'wasi:cli/exit': {
            exit: () => {}
        },
        'wasi:cli/stderr': {
            getStderr: () => {}
        },
        'wasi:cli/stdin': {
            getStdin: () => {}
        },
        'wasi:cli/terminal-input': {
            TerminalInput: {}
        },
        'wasi:cli/terminal-output': {
            TerminalOutput: {}
        },
        'wasi:cli/terminal-stderr': {
            getTerminalStderr: () => {}
        },
        'wasi:cli/terminal-stdin': {
            getTerminalStdin: () => {}
        },
        'wasi:cli/terminal-stdout': {
            getTerminalStdout: () => {}
        },
        'wasi:io/error': {
            Error: {}
        },
    }
);

// Run our component
c.run.run();

</script>
```

The generated code is good at throwing up an error for any unused imports, so it was fairly easy to follow the errors until everything was stubbed out.

## Creating a library using `wit-bindgen`

While being able to run Rust binaries as WASM is very cool, the main use case I tend to have for WASM is sharing code between the backend and frontend, so I'm more interested in how to create a shared library that can run in the browser, too.

I've created a rather contrived WIT definition just to show off the ability to pass around more complex types; it looks like this:

```wit    
interface item-handling {
    variant item-error {
        not-found,
        other(string)
    }

    enum item-type {
        file,
        folder,
    }

    record item {
        ty: item-type,
        name: string,
    }

    resource items {
        constructor();
        get: func(name: string) -> result<item, item-error>;
    }
}

world example {
    use item-handling.{ item };

    import set-items: func() -> list<item>;

    export item-handling;
}
```

The idea is that we'll write a WASM component that needs to be handed a function (`set-items`) which, when called, returns a list of `item`. It will then export an `item-handling` interface containing an opaque `items` type which can be constructed, which has a `get` method to get items given some `string` key (or return an `item-error` is there is some problem). This small WIT interface show cases how complex types can be passed back and forth between WASM component and host. Let's implement it!

First, let's create a new Rust library:

```sh
cargo init --lib rust_lib
```

We need to export this as a `cdylib` and pull in `wit-bindgen`, so we add this to the generated `Cargo.toml` file:

```toml
[lib]
crate-type = ["cdylib"]

[dependencies]
# Latest version at the time of writing:
wit-bindgen = "0.57.1"
```

Now, we edit our `lib.rs` file. To keep everything in one place, I'll embed the interface with the Rust code via `wit-bindgen`s `generate!` macro, but you can define the WIT separately too if you prefer. Here's the code:

```rust
use std::collections::HashMap;

mod bindings {
    wit_bindgen::generate!({
        inline:r#"
            package example:component@0.1.0;
    
            interface item-handling {
                variant item-error {
                    not-found,
                    other(string)
                }
    
                enum item-type {
                    file,
                    folder,
                }
    
                record item {
                    ty: item-type,
                    name: string,
                }
    
                resource items {
                    constructor();
                    get: func(name: string) -> result<item, item-error>;
                }
            }
    
            world example {
                use item-handling.{ item };
    
                // we'll want to be able to get items
                // from the host as needed.
                import set-items: func() -> list<item>;
    
                // export the item functionality
                export item-handling;
            }
        "#
    });

    // Boilerplate: `super::Component` is implementing our interfaces.
    type C = super::Component;
    export!(C);
}

// Anything exported from our component lives in an `exports` mod.
use bindings::exports::example::component as exports;
// Anything imported lives at the root.
use bindings::example::component as imports;

use exports::item_handling::{ 
    Item as ExportedItem,
    ItemError as ExportedItemError,
    ItemType as ExportedItemType
};
use imports::item_handling::{
    Item as ImportedItem,
    ItemType as ImportedItemType,
};

// Create a type which will implement our `items` resource:
struct MyItems {
    items: HashMap<String, ExportedItem>,
}

impl exports::item_handling::GuestItems for MyItems {
    // This is called when `items.constructor()` is called from the host.
    fn new() -> MyItems {
        let items = bindings::set_items();
        MyItems {
            items: items
                .into_iter()
                .map(|item| (item.name.clone(), to_exported_item(item)))
                .collect()
        }
    }

    // This is called when `items.get()` is called from the host.
    fn get(&self, name: String) -> Result<ExportedItem, ExportedItemError> {
        let item = self.items.get(&name).ok_or(ExportedItemError::NotFound)?;
        Ok(item.clone())
    }
}

// Different types are generated for exports and imports even if they
// are conceptually the same, so here we convert from import to export
// and work internally with the export types.
fn to_exported_item(item: ImportedItem) -> ExportedItem {
    ExportedItem {
        name: item.name,
        ty: match item.ty {
            ImportedItemType::File => ExportedItemType::File,
            ImportedItemType::Folder => ExportedItemType::Folder,
        }
    }
}

// Create a type to represent our top level component, which will implement
// all exported interfaces.
struct Component;

// Define any resources and methods that we export in our `item-handling` interface.
// here, we only need to define what will implement the `items` resource, since we
// don't have any methods in this interface.
impl exports::item_handling::Guest for Component {
    type Items = MyItems; 
}
```

There is plenty of boilerplate to work through here. 

Concrete types in the WIT definition are auto generated separately for any imports and exports (so we end up with two `item` types here of the same shape, which I named `ImportedItem` and `ExportedItem`, since we both import and export this type). Resources and methods that are exported need their implementations to be defined, and we do that by implementing all of the generated traits. Imports are generated at the top level and can be called as needed (the implementations are provided by the host); we do that here when we call `bindings::set_items` to pull in a list of items from the host which we will use.

We use the same commands and approach that we are now familiar with to transpile this for JS; `wit-bindgen` and compiling to the `wasm32-wasip2` target handle the rest for us:

```
cargo build --manifest-path rust_lib/Cargo.toml --target wasm32-wasip2
jco transpile rust_lib/target/wasm32-wasip2/debug/rust_lib.wasm --instantiation async -o rust_lib_js
```

As with our Rust binary, we need to now provide all of the relevant imports when we load this component in a browser. Our `index.html` file can contain something like this to make the above work:

```html
<script type="module">
import * as component from "./rust_lib_js/rust_lib.js"

const c = await component.instantiate(
    // Provide a means to fetch and compile our WASM
    (path) => {
        const wasmPromise = fetch(`./rust_lib_js/${path}`);
        return WebAssembly.compileStreaming(wasmPromise)
    },
    // Provide the expected imports.
    {
        // This is the import we defined; a single function to pass items in.
        "set-items": {
            default: () => [
                {
                    name: "my_dir",
                    ty: "folder"
                },
                {
                    name: "Foo.txt",
                    ty: "file"
                }
            ]
        },
        // This is called once from Rust to initialise the HashMap with randomness.
        'wasi:random/insecure-seed': {
            insecureSeed: () => {
                return [0, 0]
            }
        },
        // Stubs for WASI imports that our component is asking for but doesn't use.
        'wasi:cli/environment': {
            getEnvironment: () => {}
        },
        'wasi:cli/exit': {
            exit: () => {}
        },
        'wasi:cli/stderr': {
            getStderr: () => {}
        },
        'wasi:cli/stdin': {
            getStdin: () => {}
        },
        'wasi:cli/stdout': {
            getStdout: () => {}
        },
        'wasi:cli/terminal-input': {
            TerminalInput: {}
        },
        'wasi:cli/terminal-output': {
            TerminalOutput: {}
        },
        'wasi:cli/terminal-stderr': {
            getTerminalStderr: () => {}
        },
        'wasi:cli/terminal-stdin': {
            getTerminalStdin: () => {}
        },
        'wasi:cli/terminal-stdout': {
            getTerminalStdout: () => {}
        },
        'wasi:io/error': {
            Error: {}
        },
        'wasi:io/poll': {
            Pollable: {}
        },
        'wasi:io/streams': {
            InputStream: {},
            OutputStream: {},
        },
    }
);

// This calls `<MyItems as exports::item_handling::GuestItems>::new()`:
const items = new c.itemHandling.Items()

// Now we can call the `get` method defined on our `item` resource:
console.log(items.get("my_dir"));
console.log(items.get("Foo.txt"));
</script>
```

If we serve this via `python3 -m http.server 8080` and visit `http://localhost:8080`, we'll see two objects logged to the console:

```
Object { ty: "folder", name: "my_dir" }
Object { ty: "file", name: "Foo.txt" }
```

This demonstrates that we've successfully passed complex types in and out of our WebAssembly component, where `wit-bindgen` has defined the correct ABI on the Rust side, and `jco` has transformed the WIT types to/from suitable JS types as needed.

As with other components, we could also run this component in Wasmtime if we plugged in some other component which satisfied the required import, but I'll leave that as an exercise for the reader.

# Further investigation

There is still a lot that I have not looked into regarding WebAssembly components, but needs more exploration:

1. If I create a resource in JavaScript, how do I then "drop" it to clean up when I no longer need it? Offhand I cannot see any explicit logic for this, and so we probably need to export a `consume`/`drop` method on any resource owned by the host (often the web browser) to allow the host to manually clean up when done.
2. How large does the generated JavaScript get? `jco` generated ~6000loc of JavaScript for my simple example above. It is worth keeping an eye on this and taking steps to optimize it if possible.
3. What is the performance like? Passing values from/to WebAssembly is inherently going to be less performant as they need to be converted to/from the shapes that the guest and host expect (ie strings are represented as utf16 in JavaScript and utf8 in Rust). WebAssembly components introduce an intermediate ABI which means transforming types into this ABI representation and then from that into the target representation, likely adding extra overhead.
4. Are there any competing approaches to pay more attention to? How long until we get WASI 1.0? I would always aim to keep the `wit-bindgen` layer as small as possible and separate from core logic so that it can be swapped with something else if needed in the future.
5. Defining stub WIT interfaces in JavaScript is a bit cumbersome but also very flexible as it gives you complete control over what your WASM program is able to do. There are also shims available like [this one][wasi-shim] which are worth looking more into for real projects.

# Final Thoughts

We started small to explore how things are imported and exported to WebAssembly, moving on to WebAssembly components and WASI 0.2 to standardise how to interact with some piece of WebAssembly, and finally used Rust to see how we can use these ideas to create complex components that can pass complex types back and forth, and be shared across browser and native WebAssembly runtimes.

While other approaches exist to share Rust code with the browser, such as `wasm-bindgen`, I am excited by WASI 0.2 and beyond as a standard way to have compelx interfaces between WebAssembly and native code. I could see this being very useful for instance in allowing WebAssembly based plugins to be written for some piece of software, or simply for sharing more complex native code between browser and backend (particularly if you're using languages like Rust which has great support for compiling to these new WebAssembly targets).

Finally, thank you for reading!


[wasmtime]: https://github.com/bytecodealliance/wasmtime
[wasmer]: https://wasmer.io/
[emscripten]: https://emscripten.org/
[installing-node]: https://nodejs.org
[installing-rust]: https://rust-lang.org/tools/install/
[component-abi]: https://component-model.bytecodealliance.org/advanced/canonical-abi.html
[wasi]: https://github.com/WebAssembly/WASI
[wasi-wit]: https://github.com/WebAssembly/WASI/tree/main/proposals
[wasi-shim]: https://www.npmjs.com/package/@bytecodealliance/preview2-shim
