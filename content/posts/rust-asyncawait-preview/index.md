+++
title = "Rust: Getting started with nightly async/await support"
description = "Async/await syntax is one of the most eagerly anticipated features coming to Rust. Already available in the nightly channel, this post contains my notes on how to get started with this new feature, and how to use it alongside the current Futures based ecosystem."
date = 2018-11-26
[extra]
created = "2018-11-26"
+++

The async/await support coming to Rust brings with it a much more ergonomic way to work with asynchronous computations. In this post I'll introduce `std::future::Future`, and run through how to make use of them, and how to interoperate with the current ecosystem which is built around version `futures::Future` from the `futures` package (version 0.1).

I'll refer to Futures from the 0.1 `futures` package as "0.1 Futures" or "old Futures", and the `Future` trait exposed in the nightly standard library that is the backbone of the async/await syntax as "std Futures" or "new Futures" (or something along that line!).

## Prerequisites

To use the new async/await syntax, you'll need to be using a relatively recent Rust nightly (as of November 26th 2018). An appropriate  `Cargo.toml` looks like this:

```toml
[package]
name = "My Package"
version = "0.1.0"
authors = ["You"]
edition = "2018"

[dependencies]

futures = "0.1.25"

# enable the async-await stuff using a feature flag:
tokio = { version = "0.1.13", features = ["async-await-preview"] }

# Only needs to be explicitly imported if you want to make use of
# the machinery to convert promises back and forth between 0.1 and 0.3
# (which is built into the provided await! macro only otherwise):
tokio-async-await = "0.1.4"
```

For most normal things you'll just need to use `tokio` with the `async-await-preview` feature flag, though the `tokio-async-await` package (parts of which are reexported by `tokio` with said feature enabled) provides a couple of useful bits on its own, including compatibility shims to convert between old and new style Futures.

In the root of your crate you'll need to opt in to some features as well to get the new syntax. The first ones are a must, but I use the others in my example code for a couple of things:

```rust
// enable the await! macro, async support, and the new std::Futures api.
#![feature(await_macro, async_await, futures_api)]
// only needed if we want to manually write a method to go forward from 0.1 to 0.3 future,
// or manually implement a std future (it provides Pin and Unpin):
#![feature(pin)]
// only needed to manually implement a std future:
#![feature(arbitrary_self_types)]
```

The `await_macro` feature defines an `await!` macro that is the main way to wait for Futures to resolve. Tokio exports it's own shim on top of this which works in the same way but is also compatible with 0.1 Futures. This isn't necessary if you avoid ever needing to call `await!` on an old Future, but is useful for interop. We can bring it into scope across our entire crate with:

```rust
#[macro_use]
extern crate tokio;
```

Or bring it into scope in only the modules we want using the standard import syntax in each module we want it:

```rust
use tokio::await;
```

We'll also want to import the `tokio::prelude`, which is beefed up with some additional async/await helper functions when the `tokio-async-await` feature flag is used:

```rust
use tokio::prelude::*;
```

In total, we gain access to these additional methods (I hope I haven't missed any):

```
Stream.next()
Sink.send_async(value)
AsyncRead.read_async(buf)
AsyncRead.read_exact_async(buf)
AsyncWrite.write_async(buf)
AsyncWrite.write_all_async(buf)
AsyncWrite.flush_async()
tokio::run_async(new_future)
tokio::spawn_async(new_future)
```

I'll cover almost all of these in the following examples.

## std::future::Future

With that out of the way, we can make new style futures by using the `async` keyword.

This function returns a `Future<Output=&'static str>` when called:

```rust
async fn hello_world() -> &'static str {
    "Hello World"
}
```

And this block returns the same:

```rust
let hello_world_fut = async { "Hello World" };
```

Worth noting straight away is that new Futures only have a single `Output`, whereas old style Futures have separate `Item` and `Error` types for handling possible failure. If you want to handle errors with new style Futures, you just return a `Result` as that output, which plays nice with things like `?` and so on.

Inside an `async` function, closure or block, you can use the `await!` macro to wait for other futures to resolve. The next example counts to three, one per second:

```rust
// use Delay from the tokio::timer module to sleep the task:
async fn sleep(n: u64) {
    use tokio::timer::Delay;
    use std::time::{Duration, Instant};
    await!(Delay::new(Instant::now() + Duration::from_secs(n))).unwrap();
};

// sleep a second before each line is printed:
tokio::run_async(async {
    await!(sleep(1));
    println!("One");
    await!(sleep(1));
    println!("Two");
    await!(sleep(1));
    println!("Three");
});
```

I'll show more examples as we go, but hopefully you have already gotten a feel for how ergonomically superior async/await syntax is to chaining futures together.

## Converting new style Futures to old style Futures

To make use of new style Futures alongside the various combinators and such exposed on old style Futures, you'll need to convert them. Although not explicitly exposed, you can make use of the machinery in the `tokio-async-await` crate to make quick work of it (I can't guarantee that this API will not change however):

```rust
// converts from a new style Future to an old style one:
fn backward<I,E>(f: impl StdFuture<Output=Result<I,E>>) -> impl futures::Future<Item=I, Error=E> {
    use tokio_async_await::compat::backward;
    backward::Compat::new(f)
};
```

The only caveat here is that the enw style future needs to output a `Result`, so that we can map to the `Item` and `Error` associated types needed for old style futures. With this in hand, we can use a new Future (made by an async thing) as if it was an old one:

```rust
// Map our hello_world() future to return a Result<&str,()> rather
// than just &'str, so that we can convert it to an old style one:
let hello_world_result = async {
    let s = await!(hello_world());
    Ok::<_,()>(s)
};

// use the above function to convert back:
let hello_world_old = backward(hello_world_result);

// We can then run it like any old style future, allowing to to use any
// of the machinery currently available for 0.1 style futures:
tokio::run(
    hello_world_old.map(|val| println!("Running as 0.1 future: {}", val))
);
```

The main use case for this is making use of combinators like `select` and `join` from the land of old Futures, rather than having to reimplement them to work alongside new Futures.

## Converting old style Futures into new style Futures

The easiest way to convert an old style Future into a new one is simply by using the `await!` macro that Tokio gives us (rather than `std::await`), since it will convert old style Futures for us. We've already seen this above using the `tokio::timer::Delay` future in a new style `async` block.

If we want, we can use the same approach to write ourselves a function to manually convert them for us:

```rust
// converts from an old style Future to a new style one:
fn forward<I,E>(f: impl futures::Future<Item=I, Error=E> + Unpin) -> impl StdFuture<Output=Result<I,E>> {
    use tokio_async_await::compat::forward::IntoAwaitable;
    f.into_awaitable()
}
```

We can test that it works by using the `std::await` macro on a converted Future instead of the Tokio version:

```rust
tokio::run_async(async {
    // Create some old style Future:
    let old_future = futures::future::ok::<_,()>("Awaiting a manually converted 0.1 future!");
    // Convert to a new style one:
    let new_future = forward(old_future);
    // `await` the result and print it:
    println!("{}", std::await!(new_future).unwrap());
});
```

I'm not really sure if this is super useful, but it's nice to know that you can easily go back and forth between new and old style Futures.

## Fun with new style Futures

### Manually implementing a new style Future on top of Tokio poll_x methods

In my last post, I manually implemented an old style `Future` which reads one byte at a time from some `AsyncRead` type. Let's do the same again here, but with new style Futures.

Implementing a new style Future isn't significantly more difficult; the main challenges are understanding `Pin`, and converting between the `Async` type Tokio uses and the `std::task::Poll` type that new Futures hand back.

```rust
// expose the std types we need to work with:
use std::task::{ Poll as StdPoll, LocalWaker};
use std::future::Future as StdFuture;
use std::pin::{ Pin, Unpin };

struct ByteFuture<R>(R);

impl <R: AsyncRead + Unpin> StdFuture for ByteFuture<R> {

    // std futures have just one output, but it can be a Result to signal errors:
    type Output = Result<u8, tokio::io::Error>;

    // poll takes a Pin thing now. By requiring that Self is Unpin (which it is
    // so long as the reader thing it contains is), we are allowed mutable access
    // to it (which we need to poll the AsyncReader):
    fn poll(mut self: Pin<&mut Self>, _lw: &LocalWaker) -> StdPoll<Self::Output> {

        let mut buf = [0;1];

        // we need to convert from the Async type returned from the poll_x method
        // to the return type expected from the std Future (it's either Ready
        // or Pending now, since there is no separate Error item any more):
        match self.0.poll_read(&mut buf) {
            Ok(Async::Ready(_num_bytes_read)) => StdPoll::Ready(Ok(buf[0])),
            Ok(Async::NotReady) => StdPoll::Pending,
            Err(e) => StdPoll::Ready(Err(e))
        }
    }

}

let byte_future = ByteFuture(tokio::io::stdin());
```

`byte_future` is now a new style Future that yields one byte of input from `stdin`.

`Pin` is a type that is used to guarantee that we can't move `Self`. This is necessary because new style futures can contain self referential variables (which allows references working across `await` points). If `Self` could be moved, those references would be invalidated.

`Unpin` is a marker trait that means it is safe to move a thing. This being true, it becomes safe to mutably access the thing (which lets you move memory about, eg with `mem::replace`). We need mutable access so that we demand that the `AsyncRead` type is also `Unpin` in order to get it.

We can achieve exactly the same and avoid implementing our own `Future` by making use of the `read_async` method provided on `AsyncRead`:

```rust
let byte_future2 = async {
    let mut buf = [0;1];
    let mut stdin = tokio::io::stdin();
    await!(stdin.read_async(&mut buf))?;
    Ok::<u8,tokio::io::Error>(buf[0])
};
```

This returns a `Future<Output=Result<u8,tokio::io::Error>>` just like our Future implementation above. We also use `?` to bail out early if `read_async` returns with an error. It's great that these things play nice together like this.


## Writing new style streams to work with old-style AsyncReaders

It's probably worth noting that there is (currently at least) no such thing as a new style Stream or Sink. Instead, helper methods are added to each which return new style Futures that we can `await` in order to send and receive input from them.

The simplest way to stream bytes from an `AsyncRead` type is probably to just use the `.read_async` method again, but in a while loop (this would be easy to add better buffering to if we wanted):

```rust
let stream_bytes = async {
    let mut buf = [0;1];
    let mut stdin = tokio::io::stdin();

    // While read_async returns a number of bytes read and not an error:
    while let Ok(n) = await!(stdin.read_async(&mut buf)) {
        // bail if we've read everything:
        if n == 0 { break };
        // stop if the byte we read is a newline:
        if buf[0] == b'\n' { break };
        // print each byte we read up to a newline/error:
        println!("Streamed: {}", buf[0] as char);
    }
};
```

This Future will stream bytes one at a time from `stdin` until it hits a newline, or stdin is closed, printing each byte as a `char` each time one comes through.

If we already have a `Stream`, we can use the `next()` method which has been added to it to pluck items out of it in a standard while loop:

```rust
let mut byte_stream = stream::iter_ok::<_,()>("from an old style stream\n".chars());
let stream_bytes2 = async move {
    // Some(Output), where Output is Result<char, ()>. We bail
    // if the stream ends (None) or the result indicates an error:
    while let Some(Ok(c)) = await!(byte_stream.next()) {
        println!("Streamed 2: {}", c);
    }
};
```

Using `while` loops in this way replaces using stream combinators like `for_each`, as well as error handling combinators like `map_err`; we can just use normal Rust syntax to deal with everything. Lovely stuff!


## Writing new style sinks to work with old-style AsyncWriters

If you don't yet have a `Sink`, Tokio adds `write_async`, `write_all_async` and `flush_async` methods to `AsyncWriter` types, so it becomes easy to put bytes into them without any extra machinery:

```rust
let sink_message = async {
    let message = "writing to stdout, one byte at a time\n";
    let mut stdout = tokio::io::stdout();

    for byte in message.bytes() {
        let buf = &[byte];
        if let Err(e) = await!(stdout.write_all_async(buf)) {
            println!("Error writing out: {:?}", e);
        }
    }

    await!(stdout.flush_async()).unwrap();
}
```

This time, we loop over some bytes we want to output, and send each one to `stdout` using `write_all_async`. We could certainly improve the buffering here by writing more bytes at a time. We need to remember to `flush_async` at the end to ensure bytes have been written out completely.

If you already have a `Sink`, you get a new `send_async` method on it, which makes it easy to send things into it without the messy ownership transferring stuff you have to do with `send`. In this example I forward bytes from a Stream to a Sink:

```rust
// Create a quick stream of bytes:
let mut byte_stream = stream::iter_ok::<_,()>("From a Stream to a Sink\n".bytes());

// Use `FramedWrite` to build ourselves a one-byte-at-a-time Sink to stdout:
let mut byte_sink = FramedWrite::new(tokio::io::stdout(), BytesCodec::new())
    .with(|byte| {
        // convert an incoming byte into the Bytes type expected by the codec:
        Ok::<_, tokio::io::Error>([byte][..].into())
    });

// Running this Future will lead to bytes being forwarded from Stream to Sink:
let forward_bytes = async move {
    while let Some(Ok(byte)) = await!(byte_stream.next()) {
        if let Err(e) = await!(byte_sink.send_async(byte)) {
            println!("Error: {:?}", e);
        }
    }
}
```

## Spawning new style futures to run concurrently

Just like `tokio::spawn` did for old style Futures, `tokio::spawn_async` allows spawning into the background of new style futures, allowing multiple jobs to run concurrently. Remember, we can always convert our new style Futures back into old style ones if we want to use other methods to spawn and run Futures.

Here, we concurrently write the same text to `stdout` multiple times, leaving a good chance (but not a guarantee) that it'll be garbled together:

```rust
tokio::run_async(async {

    async fn write_to_stdout() {
        let mut stdout = tokio::io::stdout();
        let message = "Concurrently Writing This Message";

        for byte in message.bytes() {
            let buf = &[byte];
            await!(stdout.write_all_async(buf)).unwrap();
            await!(stdout.flush_async()).unwrap();
        }
    }

    // execute this future-returning fn multiple times
    // concurrently:
    tokio::spawn_async(write_to_stdout());
    tokio::spawn_async(write_to_stdout());
    tokio::spawn_async(write_to_stdout());
    tokio::spawn_async(write_to_stdout());
    tokio::spawn_async(write_to_stdout());
    tokio::spawn_async(write_to_stdout());

});
```

## Conclusion

I'm really very excited about async/await syntax; it makes writing and composing Futures way easier. This post has discussed how to make use of all of this new stuff alongside the current Futures 0.1 ecosystem, so that you can start playing with and benefitting from it straight away.