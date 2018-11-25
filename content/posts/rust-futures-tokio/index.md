+++
title = "Rust: Converting AsyncRead and AsyncWrite to Futures, Sinks and Streams"
description = "Recently I had another shot at using Futures and Tokio again in Rust. One of the main challenges I faced was how to work with things that implemented the AsyncRead and AsyncWrite traits. So, here are some ways to 'upgrade' them into Futures, Sinks and Streams to gain access to the various combinators provided by them."
date = 2018-11-24
[extra]
created = "2018-11-24"
+++

Recently, I started having a go at the [boundvariable][boundvariable] programming challenge (you can find my results [here][jsdw-boundvariable], but there be spoilers!).

The first step in the challenge is to write an interpreter. You're given the spec that you need to adhere to. The interpreter that you create is capable of being fed ASCII input one byte at a time, as well as handing back ASCII output one byte at a time. I eventually decided that it would be nice if my interpreter could allow TCP connections to be established to it; input from these connections would be sent into the interpreter, and output from the interpreter would be sent to any current TCP connections as well as to stdout. This seemed like a good opportunity to have anothe go with _Tokio_; an asynchronous IO framework for Rust.

Prior to this experience, I had thought that Futures, Sinks and Streams were the smallest building blocks in the world of Tokio, and so I went looking for these things to read and write my bytes for me. Actually, all of the fundamental objects to read and write bytes to things (in my case I was only interested in [`tokio::io::Stdout`][tokio-stdout], [`tokio::io::Stdin`][tokio-stdin] and [`tokio::net::TcpStream`][tokio-tcpstream]) implement one or both of `AsyncRead` and `AsyncWrite`, but not the `Future`, `Sink` or `Stream` traits. In fact, there are lots of `poll_x` methods dotted around, so I realised I needed to figure out how to make use of them.

My goal was simple—reading and writing single bytes at a time—so that is the focus of my example code, but adding things like buffering, or encoding/decoding the bytes into more complex structures all felt much more achievable once I grasped the basics! I'll look at each of the possible ways to convert these things:

- `AsyncRead` to `Future`, for one-off reads
- `AsyncRead` to `Stream`, for continuous reading
- `AsyncWrite` to `Sink`, for continuous writing
- `AsyncWrite` to `Future`, for one-off writing

Complete code samples can be found [here][code].

So, let's begin!

## Converting an `AsyncRead` to a `Future`, for one-off reads

The most verbose way to turn a thing implementing AsyncRead into a future that emits a single byte when completed is by manually implementing the `Future` trait on an appropriate type to do this for us:

```rust
struct ByteFuture<R>(R);

// As long as R implements AsyncRead, ByteFuture<R> is
// a valid Future:
impl <R: AsyncRead> Future for ByteFuture<R> {

    // We want to get a single byte back:
    type Item = u8;

    // If things go wrong, it'll be a tokio::io::Error:
    type Error = io::Error;

    // Each time the future is polled, we run the poll_read method
    // on the AsyncReader, to try and read a single byte into a buffer.
    // If we succeed, we give back the byte, otherwise we say we're not
    // ready yet:
    fn poll(&mut self) -> Result<Async<u8>, io::Error> {
        let mut buf = [0;1];
        match self.0.poll_read(&mut buf) {
            Ok(Async::Ready(_num_bytes_read)) => Ok(Async::Ready(buf[0])),
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(e) => Err(e)
        }
    }

}

// Now we can use the above to create a future that
// will resolve to a single byte read from stdin:
let byte_future = ByteFuture(io::stdin());
```

The nice thing about implementing the `Future` yourself is that you have complete control over things like buffering, errors and such. You can also replace `poll_read` with any similar `poll_x` method you find to create a Future that resolves when it returns something.

How does anything know when to call `poll` again if `NotReady` is returned? It is assumed that the `poll_read` method called inside it will notify the Task containing this future (a Task is just a future that has been handed off to `tokio` to be run) once it's ready to be tried again. Without this, the `poll` method would never be called again, so if you return `NotReady`, it should be because an inner `poll_x` method also returned `NotReady`, so you can be sure that something will try again. Another offshoot of this is that it is assumed that `poll_x` methods are called in the context of a running Task so that they can notify the task when it needs to try again. In other words, don't use any `poll` method outside of a Future.

One step less verbose is using the `futures::future::poll_fn` helper function, which transforms any poll method into an ad-hoc future on the fly, without the need for custom types and such. This achieves the same as above, reading from `stdin`:

```rust
let byte_future = future::poll_fn(move || {
    let mut buf = [0;1];
    match io::stdin().poll_read(&mut buf) {
        Ok(Async::Ready(_num_bytes_read)) => Ok(Async::Ready(buf[0])),
        Ok(Async::NotReady) => Ok(Async::NotReady),
        Err(e) => Err(e)
    }
});
```

Note how the method is practically identical to the `poll` implementation of the future. By making the closure passed to the `poll_fn` method `move`, you can pass in and make available whatever state you need for buffering and things.

Using the `try_ready!` macro, which is a bit like `try!`/`?` but also returns if the thing passed to it returns `Ok(Async::NotReady)`, we can shorten the above slightly (though you may need to provide explicit type information because the error can be converted):

```rust
let byte_future = future::poll_fn(move || {
    let mut buf = [0;1];
    let _num_bytes_read = try_ready!(io::stdin().poll_read(&mut buf));
    Ok(Async::Ready(buf[0]))
});
```

We're down to just 4 lines of code now, but it turns out we can do even better by making use of the handy `tokio::io::read` helper:

```rust
let byte_future = io::read(io::stdin(), [0;1])
        .map(|(_stdin,buf,_num_bytes_read)| buf[0]);
```

This helper consumes an `AsyncRead`er and a buffer, but fortunately gives back the buffer (and how many bytes were read into it) when it resolves, so it's easy to map this result to the single byte we're interested in.

## Converting an `AsyncRead` to a `Stream`, for continuous reading

A `Stream` is very similar to a `Future`, except that it can keep yielding items indefinitely. Once again, we can create a thing that takes an `AsyncRead` and implements `Stream` for us:

```rust
struct ByteStream<R>(R);

impl <R: AsyncRead> Stream for ByteStream<R> {

    // The same as our future above:
    type Item = u8;
    type Error = io::Error;

    // poll is very similar to our Future implementation, except that
    // it returns an `Option<u8>` instead of a `u8`. This is so that the
    // Stream can signal that it's finished by returning `None`:
    fn poll(&mut self) -> Result<Async<Option<u8>>, io::Error> {
        let mut buf = [0;1];
        match self.0.poll_read(&mut buf) {
            Ok(Async::Ready(n)) => {
                // By convention, if an AsyncRead says that it read 0 bytes,
                // we should assume that it has got to the end, so we signal that
                // the Stream is done in this case by returning None:
                if n == 0 {
                    Ok(Async::Ready(None))
                } else {
                    Ok(Async::Ready(Some(buf[0])))
                }
            },
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(e) => Err(e)
        }
    }
}
let byte_stream1 = ByteStream(io::stdin());
```

This is very similar to the `Future` implementation, except that we make a note of when the `AsyncRead` has made it to the end and signal that the `Stream` is done too.

Once again, we have a helper function that we can use to implement a `Stream` in a more ad-hoc way:

```rust
let byte_stream = stream::poll_fn(move || {
    let mut buf = [0;1];
    match io::stdin().poll_read(&mut buf) {
        Ok(Async::Ready(n)) => {
            if n == 0 {
                Ok(Async::Ready(None))
            } else {
                Ok(Async::Ready(Some(buf[0])))
            }
        },
        Ok(Async::NotReady) => Ok(Async::NotReady),
        Err(e) => Err(e)
    }
});
```

This is almost a copy of the `poll` method above, just like our usage of `futures::poll_fn` above.

If you wonder whether there is a way to put the nice `io::read` helper function to use again, but this time for a Stream, there is! We can combine it with the `stream::unfold` function to convert a future into a stream of futures:

```rust
let byte_stream = stream::unfold((), |_| {
    let next = io::read(io::stdin(), [0;1]).map(|(_stdin,buf,n)| (buf[0], ()));
    Some(next)
});
```

This has a notable downside however; there is no way to decide whether to return or not after running the `io::read`, and so I can't signal that the reader has reached the end. This is fine however for something like `stdin`, which you might expect never to be closed, but no good for streaming file data and such.

A nicer, and even mroe concise approach, is to use functionality from the `tokio::codec` module, which allows you to describe how to encode and decode bytes from `AsyncRead`/`AsyncWrite` things, and wraps them into Streams and Sinks for you. This is how we could make our one-byte-at-a-time Stream:

```rust
let byte_stream = codec::FramedRead::new(io::stdin(), codec::BytesCodec::new())
    // convert our bytes buffer into a stream that emits one byte at a time:
    .map(|bytes| stream::iter_ok::<_, io::Error>(bytes))
    // flatten our stream of streams down into one stream:
    .flatten();
```

If we were happy being fed a Stream of byte buffers, we'd be finished in one line, but because we want a stream of bytes we map each buffer into a stream of bytes and then flatten the resulting stream-of-streams into one stream. I would also expect this approach to be faster than the previous ones as well, since it reads more than one byte at a time from the underlying reader.

To do more complex encoding and decoding, implement `tokio::codec::Decode` and/or `tokio::codec::Encode` on some type, and then pass that in instead of `BytesCodec` to make use of it.

## Converting an `AsyncWrite` to a `Sink`, for continuous writing

Implementing `Sink` is a little harder than implementing `Stream`, because writing to a Sink is a two-stage process; first you begin sending data to the Sink (which might queue it up in a buffer), and then you flush the data out to the Sink to ensure it has all been written out. Here's a simple one-byte-at-a-time implementation:

```rust
struct ByteSink<W>(W);

impl <W: AsyncWrite> Sink for ByteSink<W> {

    // We want to send single bytes to the sink:
    type SinkItem = u8;

    // An error will be of this type:
    type SinkError = io::Error;

    // This is called to provide an item to the Sink. We might want to
    // push it to a buffer here, but to keep things simple we just forward
    // it on to the underlying `AsyncWrite` by calling `poll_write`. The item
    // is returned if nothing can be done with it yet, which is why the return
    // type is a little different here:
    fn start_send(&mut self, item: u8) -> Result<AsyncSink<u8>, io::Error> {
        match self.0.poll_write(&[item])? {
            Async::NotReady => Ok(AsyncSink::NotReady(item)),
            Async::Ready(_) => Ok(AsyncSink::Ready)
        }
    }

    // This is called after potentially multiple calls to `start_send`. Its goal is
    // to flush the data out to ensure it's been fully written.
    fn poll_complete(&mut self) -> Result<Async<()>,io::Error> {
        match self.0.poll_flush()? {
            Async::Ready(_) => Ok(Async::Ready(())),
            Async::NotReady => Ok(Async::NotReady)
        }
    }
}

// Convert our stdout `AsyncWrite` into a single-byte Sink:
let byte_sink = ByteSink(io::stdout());
```

There's room for improvement here, for instance I don't attempt to buffer anything at all, and rely on the underlying `AsyncWrite` to be performant for me. As with implementing `Future`s and `Stream`s, you must only return `NotReady` if the underlying writer did, to be sure that the underlying writer will wake the task up and call these methods again when appropriate. Failure to do so will lead to your implementation hanging forever as nothing knows to try it again.

As with our `Stream` implementation, `tokio::codec` comes to the rescue again to give us a significantly more concise way to implement the above:

```rust
let byte_sink = codec::FramedWrite::new(io::stdout(), codec::BytesCodec::new())
    .with(|byte| {
        // convert an incoming byte into the Bytes type expected by the codec:
        Ok::<_, io::Error>([byte][..].into())
    });
```

As with the above implementation it does not attempt to do any buffering. the `with` method is a bit like `map` in reverse; instead of mapping the value coming out of the `Stream`/`Future`, we map the value before it comes into the `Sink`, to turn it into the required type.

There seem to be fewer ways to convert an `AsyncWrite` into a `Sink`, I suppose because it is a little more complex.

## Converting an `AsyncWrite` to a `Future`, for one-off writing

For completeness sake, I include the final of the possible conversions; one-off writes to an `AsyncWrite` which leads to a single `Future`. Once you have a `Sink`, this can be done by using the `send` or `send_all` methods available on `Sink`. An alternative if you only have an `AsyncWrite` is to use the `tokio::io::write_all` helper function:

```rust
let write_once = io::write_all(io::stdout(), &[b'x']);
```

`write_once` is a Future that resolves when the bytes provided are written and flushed to the output.

## Conclusion

Hopefully I've managed to shine some light on how to work with `AsyncRead`/`AsyncWrite` things. I've demonstrated various ways to "upgrade" things that have `poll_x` methods into an appropriate type (be it a Future, Sink or Stream), which will hopefully make them easier to work with!

All of the code used above is [available here][code].

[boundvariable]: http://boundvariable.org
[jsdw-boundvariable]: https://github.com/jsdw/boundvariable
[AoC]: https://adventofcode.com/
[tokio-stdin]: https://docs.rs/tokio/0.1.13/tokio/io/struct.Stdin.html
[tokio-stdout]: https://docs.rs/tokio/0.1.13/tokio/io/struct.Stdout.html
[tokio-tcpstream]: https://docs.rs/tokio/0.1.13/tokio/net/struct.TcpStream.html
[code]: https://github.com/jsdw/jsdw.me/blob/master/content/posts/rust-futures-tokio/src/main.rs