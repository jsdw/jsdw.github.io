// enable the await! macro, async support, and the new std::Futures api.
#![feature(await_macro, async_await, futures_api)]
// only needed if we want to manually write a method to go forward from 0.1 to 0.3 future,
// or manually implement a std future (it provides Pin and Unpin):
#![feature(pin)]
// only needed to manually implement a std future:
#![feature(arbitrary_self_types)]

// Using the await! macro exported from tokio instead of the standard one
// we're given basically allows you to await! old-style 0.1 Futures as well
// ("#[macro use] extern crate tokio" imports this project-wide for you):
use tokio::await;

// With the async/await feature enabled in Tokio, this also imports a
// couple of extension traits which add convenience methods to things:
use tokio::prelude::*;

use tokio::codec::{FramedWrite, BytesCodec};

// Make some std things more readily accessible for our demo stuff. We
// don't need any of this to just use 0.1 or std futures:
use std::task::{ Poll as StdPoll, LocalWaker};
use std::future::Future as StdFuture;
use std::pin::{ Pin, Unpin };

// New methods we get:
// - Stream.next()
// - Sink.send_async(value)
// - AsyncRead.read_async(buf)
// - AsyncRead.read_exact_async(buf)
// - AsyncWrite.write_async(buf)
// - AsyncWrite.write_all_async(buf)
// - AsyncWrite.flush_async()
// - tokio::run_async(new_future)
// - tokio::spawn_async(new_future)

fn main() {

    // ####################################
    // ### Intro to std::future::Future ###
    // ####################################

    // In the new world of async/await, we can make a function that
    // return a Future by prepending "async" to it (this allows you to
    // use await! inside it). Note that std::future::Future's have just
    // one output, as opposed to futures::Future which can output either
    // an Item or an Error (but that one output can be a Result!):
    async fn hello_world() -> &'static str {
        "Hello World 1"
    }
    let hello_world1 = hello_world();

    // We can also make blocks of code async, which once again means that
    // they return a future, and no longer run immediately:
    let hello_world2 = async { "Hello World 2" };

    // The easiest way to run these new Future things is by sticking them
    // into an async block and await!-ing them, like so. await! waits for
    // the future to hand back a value and then resolves to that value.
    // Tokio can get on with other things while it's awaiting something.
    // await! is the modern equivalent to `.and_then`:
    tokio::run_async(async {
        println!("{}", await!(hello_world1));
        println!("{}", await!(hello_world2));
    });

    // Showing await working with a "real" future; this will wait a second before
    // returning:
    async fn sleep(n: u64) {
        use tokio::timer::Delay;
        use std::time::{Duration, Instant};
        await!(Delay::new(Instant::now() + Duration::from_secs(n))).unwrap();
    };

    tokio::run_async(async {
        await!(sleep(1));
        println!("One");
        await!(sleep(1));
        println!("Two");
        await!(sleep(1));
        println!("Three");
    });

    // #############################################################
    // ### Converting std::future::Future to 0.1 futures::Future ###
    // #############################################################

    // By using tokio_async_await bits, we can write a function which
    // converts std::future::Future back into the 0.1 futures::Future.
    // However, there's a catch. std::future::Future only has 1 output,
    // but 0.1 futures output both an Item or an Error. So, we can only
    // convert back std::Futures that return a Result:
    fn backward<I,E>(f: impl StdFuture<Output=Result<I,E>>) -> impl futures::Future<Item=I, Error=E> {
        use tokio_async_await::compat::backward;
        backward::Compat::new(f)
    };

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

    // ###############################################################
    // ### Converting 0.1 futures::Future to a std::future::Future ###
    // ###############################################################

    // Notice that 0.1 style futures can be `await!`ed too, because the await!
    // macro that Tokio exports knows how to convert them to std futures. This
    // is the easiest way to convert old to new style futures.
    tokio::run_async(async {
        let old_future = futures::future::ok::<_,()>("Awaiting a 0.1 future!");
        println!("{}", await!(old_future).unwrap());
    });

    // We can manually convert 0.1 to std futures as well if we like, using
    // the same machinery that tokio::await uses, though I can't really see
    // why you'd want to do this (since tokio::await! can do it for you):
    fn forward<I,E>(f: impl futures::Future<Item=I, Error=E> + Unpin) -> impl StdFuture<Output=Result<I,E>> {
        use tokio_async_await::compat::forward::IntoAwaitable;
        f.into_awaitable()
    }

    // As above, except that we manually convert the future from a 0.1 to a std one
    // and then use the std::await macro instead of the tokio shim to prove that the
    // forward conversion worked:
    tokio::run_async(async {
        let old_future = futures::future::ok::<_,()>("Awaiting a manually converted 0.1 future!");
        println!("{}", std::await!(forward(old_future)).unwrap());
    });

    // ##########################################
    // ### The "new" way to do various things ###
    // ##########################################

    // Writing new style futures to work with old-style tokio poll_x funcs
    // ===================================================================

    // Tokio still uses 0.1 style futures under the hood, so if you need to interact with
    // poll_x methods and such, you can still write old style futures as you have done,
    // and just use tokio::await! to treat them like new style ones.
    //
    // If you do choose to write modern future implementations, you'll have to convert between
    // the Poll types returned by poll_x methods and the new way. Here's a future which reads
    // a single byte from an AsyncRead:
    struct ByteFuture<R>(R);
    impl <R: AsyncRead + Unpin> StdFuture for ByteFuture<R> {

        // std futures have just one output, but it can be a Result to signal errors:
        type Output = Result<u8, tokio::io::Error>;

        // poll takes a Pin thing now. By requiring that Self is Unpin (which it is
        // so long as the reader thing it contains is), we allow mutable access to it
        // (which we need to read from the AsyncReader):
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

    // We can now run our std Future in an async block:
    tokio::run_async(async {
        let byte = await!(byte_future).expect("cannot read byte");
        println!("Received (Future impl): {}", byte as char);
    });

    // A shorter way to read a single byte; we just use the read_async method
    // provided by tokio_async_await on any AsyncRead:
    let byte_future2 = async {
        let mut buf = [0;1];
        let mut stdin = tokio::io::stdin();
        await!(stdin.read_async(&mut buf))?;
        Ok::<u8,tokio::io::Error>(buf[0])
    };

    tokio::run_async(async {
        let byte = await!(byte_future2).expect("cannot read byte");
        println!("Received (read_async impl): {}", byte as char);
    });

    // Writing new style streams to work with old-style AsyncReaders
    // =============================================================

    // The simplest way to stream bytes from an AsyncReader is probably to just
    // use the `.read_async` method again, but in a while loop (this would be
    // easy to add better buffering to if we wanted):
    tokio::run_async(async {
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
    });

    // if we have a stream already, we can use the `.next()` method provided
    // by tokio_async_await to read each item from it in a loop. adding `move`
    // to the async block allows things to be moved in, just like a closure.
    let mut byte_stream = stream::iter_ok::<_,()>("from an old style stream\n".chars());
    tokio::run_async(async move {

        while let Some(Ok(c)) = await!(byte_stream.next()) {
            println!("Streamed 2: {}", c);
        }

    });

    // Writing new style sinks to work with old-style AsyncWriters
    // ===========================================================

    // tokio_async_await adds `write_async`, `write_all_async` and `flush_async`
    // methods to AsyncWriters, so it becomes easy to put bytes into them:
    tokio::run_async(async {
        let message = "writing to stdout, one byte at a time\n";
        let mut stdout = tokio::io::stdout();

        for byte in message.bytes() {
            let buf = &[byte];
            if let Err(e) = await!(stdout.write_all_async(buf)) {
                println!("Error writing out: {:?}", e);
            }
        }

        await!(stdout.flush_async()).unwrap();
    });

    // If you already have a Sink, you can use the `send_async` method added
    // to them by tokio_async_await to easily (and without having to give up
    // ownership of the sink!) push values into it.
    //
    // We can make a byte sink using the tokio::codec stuff quite easily,
    // or implement it any other way we like:
    let mut byte_stream = stream::iter_ok::<_,()>("From a Stream to a Sink\n".bytes());
    let mut byte_sink = FramedWrite::new(tokio::io::stdout(), BytesCodec::new())
        .with(|byte| {
            // convert an incoming byte into the Bytes type expected by the codec:
            Ok::<_, tokio::io::Error>([byte][..].into())
        });

    // Now, forward everything from the Stream to the Sink (we could also
    // just use .forward):
    tokio::run_async(async move {
        while let Some(Ok(byte)) = await!(byte_stream.next()) {
            if let Err(e) = await!(byte_sink.send_async(byte)) {
                println!("Error: {:?}", e);
            }
        }
    });

    // Spawning new style futures to run concurrently
    // ==============================================

    // `tokio::spawn_async` let's us spawn futures to be run concurrently,
    // just like `tokio::spawn` does for 0.1 futures (remember, we know how
    // to convert back and forth between 0.1 and std futures):
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
    println!();

}
