use tokio::io;
use tokio::prelude::*;
use tokio::codec;
use futures::future;
use futures::try_ready;

fn main() {

    // ########################################################
    // ### AsyncRead (/poll_x) to Future, for one-off reads ###
    // ########################################################

    println!("### Single byte futures");

    // The most verbose way to turn a thing implementing AsyncRead
    // into a future that emits a single byte is implementing the
    // `Future` trait to do so for us, by calling the poll_read
    // method and matching on the result, transforming a successful
    // read from the number of bytes read to the actual byte value:
    struct ByteFuture<R>(R);
    impl <R: AsyncRead> Future for ByteFuture<R> {
        type Item = u8;
        type Error = io::Error;
        fn poll(&mut self) -> Result<Async<u8>, io::Error> {
            let mut buf = [0;1];
            match self.0.poll_read(&mut buf) {
                Ok(Async::Ready(_num_bytes_read)) => Ok(Async::Ready(buf[0])),
                Ok(Async::NotReady) => Ok(Async::NotReady),
                Err(e) => Err(e)
            }
        }
    }
    let byte_future1 = ByteFuture(io::stdin());

    run_future(byte_future1);

    // A slightly less verbose approach uses future::poll_fn to turn
    // our poll_x functions into futures for us. We basically have to
    // implement the poll method of a Future, much like above:
    let byte_future2 = future::poll_fn(move || {
        let mut buf = [0;1];
        match io::stdin().poll_read(&mut buf) {
            Ok(Async::Ready(_num_bytes_read)) => Ok(Async::Ready(buf[0])),
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(e) => Err(e)
        }
    });

    run_future(byte_future2);

    // `try_ready!` is like `?` but for poll results, so we could
    // use that to simplify the above like so (this isn't documented
    // though, so I wonder how long it will be around for). Because of
    // try_ready though, the error type needs specifying since it could
    // have been converted using From (like `?` would do):
    let byte_future3 = future::poll_fn(move || {
        let mut buf = [0;1];
        let _num_bytes_read = try_ready!(io::stdin().poll_read(&mut buf));
        Ok::<_,io::Error>(Async::Ready(buf[0]))
    });

    run_future(byte_future3);

    // If we are specifically dealing with AsyncRead things, we can also
    // use the `read` function to read bytes into a buffer. This takes
    // ownership of a buffer and reader, but returns them on success:
    let byte_future4 = io::read(io::stdin(), [0;1])
        .map(|(_stdin,buf,_num_bytes_read)| buf[0]);

    run_future(byte_future4);

    // Run the byte_future as a task (we need to handle the
    // result and error, because they have nowhere else to go,
    // much like if we spawned it inside a future we called run with):
    fn run_future<F: Future<Item=u8,Error=io::Error> + Send + 'static>(byte_future: F) {
        tokio::run(
            byte_future
                .map(|byte| println!("Read: {:?}", byte as char))
                .map_err(|e: io::Error| println!("Error reading byte: {:?}", e))
        );
    }

    // #############################################################
    // ### AsyncRead (/poll_x) to Stream, for continuous reading ###
    // #############################################################

    println!("### Streaming bytes to the next newline");

    // Turning an AsyncRead into a `Stream` is almost the same as turning it into
    // a `Future`; the main difference is that we return `Async<Option<u8>` instead
    // of `Async<u8>`; This allows the stream to signal that it's finished by sending
    // back None, instead of a byte. We should also handle the reader finishing (we
    // should have some do for the Futures as well, really..); in this case it
    // returns `n == 0` on `poll_read` for the number of bytes read, so we watch for
    // this and close the Stream when we see it by sending back `None`:
    struct ByteStream<R>(R);
    impl <R: AsyncRead> Stream for ByteStream<R> {
        type Item = u8;
        type Error = io::Error;
        fn poll(&mut self) -> Result<Async<Option<u8>>, io::Error> {
            let mut buf = [0;1];
            match self.0.poll_read(&mut buf) {
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
        }
    }
    let byte_stream1 = ByteStream(io::stdin());

    run_stream(byte_stream1);

    // Read a stream of bytes from stdin. This is almost the same as reading
    // a single byte, except we return Some(T) in our Async::Ready; when the
    // stream has finished we can return None. Uses stream::poll_fn instead of
    // futures::poll_fn.
    let byte_stream2 = stream::poll_fn(move || {
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

    run_stream(byte_stream2);

    // We can also use the io::read helper + stream::unfold to continuously read bytes
    // into a stream, However We don't have a way to return None and end the stream
    // *after* trying a read, so this won't work for AsyncWriters that may end:
    let byte_stream3 = stream::unfold((), |_| {
        let next = io::read(io::stdin(), [0;1]).map(|(_stdin,buf,n)| (buf[0], ()));
        Some(next)
    });

    run_stream(byte_stream3);

    // We can also use `tokio::codec::FramedRead`, and the basic BytesCodec, to
    // turn an AsyncRead into a Stream. If we want individual bytes we need to do
    // a bit more work to flatten the BytesMut that we get back into individual bytes:
    let byte_stream4 = codec::FramedRead::new(io::stdin(), codec::BytesCodec::new())
        // convert our bytes buffer into a stream that emits one byte at a time:
        .map(|bytes| stream::iter_ok::<_, io::Error>(bytes))
        // flatten our stream of streams down into one stream:
        .flatten();

    run_stream(byte_stream4);

    // To run a stream, we can use the stream_combinators like take_while to end
    // the stream for us when some condition (eg newline) is hit. We then use
    // for_each to convert the stream into a future that runs the function passed
    // for each item before returning, and handle any error at the end:
    fn run_stream<S: Stream<Item=u8,Error=io::Error> + Send + 'static>(byte_stream: S) {
        tokio::run(
            byte_stream
                // take until we hit a newline, then finish:
                .take_while(|&b| Ok(b != b'\n'))
                // for each byte we take, print it:
                .for_each(|b| {
                    println!("Streamed: {:?}", b as char);
                    // If we error here, the stream ends:
                    Ok(())
                })
                // We need to do something with errors:
                .map_err(|e| println!("Error reading byte: {:?}", e))
        );
    }

    // ############################################################
    // ### AsyncWrite (/poll_x) to Sink, for continuous writing ###
    // ############################################################

    println!("### Outputting to a sink");

    // Implementing the Sink trait involves handling two poll functions,
    // start_send to push a thing into the Sink (this might put the thing in
    // an internal buffer, but here we just forward it to the poll_write call),
    // and then poll_complete will to called to ensure that everything is fully
    // flushed to the output:
    struct ByteSink<W>(W);
    impl <W: AsyncWrite> Sink for ByteSink<W> {
        type SinkItem = u8;
        type SinkError = io::Error;
        fn start_send(&mut self, item: u8) -> Result<AsyncSink<u8>, io::Error> {
            match self.0.poll_write(&[item])? {
                Async::NotReady => Ok(AsyncSink::NotReady(item)),
                Async::Ready(_) => Ok(AsyncSink::Ready)
            }
        }
        fn poll_complete(&mut self) -> Result<Async<()>,io::Error> {
            match self.0.poll_flush()? {
                Async::Ready(_) => Ok(Async::Ready(())),
                Async::NotReady => Ok(Async::NotReady)
            }
        }
    }
    let byte_sink1 = ByteSink(io::stdout());

    run_sink(byte_sink1);

    // We can use BytesCodec again to create a Sink which accepts Bytes buffers
    // containing bytes. To turn that into a Sink accepting individual bytes we
    // use `with` to place a function in front, which goes from a byte to a Bytes:
    let byte_sink2 = codec::FramedWrite::new(io::stdout(), codec::BytesCodec::new())
        .with(|byte| {
            // convert an incoming byte into the Bytes type expected by the codec:
            Ok::<_, io::Error>([byte][..].into())
        });

    run_sink(byte_sink2);

    // Now we have a sink which accepts bytes, we can stream bytes into it:
    fn run_sink<S: Sink<SinkItem=u8,SinkError=io::Error> + Send + 'static>(byte_sink: S) {
        tokio::run(
            stream::iter_ok::<_, io::Error>("sending to sink\n".bytes())
                // forward our stream to the aink we have made:
                .forward(byte_sink)
                // map output from this Forward Future to () since we don't care:
                .map(|_| println!("bytes sent to sink!"))
                // ignore any errors in this case:
                .map_err(|_| ())
        )
    }

    // ############################################################
    // ### AsyncWrite (/poll_x) to Sink, for continuous writing ###
    // ############################################################

    println!("### Outputting a single byte to a sink");

    // Once we've created a `Sink`, as above, we can use the `send` method, which returns
    // a Future that resolves when a single item has been sent and flushed. If we don't
    // have a `Sink`, we can use the `io::write_all` helper to quickly write to an AsyncWrite:
    let write_once = io::write_all(io::stdout(), &[b'x']);

    tokio::run(
        write_once
            .map(|_| ())
            .map_err(|e| println!("Error writing one byte: {:?}", e))
    );

}
