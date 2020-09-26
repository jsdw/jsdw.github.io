+++
title = "Fun with Function Traits"
description = ""
date = 2020-09-26
draft = true
[extra]
toc = 0
+++

I'd like to explore some slightly more exotic ways of working with function traits in Rust. A goal of this post is to describe how it's possible to create an API like this in Rust (which, not long ago, I'd have thought wasn't possible):

```rust
let api = Api::new();

api.add_route("/some/path", |body: Foo| {
    SomeResult { val: body.a + body.b  }
});

api.add_route("/another/path", |body: Bar, type: Header<ContentType>| {
    AnotherResult { type: type.content_type, result: body.wibble }
});

assert_eq!(
    api.describe(),
    vec![
        Description { path: "/some/path", input: "Foo", output: "SomeResult" },
        Description { path: "/another/path", input: "Bar", output: "AnotherResult" }
    ]
);
```

This example is inspired by some real code I wrote recently that allowed me to very quickly define simple APIs that had the following properties:
- Data coming into and leaving the API is constrained (to be primarily JSON, in my case).
- It's possible to "ask" for specific bits of information from the incoming request, such as what user is making it.
- Errors from the API routes are handed back in a standard format.
- It's possible to describe the shape of the entire API, so that I can generate TypeScript types and documentation for it entirely from the code (and doc comments in the code).

Here, I'm going to focus on how we can allow a variable number of arguments to be "asked for" by the closures we pass to some function, and how you can obtain information about the arguments coming in and the responses of those closures.

# The Basics

As a refresher, when we want to pass functions around in Rust, we normally resort to using the function traits `Fn`, `FnMut` and `FnOnce`. The function _type_ `fn(foo) -> bar` can also be used but is decidedly less powerful.

I'm not particularly concerned about the distinction between those traits here ([the rust book][rust-book-closures] covers that), but to sum it up:

- `FnOnce` impls can mutate _and_ consume (take ownership of) the values they close over when they run, and so can only be run once.
- `FnMut` impls can _mutate_ the values they close over when they run, but not consume them.
- `Fn` impls can only immutably borrow variables when they run.

Things accepting `FnOnce` traits therefore give more power to the thing defining the function (the function can do more), but less power to the thing that calls it (it can only be called once for instance). On the other end of the scale, it's harder to define a function which satisfies `Fn` (it's not allowed to take owenership or mutate), but the thing calling it has a lot of flexibility.

Anyway, that's not why we're here! What I'm really interested in is how you can make use of these traits to pass functions in to other functions. A very simple example might look like this:

```rust
fn run_twice<F>(f: F, input: usize) -> usize
where
  F: Fn(usize) -> usize
{
    f(f(input))
}
```

Here, we take a function of `usize -> usize` and an initial `input`, and return the result of having applied that function twice.

We can make our `run_twice` function more widely usable by making the arguments generic, too:

```rust
fn run_twice<F,T>(f: F, input: T) -> T
where
  F: Fn(T) -> T
{
    f(f(input))
}
```

If we need to, we can also constrain the arguments (and responses) to the function to implementing certain traits:

```rust
fn run_twice_and_log<F,T>(f: F, input: T) -> T
where
  T: std::fmt::Debug,
  F: Fn(T) -> T
{
    println!("Before: {:?}", input);
    let after = f(f(input));
    println!("After: {:?}", after);
    after
}
```

This is neat, but how do we allow `input` to take a variable number of arguments? For that, we need more traits!

# Traits on top of the Function Traits

If we want to accept something that can take several different shapes, we look towards traits. Let's define a couple of types representing a simple HTTP request and response to motivate the upcoming examples:

```rust
/// A naive representation of (part of) an HTTP request,
/// with a path, some headers and some bytes in the body.
struct Request {
    path: String,
    headers: Vec<(String,String)>,
    body: Vec<u8>
}

/// For our response, we only care that we return some
/// bytes in the body; let's ignore the rest.
struct Response {
    body: Vec<u8>
}
```

What we'll try to do now is to define a trait which has a single function that goes from a `Request` to a `Response`, and then implement that trait for several different shapes of `Fn`.

So, first off, let's define a trait with one method whose goal is to take a `Request` and return a `Response`:

```rust
trait Handler {
    fn handle(&self, req: Request) -> Response;
}
```

Now we can try to implmenet that for one possible shape of closure:

```rust
impl <F, Body> Handler for F
where
  F: Fn(Body) -> Response,
  Body: serde::de::DeserializeOwned
{
    fn handle(&self, req: Request) -> Response {
        let body = serde_json::from_slice(&req.body).unwrap();
        self(body)
    }
}
```

This says: "for anything that implemenets `Fn(Body) -> Response`, where `Body` is some type that can be deserialized, we define a `handle` method that deserializes the body and passes it to this `Fn` implmentation to get back our `Response`". We're now making use of `serde` for general serialization and deserialization, and `serde_json` to, well, do this with JSON.

This would a good start, but it doesn't actually work! If you try compiling the above, you run into something like:

```
error[E0207]: the type parameter `Body` is not constrained by the impl trait, self type, or predicates
  --> src/main.rs:34:10
   |
34 | impl <F, Body> Handler for F
   |          ^^^^ unconstrained type parameter
```

What's that all about? Well, I'm not entirely sure, but [this RFC][unconstrained-rfc] discusses the reason for disallowing things like the above. Fortunately, the fix is easy; we have to mention `Body` somewhere in the `impl` line. One way to do that is to allow our `Handler` trait to take a type parameter, so that it looks more like:

```rust
trait Handler<T> { // <-- We've added <T>
    fn handle(&self, req: Request) -> Response;
}
```

Then, we have somewhere to pass `Body` to in our impl to make the compiler happy:

```rust
impl <F, Body> Handler<Body> for F // <-- We've added <Body>
where
  F: Fn(Body) -> Response,
  Body: serde::de::DeserializeOwned
{
  fn handle(&self, req: Request) -> Response {
      let body = serde_json::from_slice(&req.body).unwrap();
      self(body)
  }
}
```

Everything compiles again, woohoo!

# Aside: Trait Families

We've gone from a `Handler` trait to a `Handler<Body>` trait. What we've essentially done here is moved from defining a single trait called `Handler` to defining an entire family of traits parameterised by `Body`. What do I mean? Well, an important limitation of traits is that you can't have overlapping implementations.

If I have:

```rust
trait Foo {}
```

I can implement this for every type `T` by doing:

```rust
impl <T> Foo for T {}
```

But once I've done that, I can't implement `Foo` for anything else, because it's already been implemented for every type.

Even if I have some trait `A` and some trait `B`, and nothing implements them both, I can't write something like this:

```rust
impl <T> Foo for T where T: A {}
impl <T> Foo for T where T: B {}
```

Why not? Because there's no guarantee that things won't ever implement both `A` and `B`, and if something ever did, which impl should be picked?

However, what I _can_ do is to define a family of traits like so:

```rust
trait Foo<T> {}
```

Each time I vary `T` I have an entirely new trait to work with, and so this is perfeclty valid:

```rust
struct One;
struct Two;

impl <T> Foo<One> for T {}
impl <T> Foo<Two> for T {}
```

In many ways, this is the same as just defining `FooOne` and `FooTwo` as separate traits. The advantages of `Foo<One>` and `Foo<Two>` is that we have ways to talk about them both at once, and we have Rust's excellent type inference to help avoid us _needing_ to talk about `One` and `Two` specifically when it can work out which is valid.

Let's look at a more complete example to see what I mean:

```rust
// A couple of traits with default impls for simplicity:
trait A {
    fn a(&self) -> &'static str { "A" }
}
trait B {
    fn b(&self) -> &'static str { "B" }
}

// A trait family:
trait WhatAmI<T> {
    fn what_am_i(&self);
}

// use these to talk about 2 actual traits from our family:
struct One;
struct Two;

// impl WhatAmI<One> for all T's that impl A
impl <T> WhatAmI<One> for T
where T: A
{
    fn what_am_i(&self) {
        println!("{}", self.a());
    }
}

// impl WhatAmI<Two> for all T's that impl B
impl <T> WhatAmI<Two> for T
where T: B
{
    fn what_am_i(&self) {
        println!("{}", self.b());
    }
}
```

So, the above is sortof equivalent to having defined and implemented a `WhatAmIOne` and `WhatAmITwo` trait. The nice thing about defining them as a single family of traits is that I can now talk about them both at once in other places:

```rust
fn say_what_i_am<W,T>(thing: W)
where
  W: WhatAmI<T>
{
    thing.what_am_i();
}
```

Let's try using this `say_what_i_am` method:

```rust
struct IsA;
impl A for IsA {}

struct IsB;
impl B for IsB {}

struct IsBoth;
impl A for IsBoth {}
impl B for IsBoth {}

say_what_i_am(IsA); // prints "A"
say_what_i_am(IsB); // prints "B"
say_what_i_am(IsBoth); // error!
say_what_i_am::<IsBoth,One>(IsBoth); // prints "A"
say_what_i_am::<IsBoth,Two>(IsBoth); // prints "B"
```

The very cool thing here is that for `IsA` and `IsB`, which only impl one of `A` and `B`, the compiler can infer exactly which concrete trait from the set of `WhatAmI` traits to use. In other words, it knows that we must be referring to `WhatAmI<One>` and `WhatAmI<Two>` respectively! For `IsBoth`, the compiler fails to infer whether `T` is `One` or `Two`, because either is possible, and so we get a compiler error unless we tell it specifically which to use.

This inference of trait parameters seems to be very effective, and will help us to work with different shapes of functions while avoiding issues about overlapping trait implementations.

# Getting back on track

Armed with this knowledge, let's implement a second


[rust-book-closures]: https://doc.rust-lang.org/book/ch13-01-closures.html
[unconstrained-rfc]: https://github.com/rust-lang/rfcs/blob/master/text/0447-no-unused-impl-parameters.md