+++
title = "Rust: Fun with Function Traits"
description = "In this post I'm going to work towards writing a function that can accept and work with closures taking variable numbers and types of arguments. On the way, I'll look at trait families, and a way to avoid overlapping impls."
date = 2020-09-26
draft = false
[extra]
toc = 0
+++

In this post I'm going to work towards writing a function that can accept and work with closures taking variable numbers and types of arguments. On the way, I'll look at _trait families_, and a way to avoid overlapping impls.

As a refresher, when we want to pass functions around in Rust, we normally resort to using the function traits `Fn`, `FnMut` and `FnOnce`. The function _type_ `fn(foo) -> bar` can also be used but is decidedly less powerful.

I'm not particularly concerned about the distinction between those traits here ([the rust book][rust-book-closures] covers that), but to sum it up:

- Things that impl `FnOnce` can mutate _and_ consume (take ownership of) the values they close over when they run, and so can only be run once.
- Things that impl `FnMut` can mutate the values they close over when they run, but not consume them.
- Things that impl `Fn` can only immutably borrow variables when they run.

Anything that implements `Fn` also implements `FnMut` and `FnOnce`. Anything that's `FnMut` also implements `FnOnce`. Closures and functions automatically implement these traits based on how they use the variables that they close over.

If you're writing something that can be handed a closure, It's often preferable to accept anything that implements the `FnOnce` trait if you are able, falling back to `FnMut` if you need to call the function more than once, or `Fn` if you can't mutate things. This gives as much flexibility to the person defining the function as possible.

Anyway, a very simple example which takes advantage of a function trait might look like this:

```rust
fn run_twice<F>(f: F, input: usize) -> usize
where
  F: Fn(usize) -> usize
{
    f(f(input))
}
```

Here, `run_twice` takes a function of `usize -> usize` and an initial `input`, and returns the result of having applied that function twice.

We can make our `run_twice` function more widely usable by making the function argument and return type generic:

```rust
fn run_twice<F,T>(f: F, input: T) -> T
where
  F: Fn(T) -> T
{
    f(f(input))
}
```

Aside: a nice property of this is that we are being less specific to the function about what its arguments will be, and so the range of things that the function can do with the arguments is reduced (this function can't alter the `T`'s itself, for instance, whereas the less generic version could have altered the `usize`s).

Using this looks like:

```rust
run_twice(|n| n * 2, 2);
run_twice(|mut v| { v.push(true); v }, vec![false,true]);
```

If we need to be able to do certain things with our `T`'s, we can constrain the input and output of the function to requiring certain traits be implemented on them, here `Debug`:

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

As well as built-in traits, we can also require that our own traits are implemented on the inputs and outputs:

```rust
// Return the name of the type as a String
// (`Any` provides similar functionality)
trait TypeName {
    fn type_name() -> String;
}

// Some implementations on types we want to be able to inspect.
// We could use macros to help generate these for many types.
impl TypeName for usize {
    fn type_name() -> String {
        "usize".to_owned()
    }
}
impl TypeName for String {
    fn type_name() -> String {
        "String".to_owned()
    }
}
impl <T: TypeName> TypeName for Vec<T> {
    fn type_name() -> String {
        format!("Vec<{}>", T::type_name())
    }
}
impl TypeName for () {
    fn type_name() -> String {
        "()".to_owned()
    }
}
impl <T1: TypeName, T2: TypeName> TypeName for (T1,T2) {
    fn type_name() -> String {
        format!("({},{})", T1::type_name(), T2::type_name())
    }
}

// Allow some basic introspection given the above:
fn inspect_function<F,T1,T2>(f: F)
where
  F: Fn(T1) -> T2,
  T1: TypeName,
  T2: TypeName
{
    println!("This function has the shape: {} -> {}",
        T1::type_name(),
        T2::type_name());
}

inspect_function(|n: usize| n.to_string());
// -> This function has the shape: i8 -> String
inspect_function(|mut v: Vec<_>| { v.push(10usize); });
// -> This function has the shape: Vec<i8> -> ()
```

Is there a way to allow our `inspect_function` to accept functions that take a variable number of arguments? If you'd asked me a few months ago I'd probably have said no, but traits in Rust continue to surprise me, so let's see how we can do this.


# More Traits

If we want to work with a collection of different things in a uniform way, we look towards traits. So, what if we could do something like this:

```rust
// Write a trait which all function types we're
// interested in must implement:
trait Describable {
    fn describe() -> String;
}

// Use this trait to allow different function
// types to be provided:
fn inspect_function<F>(f: F)
where
  F: Describable
{
    println!("This function has the shape: {}", F::describe());
}
```

We'd just need to implement `Describable` for different shapes of function and job done! Let's have a go at one such implementation:

```rust
impl <F, T1, T2> Describable for F
where
  F: Fn(T1) -> T2,
  T1: TypeName,
  T2: TypeName
{
    fn describe() -> String {
        format!("{} -> {}", T1::type_name(), T2::type_name())
    }
}
```

With this in hand, we are roughly back to where we started; `inspect_function` can describe simple 1 argument `Fn`s.

Well, actually we're worse off right now, because the above doesn't actually work..

```
error[E0207]: the type parameter `T1` is not constrained by the impl trait, self type, or predicates
  --> src/main.rs:50:10
   |
50 | impl <F, T1, T2> Describable for F
   |          ^^ unconstrained type parameter

error[E0207]: the type parameter `T2` is not constrained by the impl trait, self type, or predicates
  --> src/main.rs:50:14
   |
50 | impl <F, T1, T2> Describable for F
   |              ^^ unconstrained type parameter
```

What's that all about? Well, I'm not entirely sure, but [this RFC][unconstrained-rfc] goes into detail about the reasons for disallowing things like the above. Fortunately, the fix is easy; we have to mention `T1` and `T2` somewhere in the `impl` line. One way to do that is to allow our `Describable` trait to take type parameters, so we'd have something more like:

```rust
trait Describable<Args> {
    fn describe() -> String;
}

impl <F, T1, T2> Describable<(T1,T2)> for F
where
  F: Fn(T1) -> T2,
  T1: TypeName,
  T2: TypeName
{
    fn describe() -> String {
        format!("{} -> {}", T1::type_name(), T2::type_name())
    }
}

fn inspect_function<F,Args>(f: F)
where
  F: Describable<Args>
{
    println!("This function has the shape: {}", F::describe());
}
```

Now, `inspect_function` will work as expected. We've added a type parameter `Args` to the `Describable` trait, and then we've "used up" any spare parameters by passing them to this, so `Args` becomes `(T1,T2)` in our single implementation above. We've also made `inspect_function` generic over `Args` as well now, since we'll need to pass them along to `Describable`.

What we've really done here is turn the `Describable` trait into a _family_ of traits where each possible value for `Args` leads to a different concrete trait.

# Trait Families

I'm not sure whether the term "trait families" is used by others, but it feels like a good way of thinking about traits that take generic parameters. Let's dig a little more into what you gain by using trait families in the first place.

If I have:

```rust
trait Foo {}
```

I can implement this for every type `T` by doing:

```rust
impl <T> Foo for T {}
```

But once I've done that, I can't implement `Foo` for anything else, because it's already been implemented for every type. Why not? Because we aren't (currently) allowed to have overlapping implementations of a given trait. If we did, the compiler wouldn't know which of the multiple valid implementations to use.

Even if I have some trait `A` and some trait `B`, and nothing implements them both, I can't write something like this:

```rust
impl <T> Foo for T where T: A {}
impl <T> Foo for T where T: B {}
```

Despite the fact that _we_ know that these implementations don't overlap, the compiler doesn't. There's no guarantee that nothing will ever implement both `A` and `B`, and if something ever did, we'll end up with overlapping implementations again.

However, what I _can_ do is to define a family of traits like so:

```rust
trait Foo<T> {}
```

Each time I vary `T` I have bought into being a new concrete trait to work with, and so this is perfectly valid:

```rust
struct One;
struct Two;

impl <T> Foo<One> for T {}
impl <T> Foo<Two> for T {}
```

In many ways, this is the same as just defining `FooOne` and `FooTwo` as separate traits. However, by using a generic parameter and defining `Foo<One>` and `Foo<Two>` instead, we are promising that the general shape of each trait is the same (they are all `Foo`ey, even if the actual implementations differ). Perhaps more importantly, we also gain ways to talk about the whole family of `Foo` traits together, in a way that we can't with `FooOne` and `FooTwo`. In fact, because Rust has excellent type inference, we can often get away without telling it what the type `T` is in `Foo<T>`, and instead we can let the compiler work it out for us. As we'll see, this is super useful!

Let's look at a more complete example to see what I mean:

```rust
// First, let's define a couple of traits
// (with default impls for simplicity):
trait A {
    fn a(&self) -> &'static str { "A" }
}
trait B {
    fn b(&self) -> &'static str { "B" }
}

// Define a family of WhatAmI traits:
trait WhatAmI<T> {
    fn what_am_i(&self);
}

// Create a couple of arbitrary types that
// we'll assign to T, above.
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

So, the above is sortof equivalent to having defined and implemented a `WhatAmIOne` and `WhatAmITwo` trait. There is no overlap, since they are separate concrete traits. However, because they both belong to the same family, we have a way to talk about them both at once in other places.

The following function accepts any concrete trait from the `WhatAmI` family:

```rust
fn say_what_i_am<W,T>(thing: W)
where
  W: WhatAmI<T>
{
    thing.what_am_i();
}
```

The concrete value of `W` is constrained to be not just anything that implements a single trait, but anything implementing _any_ trait from the `WhatAmI` family.

The real magic happens when we try using this `say_what_i_am` method:

```rust
struct IsA;
impl A for IsA {}

struct IsB;
impl B for IsB {}

say_what_i_am(IsA); // prints "A"
say_what_i_am(IsB); // prints "B"
```

The super cool thing here is that for `IsA` and `IsB`, which only impl one of `A` and `B`, the compiler can infer exactly which concrete trait from the family of `WhatAmI` traits to use. In other words, it knows that we must be referring to `WhatAmI<One>` when `IsA` is passed in, and `WhatAmI<Two>` when `IsB` is passed in! It can infer that this is the case because there is only one possible choice in each case.

If there was something that implemented `A` and `B`, we'd run into issues:

```rust
struct IsBoth;
impl A for IsBoth {}
impl B for IsBoth {}

say_what_i_am(IsBoth); // error!
```

Type inference fails now, because `IsBoth` now implements both `WhatAmI<One>` and `WhatAmI<Two>`. So, we have to help the compiler by telling it which of `One` or `Two` we want to use:

```rust
say_what_i_am::<_,One>(IsBoth); // prints "A"
say_what_i_am::<_,Two>(IsBoth); // prints "B"
```

So, what we have here is a way to avoid issues with overlapping implementations (we just stamp out a different concrete trait from a given family of traits each time), but retain some of the niceness of working with traits, because we can work with families of traits in a similar way, and rely on type inference to keep things simple.

# Accepting variable arity functions using Trait Families

Armed with this knowledge, let's see how we can write a function that can accept closures with variable numbers of arguments.

This is where we'd got to before our digression into trait families:

```rust
trait Describable<Args> {
    fn describe() -> String;
}

impl <F, T1, T2> Describable<(T1,T2)> for F
where
  F: Fn(T1) -> T2,
  T1: TypeName,
  T2: TypeName
{
    fn describe() -> String {
        format!("{} -> {}", T1::type_name(), T2::type_name())
    }
}

fn inspect_function<F,Args>(f: F)
where
  F: Describable<Args>
{
    println!("This function has the shape: {}", F::describe());
}

// inspect_function(|n: usize| n.to_string());
//   prints "This function has the shape: i8 -> String"
// inspect_function(|mut v: Vec<_>| { v.push(10usize); });
//   prints "This function has the shape: Vec<i8> -> ()"
```

Let's implement `Describable` for some different numbers of function arguments:

```rust
impl <F,T1,T2,T3> Describable<(T1,T2,T3)> for F
where
  F: Fn(T1,T2) -> T3,
  T1: TypeName,
  T2: TypeName,
  T3: TypeName
{
    fn describe() -> String {
        format!("({},{}) -> {}",
            T1::type_name(),
            T2::type_name(),
            T3::type_name())
    }
}

impl <F,T1,T2,T3,T4> Describable<(T1,T2,T3,T4)> for F
where
  F: Fn(T1,T2,T3) -> T4,
  T1: TypeName,
  T2: TypeName,
  T3: TypeName,
  T4: TypeName
{
    fn describe() -> String {
        format!("({},{},{}) -> {}",
            T1::type_name(),
            T2::type_name(),
            T3::type_name(),
            T4::type_name())
    }
}
```

This works because we are stamping out new concrete traits from our `Describable` trait family for every different set of function types (ie, the type `(T1,T2)` will never overlap with `(T1,T2,T3)` which never overlaps with `(T1,T2,T3,T4)`). Since `inspect_function` accepts all members of the `Describable` trait family, and inference can resolve _which_ member to use in each case, `inspect_function` will work much like it did before, but accept more different function shapes:

```rust
inspect_function(|n: usize| n.to_string());
// -> This function has the shape: i8 -> String
inspect_function(|mut v: Vec<_>| { v.push(10usize); });
// -> This function has the shape: Vec<i8> -> ()
inspect_function(|a: usize, b: String| {});
// -> This function has the shape: (usize,String) -> ()
inspect_function(|a: usize, b: Vec<String>, c: (usize,usize)| { "hi".to_owned() });
// -> This function has the shape: (usize,Vec<String>,(usize,usize)) -> String
```

Awesome! `inspect_function` can take functions with variable numbers of arguments!

This approach seems to have good mileage from my experimentation; as long as the function types can be inferred, and there is only one valid implementation from a family of traits at a time (taking constraints and such into account), Rust is smart enough to infer which concrete trait to use.

One thing you may notice above, and run into if you try doing similar, is the amount of code duplication when implementing traits for various numbers of arguments.

To avoid some copypasta, we can replace the `Describable` implementations with a macro+usage like so (we have to tweak the string building in `describe` to make it amenable to this generalisation):

```rust
macro_rules! describable {
    ($( $($arg:ident)* => $res:ident ),+) => (
        $(
            impl <F,$res,$($arg),*> Describable<($res,$($arg),*)> for F
            where
              F: Fn($($arg),*) -> $res,
              $res: TypeName,
              $( $arg: TypeName ),*
            {
                fn describe() -> String {
                    let mut s = String::new();
                    $(
                        s += &$arg::type_name();
                        s += ",";
                    )*
                    s += " -> ";
                    s += &$res::type_name();
                    s
                }
            }
        )+
    )
}

describable!(
    => T1,
    T1 => T2,
    T1 T2 => T3,
    T1 T2 T3 => T4,
    T1 T2 T3 T4 => T5
);
```

Now we can easily implement `Describable` for any reasonable number of function args we'd like to handle.

I hope that this proves useful!

Below is the final version of all of our code. Thanks for reading!

```rust
// A custom trait to experiment with
trait TypeName {
    fn type_name() -> String;
}

impl TypeName for usize {
    fn type_name() -> String {
        "usize".to_owned()
    }
}
impl TypeName for String {
    fn type_name() -> String {
        "String".to_owned()
    }
}
impl <T: TypeName> TypeName for Vec<T> {
    fn type_name() -> String {
        format!("Vec<{}>", T::type_name())
    }
}
impl TypeName for () {
    fn type_name() -> String {
        "()".to_owned()
    }
}
impl <T1: TypeName, T2: TypeName> TypeName for (T1,T2) {
    fn type_name() -> String {
        format!("({},{})", T1::type_name(), T2::type_name())
    }
}

// A family of traits that can describe things
trait Describable<Args> {
    fn describe() -> String;
}

// implement the above for functions of varying arities
macro_rules! describable {
    ($( $($arg:ident)* => $res:ident ),+) => (
        $(
            impl <F,$res,$($arg),*> Describable<($res,$($arg),*)> for F
            where
              F: Fn($($arg),*) -> $res,
              $res: TypeName,
              $( $arg: TypeName ),*
            {
                fn describe() -> String {
                    let mut s = String::new();
                    $(
                        s += &$arg::type_name();
                        s += ",";
                    )*
                    s += " -> ";
                    s += &$res::type_name();
                    s
                }
            }
        )+
    )
}
describable!(
    => T1,
    T1 => T2,
    T1 T2 => T3,
    T1 T2 T3 => T4,
    T1 T2 T3 T4 => T5
);

// Finally, a function to show the Describable trait family in action
fn inspect_function<F,Args>(f: F)
where
  F: Describable<Args>
{
    println!("This function has the shape: {}", F::describe());
}

fn main () {
    inspect_function(|n: usize| n.to_string());
    inspect_function(|mut v: Vec<_>| { v.push(10usize); });
    inspect_function(|a: usize, b: String| {});
    inspect_function(|a: usize, b: Vec<String>, c: (usize,usize)| { "hi".to_owned() });
}
```

[rust-book-closures]: https://doc.rust-lang.org/book/ch13-01-closures.html
[unconstrained-rfc]: https://github.com/rust-lang/rfcs/blob/master/text/0447-no-unused-impl-parameters.md