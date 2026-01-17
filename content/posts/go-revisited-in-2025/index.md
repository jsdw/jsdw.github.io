+++
title = "Go: Generics and Iterators"
description = "Back when I last used Go a decade ago, it had no Generics or Iterators. Now that they've been added, I thought I'd try it again and give my thoughts on them."
date = 2026-01-16
[extra]
created = "2026-01-04"
+++

I first ran into Go probably more than a decade ago now. There were some things that I loved about the language. The async runtime meant that there was no need for `async` and `await` keywords; the runtime took care of yielding and switching between tasks for you. The standard library was pretty feature rich, at least in the context of making web servers (which was what I was most interested in at the time). Compilation times were amazing (especially coming from languages like C++). Channels and goroutines made it trivial to build actor-like programs that passed data between tasks.

There were also some things that frustrated me. A lack of generics meant that we were back in the old Java days of passing `interface{}` types around and casting them to/from the desired types. Creating custom data types was hideous and the compiler didn't have your back regarding type safety. At the same time, I was being exposed to languages like Rust which has a beautiful `Iterator` trait for iterating over all sorts of different types.

I'd heard about Go getting generics a while ago, so I thought it was time to have play with it, which is pretty much what Advent of Code is for. To my surprise, I found out that not only does Go have generics, but it also has a rather beautiful approach to iteration.

Let's have a look!

# Generics

The main thing I find myself doing a lot in languages like `Rust` is wanting to wrap functionality up into custom data structures. I also want to leverage as much type safety as possible. This is now possible to some extent in Go. For example, in Advent of Code this year I wanted a queue data structure (and was determined to not reach beyond the stdlib for anything), so I wrote myself a [double ended queue][deque] which could be used like:

```go
d := NewDeque[int]()

// Push values to the back:
d.PushBack(1)
d.PushBack(2)
d.PushBack(3)

// Or to the front:
d.PushFront(0)
d.PushFront(-1)
d.PushFront(-2)

// Pop values off either side:
val, ok := d.PopBack()
val, ok := d.PopFront()
```

This is entirely type safe; no casting to or from any `interface{}` / `any` types anywhere.

I also ended up writing [a `Heap` data structure][heap] which wrapped Go's `"container/heap"` interface into something nicer, [a `Set` structure][set] to wrap `map[T]struct{}` and expose useful functions for working with sets, and then things like [a _depth first search_ type][dfs] for searching over generic states.

Generics in Go have som similarities to generics in Rust: for each generic parameter that you declare, you can opt to constrain it to types which implement a specific interface, much like how Rust generics can be constrained to types implementing certain traits.

Here's an example:

```go
// Interface representing something
// that creates IDs:
type IdMaker[Id comparable] interface {
    NextId() Id
}

// Create int IDs:
type IntIdMaker int

func (this *IntIdMaker) NextId() int {
    *this += 1
    return int(*this)
}

// Create string IDs:
type StringIdMaker int

func (this *StringIdMaker) NextId() string {
    *this += 1
    return "s:" + strconv.Itoa(int(*this))
}
```

Here, `IdMaker` is an interface which itself has a generic parameter `Id` (which is constrained to be only types that are comparable). The interface is implemented a couple of times on the concrete types `IntIdMaker` and `StringIdMaker`.

Functions also accept generic arguments, so we can create one which uses this `IdMaker` like so:

```go
func CreateIds[Ids IdMaker[Id], Id comparable](idMaker Ids, n int) []Id {
    ids := []Id{}
    for range n {
        ids = append(ids, idMaker.NextId())
    }
    return ids
}

// Usage:
intIdMaker := IntIdMaker(0)
ids := CreateIds(&intIdMaker, 10)
fmt.Println(ids) // Output: [1 2 3 4 5 6 7 8 9 10]
```

Here, `CreateIds` takes some type which implements our `IdMaker` interface and doesn't care about the type of ID it creates. It outputs a slice of `n` IDs using it.

Structs also accept generic arguments in a similar way. Here's a dumb cache type which caches values against IDs returned from our `IdMaker`:

```go
type Cache[Ids IdMaker[Id], Id comparable, T any] struct {
    ids    IdMaker[Id]
    values map[Id]T
}

func NewCache[Ids IdMaker[Id], Id comparable, T any](idMaker IdMaker[Id]) Cache[Ids, Id, T] {
    return Cache[Ids, Id, T]{
        ids:    idMaker,
        values: map[Id]T{},
    }
}

func (this *Cache[Ids, Id, T]) Insert(val T) Id {
    thisId := this.ids.NextId()
    this.values[thisId] = val
    return thisId
}

// Usage:
stringIdMaker := StringIdMaker(0)
cache := NewCache[*StringIdMaker, string, string](&stringIdMaker)

fmt.Println(cache.Insert("hello")) // Output: s:1
fmt.Println(cache.Insert("world")) // Output: s:2
```

This is all a little convoluted in order to show generics and the constraints that can be placed on them. Overall, you can go quite far with generics in Go, but there are a couple of limitations which you'll run into:

## No generic methods

The one place where generics aren't allows it on methods. This prevents you from writing functions like `Map` or `Fold` on your new generic container types. For instance, this is not valid Go:

```go
// Some data structure:
type MyDataStructure[T any] struct{
    // impl
}

// Map the values in this data structure to a different type:
func (d MyDataStructure[T]) Map[R any](f func(T) R) MyDataStructure[R] {
    out := NewMyDataStructure[R]()
    for value := range d.Values() {
        out.Push(f(value))
    }
    return out
}

// usage:
stringData := intData.Map(func(val int) string {
    return val.String()
})
```

However, you can write a freestanding function to do this instead, which loses some of the elegance of a method call but achieves the same goal:

```go
func Map[T any, R any](d MyDataStructure[T], f func(T) R) MyDataStructure[R] {
    out := NewMyDataStructure[R]()
    for value := range d.Values() {
        out.Push(f(value))
    }
    return out
}

// usage:
stringData := mydatastructure.Map(intData, func(val int) string {
    return val.String()
})
```

## Interfaces can't use `Self` or `this` in their signatures

There exists a built-in `comparable` interface, which is implemented automatically on most types (except, annoyingly, not on slices or any type containing a slice). How could I implement a similar interface, if I wanted to be able to compare slices or more arbitrary custom types in a similar way?

My first thought would be something like this:

```go
type Comparable Interface {
    // Just want equality to keep it simple:
    Eq(other Self) bool
}
```

However, this isn't valid Go: I can't say that I want a value of the same type as an input to an interface method.

What I can do is use a generic parameter on the interface though, like this:

```go
type Comparable[Other any] interface {
    Eq(other Other) bool
}

// MyInt will implement Comparable[MyInt]:
type MyInt int

func (this MyInt) Eq(other MyInt) bool {
    return int(other) == int(this)
}

// We can now use this in custom data types like this:
type MyDataType[T Comparable[T]] struct {
    vals []T
}
ints := MyDataType[MyInt]{}
```

I suppose this is actually similar to how Rust implements such traits, except that the `Other` generic defaults to `Self` if it's not provided. The obvious limitation over using the builtin `comparable` interface is that this isn't implemented for the builtin or primitive types. You could wrap comparable types to implement this `Comparable` like this though:

```go
type FromComparable[C comparable] struct{ c C }

func (this FromComparable[C]) Eq(other FromComparable[C]) bool {
    return this.c == other.c
}

// These are Comparable now:
FromComparable[string]{"hello"}
FromComparable[int]{1}
```

Whether re-inventing the wheel to this extent is a good idea is another question though, but I suppose it's good to know that it's possible, and this sort of approach may be necessary if you want to write custom data types like Binary Trees which accept custom keys (`cmp.Ordered` exists but is only implemented for primitive types or wrappers around primitive types).

However..

## Function names cannot collide

This means that we can't actually implement our `Comparable[C]` interface more than once for a given type. This would be useful but not valid:

```go
type MyType int

// Implements Comparable[int]
func (this MyType) Eq(other int) bool {
    return int(this) == other
}

// Implements Comparable[MyType]
func (this MyType) Eq(other MyType) bool {
    return int(this) == int(other)
}
```

given this limitation, it's probably best to define `Comparable` without a generic, and have one implementation which can compare against any type you think useful, like so:

```go
type Comparable interface {
    Eq(other any) bool
}

type MyType int

// Implements Comparable
func (this MyType) Eq(other any) bool {
    switch v := other.(type) {
    case int:
        return int(this) == v
    case MyType:
        return this == v
    }
    return false
}
```

This is a little less type safe, but in this case does the trick.

There are some other [known issues][generic-issues] with generics discussed by the Go team too. Despite these limitations, Go generics make the language _so_ much more enjoyable to use in my opinion.

# Iterators

I hadn't even noticed that Go had adopted any sort of iterator approach, and was very pleasantly surprised to see that iterators have now been integrated much more into Go. Iterators are great when there are several intermediate steps that you'd like to apply to the items in some data structure (for instance a slice), or when you want to provide back each of the values in some data structure for users to do something with, or just when you want to stream values back to a user as they become available.

In Go, you've always been able to iterate over the built-in data structures:

```go
mySlice := []int{}
for index, value := range mySlice {
    // Do something with each value
}

myMap := map[int]string{}
for key, value := range myMap {
    // Do something with each value
}
```

Now, you're able to iterate over arbitrary things by writing functions/methods which return functions that match the special iterator type signatures (which are aliased as `iter.Seq[A]` or `iter.Seq2[A, B]`). These functions are our iterators.

Writing these functions looks something like this:

```go
type MyContainer[T any] struct{
    inner []T
}

func (c *MyContainer[T]) Values() iter.Seq[T] {
    return func(yield func(T) bool) {
        for _, value := range c.inner {
            if !yield(value) {
                return
            }
        }
    }
}

func (c *MyContainer[T]) KeysAndValues() iter.Seq2[int, T] {
    return func(yield func(int, T) bool) {
        for key, value := range c.inner {
            if !yield(key, value) {
                return
            }
        }
    }
}
```

How can we use these iterators? The naive way is to just call the iterator function like so:

```go
myContainer := &MyContainer[string]{}

iterFn := myContainer.Values()
iterFn(func yield(v string) bool {
    // do something with each item,
    // return false when we don't want more.
})

keyAndValueIterFn := myContainer.KeysAndValues()
keyAndValueIterFn(func yield(k int, v string) bool {
    // do something with keys and values
    // return false when we don't want more.
})
```

Go also added some syntax sugar so that `for range` loops work too, where the loop body is basically the function call:

```go
for v := range myContainer.Values() {
    // do something with values
    // break out of the loop when we don't want more.
}
for k, v := range myContainer.KeysAndValues() {
    // do something with keys and values
    // break out of the loop when we don't want more.
}
```

Aside from being able to write your own iterators, we now have some handy iterators in the standard library, like `slices.Values` to iterate over the values in a slice (useful if you want to consume an iterator somewhere and have a slice), `maps.All`, `maps.Keys` and `maps.Values` for iterating over maps, things like `strings.Lines` for iterating over all of the lines in a string, and a bunch more. Some iterator functions in the stdlib end with `Seq` to differentiate themselves from the pre-existing non-iterator functions that came before.

Iterators in Go are beautiful, in my opinion. Without any new keywords (like `yield` for instance) and minimal extra language support (just some syntax sugar to make iterators play nicely with `for range` loops), Go has made it possible to:

- Stream synchronous _or_ asynchronous values using a single, consistent interface.
- Use a simple callback based approach to returning values, which makes it easy to return them at any point in potentially complex functions.
- Convert these into pull based iterators, to allow even more flexibility.

## Push and pull based iterators

Push based iterators are iterators which push the values to the caller; the caller does not control the progress of the iterator. Pull based iterators hand control to the caller; the caller decides when to pull the next value from the iterator. 

At first glance, Rust style iterators are pull based and Go style iterators are push based. Let's look at a very simple iterator implementation in Go:

```go
func Range(n int) iter.Seq[int] {
    return func(yield func(int) bool) {
        for v := range n {
            if !yield(v) {
                return
            }
        }
    }
}
```

The `Range` function here returns an iterator (which is itself just a function). When handed a callback, this iterator calls the callback with each value from 0 to `n`. If the callback is done receiving values, it can return false to tell the iterator function to stop.

This has limitations. For instance, what if I want to execute two iterators in lock step, handing back the values for each? It turns out that Go has a way to convert its push based iterators into pull based ones via an `iter.Pull` function. 

Here's how we can use this function to "zip" two iterators together, returning an iterator which hands back pairs of values from both of the input iterators:

```go
func Zip[A any, B any](as iter.Seq[A], bs iter.Seq[B]) iter.Seq2[A, B] {
    return func(yield func(A, B) bool) {
        // Convert the first iterator to be pull based:
        nextA, stopA := iter.Pull(as)
        defer stopA()
        // Convert the second iterator to be pull based:
        nextB, stopB := iter.Pull(bs)
        defer stopB()

        for {
            // Now, we can ask for one value from each at a
            // time, and yield them both together:
            a, aIsOk := nextA()
            b, bIsOk := nextB()

            if aIsOk && bIsOk {
                if !yield(a, b) {
                    return
                }
            } else {
                return
            }
        }
    }
}

// usage:

strings := []string{"a", "b", "c"}
ints := []int{1, 2, 3}

for s, i := range Zip(slices.Values(strings), slices.Values(ints)) {
    fmt.Println(s, i) // "a" 1, "b" 2, "c" 3
}
```

This ability to convert push based iterators into pull based ones is magical, in my opinion, and relies on the fact that the Go runtime can pause and resume coroutines. In fact, you can implement this yourself; this is my quick attempt at doing so:

```go
func Pull[V any](it iter.Seq[V]) (next func() (V, bool), stop func()) {
    valueChan := make(chan V)
    stopChan := make(chan struct{})

    // Call the iterator with our yield callback in a
    // goroutine. This blocks each time it is handed back a
    // value until the next() fn is called to receive it.
    go func() {
        it(func(val V) bool {
            select {
            case <-stopChan:
                return false
            case valueChan <- val:
                return true
            }
        })
        close(valueChan)
    }()

    // Each time next is called, pull another value
    // from our yield function, unblocking it until it
    // yields another value.
    next = func() (V, bool) {
        val, ok := <-valueChan
        return val, ok
    }

    // Closing the stopChan signals that we should
    // return false from our yield fn, ending iteration.
    stop = func() {
        close(stopChan)
    }

    return
}
```

By using a select statement and channel inside our yield function, we block it from progressing until the next function is called and pulls the value back out of the channel. The actual implementation is more efficient than this but a similar concept.

The asynchronous runtime in Go really shines here, enabling powerful, flexible iterators without needing additional keywords. I love Rust, but because it lacks this runtime support, it needs to draw a distinction between synchronous `Iterator`s and asynchronous `Streams`, and it would require new language syntax (`gen fn`) to provide the ergonomics afforded when writing push based iterators in Go.

# Closing thoughts

Go has always moved slowly and carefully as a language, which I appreciate more and more as time goes on. That said, I really enjoy these recent evolutions to the language. Iterators and Generics add two of the big feature that I sorely missed, and makes it possible for me to write the sort of code I want to write. While I still have various other gripes, the same is true of most languages. 

Now I will just hope that Go gets tagged enums at some point :)

[aoc2025]: https://github.com/jsdw/advent-of-code-2025
[deque]: https://github.com/jsdw/advent-of-code-2025/blob/main/utils/deque.go
[heap]: https://github.com/jsdw/advent-of-code-2025/blob/main/utils/heap.go
[set]: https://github.com/jsdw/advent-of-code-2025/blob/main/utils/set.go
[dfs]: https://github.com/jsdw/advent-of-code-2025/blob/main/utils/dfs.go
[generic-issues]: https://go.googlesource.com/proposal/+/refs/heads/master/design/43651-type-parameters.md#issues
