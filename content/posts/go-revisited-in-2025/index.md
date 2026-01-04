+++
title = "Revisiting Go a decade later"
description = "Back when I last used Go, it lacked things like generics and a standard approach to iterating, and I had a bit of a love/hate relationship with it. I decided to play with it again doing Advent of Code and see what had changed."
date = 2026-01-04
+++

A number of years ago (I think we're talking a decade ago now, which makes me suddenly aware of how much time has passed), I was excitedly hopping from one language to the next on a hunt to find the one (or ones) which resonated with me the most, and came across this new language called Go.

Around the same time, there was interest at work in starting to use Go. This was when microservices were being hyped all over the place, so there was a push for us to jump onto this bandwagon, and to do so, we needed something better than PHP (our current backend language) to start building all of these shiny new microservices with. Go fit the bill; it was fast, had an async runtime (which was all rhe rage since NodeJS hit the scene), fast native compilation, and was "easy" to learn.

To begin with, I found Go a joy to use! It had a better type system than Javascript (TypeScript wasn't something I knew about about yet), it was much faster than PHP, and was much easier to use than the main other compiled language I knew at time time; C++. However, my programming language journey took me on towards Haskell and then Rust in rather short order, and I fell in love with their much more powerful trait based type systems, concepts around immutability, and things like tagged enums. As a result, I started working up a list of things I that I liked, but mostly that I didn't like, about Go, to eventually write about. I never did write anything, but for the sake of posterity, here were a few of the things I would ramble about:

- No generics!!!! (I used this many exclamation marks on the original note). I was frustrated that built-in types got special treatment here and I was unable to create my own such types. I had also used Java before it had generics, and it felt like Go was making the same mistake by not adding them.
- Because of the above, we also didn't get any generic functions for iterating over slices or maps, filtering values, mapping slice and maps and so on. Things that I was used to in Javascript and that worked great in Rust and Haskell.
- Variables in for loops were instantiated once, rather than being scoped to the block of the loop. This led to an annoying footgun around reusing these variables in goroutines, for instance.
- Pointers and potential nil values everywhere (which felt like a step back from the `Maybe` and `Option` types of Rust/Haskell).
- Inserting into a nil map panics, but inserting into a nil slice is ok (I thought this was a weird inconsistency, though sort of understand why).
- Sortof related, APIs like `slice = append(slice, item)` are messy, especially coming from languages like Rust and C++ which expose apis like `vec.push(item)` and do the reallocating under the hood.
- Things that should be warnings are hard errors, eg unused imports and variables. When I'm debugging things, I just want to see if everything compiles and don't want to be force to tidy things each time.
- No tagged enums. Instead, we have multiple return values, making it possible to use an error or value that shouldn't be used.
- No REPL (I must have been in my Haskell phase at the time of writing this one).
- No way to prevent users fro mconstructing zero valued instances of your types, which means it's possible for instances to exist which aren't properly initialized.
- Having to write all of your Go code in one place (`$GOPATH`) was a PITA. I wanted to put separate projects in different places, not have some sort of Go monorepository.

I did also have a few "good"s, like:

- `go fmt` is great to enforce a single style and eliminate style bikeshedding.
- Super easy to write and run tests (I was used to working with UIs and Javascript at the time, and this was always very painful by comparison).
- Execution speed and compilation times are both great.
- The async runtime, and async functions being the same "colour" as regular ones, is great. Haskell has this too, but Javascript had lots of callbacks and Rust may not have got `Future`s yet at all.

In any case, it's fair to say that I had a love/hate relationship with Go.

# So what's new in 2025

I figured that I'd have a go at doing this years [Advent of Code][aoc2025] problems in Go to reacquaint myself with it, mainly because I had read a little about Generics being added to the language a while back and thoght it was about time I actually tried them out!

From skimming the last few years of Go changelogs, the things which caught my eye essentially boiled down to:

- Modules (1.11)
- Block scoped variables in for loops (1.22)
- Generics (1.18)
- Iterators (1.23)

I'm sure there are also many significant things that didn't catch my eyes too!

The first two are quick wins: Modules basically allow you to write Go code outside of the old `GOPATH`, which is now the default way to write Go, and block scopes variables in for loops mean that writing code like this now works as you'd expect:

```go
for i := 0; i < 10; i++ {
    go func() {
        fmt.Println(i)
    }()
}
```

Previously, this would have (probably?) printed `9` 10 times, because every goroutine captures the same variable `i`, and by the time they execute, `i` has been incremented up to 9. Now, we get all of the values from 0-9 printed (in an undefined order), as each iteration sees and captures its own instance of `i`.

These were both frustrating things to run into, so it's really great to see them addressed nowadays!

## Generics

Generics are a huge new language addition which bring it in line with (almost?) every other modern language. Before generics, we had done things like using `go generate` when we wanted to reuse some code but with different types (we had an example in our codebase of a cache implementation that would be generated to handle the various types we wanted caching).

The main thing I find myself doing a lot in languages like `Rust` is wanting to wrap functionality up into custom data structures. For example, in Advent of Code this year I wanted a queue data structure (and was determined to not reach beyond the stdlib for anything), so I wrote myself a [double ended queue][deque] which could be used like:

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

I also ended up writing a `Heap` data structure which wrapped Go's `"container/heap"` interface into something nicer, a `Set` structure to wrap `map[T]struct{}` and expose useful functions for working with sets, and then things like a _depth first search_ type which was generic over the states being searched and let the user (well, me) define through functions how to transition from one state to the next, and when we have found a winner.

Some people seem to never run into this sort of need, but for me, this is amazing! I love splitting up code and packaging it into these sort of reusable structures, and with generics, I can now write this sort of thing without giving up any type safety (ie I can avoid working with `interface{}` types everywhere).

Go's generics feel conceptually similar to Rust generics (albeit a lot less powerful) in the sense that, whenever you use a generic parameter, you define the interface that a type must have in order to be usable in place of the generic parameter. The obvious use cases in Go for them that I can see are:

- Making container-like types (`Deque` / `Set` / `Tree`).
- Writing generic functions to operate over these sorts of types.

There are a couple of caveats, for instance you cannot write generic _methods_:

```go
type Foo struct {}

func (f *Foo) DoSomething[T any](val T) {
    // do something with a T
}
```

But you _can_ write generic standalone functions:

```go
type Foo struct {}

func DoSomething[T any](foo *Foo, val T) {
    // do something with a T
}
```

And you can of course write functions that use the generics defined on types:

```go
type Foo[T any] struct{}

func (f *Foo[T]) DoSomething(val T) {
	// do something with a T
}
```

I wasn't frustrated by this limitation while I was playing with Go, but if I had wanted to expand my container types, I might have wanted to add generic map/reduce type functions like this:

```go
package deque

type Deque[T any] struct{
    // impl
}

func (d Deque[T]) Map[R any](f func(T) R) Deque[R] {
    out := NewDeque[R]()
    for value := range d.Values() {
        out.PushBack(f(value))
    }
    return out
}

// usage:

stringDeque := intDeque.Map(func(val int) string {
    return val.String()
})
```

This isn't possible though, and instead such functions need to be freestanding:

```go
func Map[T any, R any](d Deque[T], f func(T) R) Deque[R] {
    out := NewDeque[R]()
    for value := range d.Values() {
        out.PushBack(f(value))
    }
    return out
}

// usage:

stringDeque := deque.Map(intDeque, func(val int) string {
    return val.String()
})
```

I believe this is because of issues around generic methods and interface satisfiability, but haven't dug into it. There are several other limitations that I didn't run into. For further reading, [here's a summary][generic-issues] of them, and [here's a discussion][generic-issues-discussion] around the lack of generics in methods.

# Iterators

I hadn't even notivced that Go had adopted any sort of iterator approach, and was very pleasantly surprised to see that iterators have now been integrated much more into Go. Iterators are great when there are several intermediate steps that you'd like to apply to the items in some data structure (for instance a slice), or when you want to provide back each of the values in some data structure for users to do something with, or just when you want to stream values back to a user as they become available.

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

Now, you're able to iterate over arbitrary things by writing functions/methods which return functions that match the special iterator type signatures (which are aliased as `iter.Seq[A]` or `iter.Seq2[A, B]`). This looks something like:

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

// usage:

for v := range myContainer.Values() {
    // do something with values
}
for k, v := range myContainer.KeysAndValues() {
    // do something with keys and values
}
```

This is great! Aside from the above, I also found myself writing some "utility" iterators which combine other iterators (mainly because I was used to having these sorts of things already in Rust, and wanted to see what they would look like in Go). Here's a function which takes two iterators and "zips" them together, returning a single iterator which advances both in lockstep, and ends as soon as either of the given iterators end:

```go
func Zip[A any, B any](as iter.Seq[A], bs iter.Seq[B]) iter.Seq2[A, B] {
	return func(yield func(A, B) bool) {
		nextA, stopA := iter.Pull(as)
		defer stopA()
		nextB, stopB := iter.Pull(bs)
		defer stopB()

		for {
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

Aside from being able to write your own iterators, we now have some handy iterators in the standard library, like `slices.Values` to iterate over the values in a slice (useful if you want to consume an iterator somewhere and have a slice), `maps.All`, `maps.Keys` and `maps.Values` for iterating over maps, things like `strings.Lines` for iterating over all of the lines in a string, and a bunch more. Some iterator functions in the stdlib end with `Seq` to differentiate themselves from the pre-existing non-iterator functions that came before.

Rust iterators are generally more difficult to write, because there is not yet any public-facing `gen` keyword to write functions which can `yield` multiple values. Also, sync and async code is written and handled very differently in Rust, and so we have `Iterator` for synchronous iteration and `Stream` (in a library) for async iteration. Go has none of these issues; iterators can be synchronous or asyncronous, and we can yield values as we go without any special syntax, because the Go runtime handles swapping between coroutines and so on under the hood where needed.

# Closing thoughts

Go has always moved slowly and carefully as a language (which I appreciate more and more as time goes on and I see languages and software in general always pushing to evolve and change, and often messing it up), but I really love how it's evolved over the past decade. Iterators and Generics add two of the big feature that I sorely missed to the language, and makes it possible for me to write the sort of code I want to write. Whle I still have various other gripes, the same is true of most languages. 

Now I will just hope that Go gets tagged enums and I'll be pretty chuffed!



[aoc2025]: https://github.com/jsdw/advent-of-code-2025
[deque]: https://github.com/jsdw/advent-of-code-2025/blob/main/utils/deque.go
[generic-issues]: https://go.googlesource.com/proposal/+/refs/heads/master/design/43651-type-parameters.md#issues
[generic-issues-discussion]: https://github.com/golang/go/issues/49085