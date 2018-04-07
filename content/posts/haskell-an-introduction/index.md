+++
title = "Haskell, and Why It Might Just Be My New Favourite Language"
description = "Coming from imperative programming languages like Javascript, Haskell might just be the most different language out there for me to learn, but here are several reasons why it also might be one of the best."
date = 2014-05-25
+++

As I am currently working mostly in front-end Web Development, Javascript is the language I have most experience in nowadays. In my spare time however, I've been doing what a lot of programmers do and searching for one language to rule all others. For me, that's a language that's concise and elegant in its construction, and one that is versatile enough that it lets the programmer create things similar to those features the language itself provides.

# Golang didn't quite cut it

Recently, I came across _Go_, Google's programming language (version 1.2). It had some nice features, including:

- A decent package system
- Implicit interfaces with duck typing
- Pretty simple parallel programming (at least from the point of view of someone coming from languages like C++ and Javascript) with safe communication via channels
- Full support for closures
- Block scoping of variables
- Strong typing
- Functions can be attached to any type (you can have a number with functions attached to it!)
- Extremely fast compile times.

The list no doubt goes on.

Go certainly has a lot to offer, but there are some significant omissions that I could never quite come to terms with. For one thing, Go lacks any sort of function overloading, so you can't do things like:

```go
func GetValue() Value { /* get default value */ }
func GetValue(idx int) Value { /* get value given an int */ }
func GetValue(idx string) Value { /* get value given a string */ }

```

Instead, you have give every variation a unique name like:

```go
func GetValue() Value { /* get default value */ }
func GetValueFromInt(idx int) Value { /* get value given an int */ }
func GetValueFromString(idx string) Value { /* get value given a string */ }`

```

Which I can't help but find ugly. Another thing I didn't like is that magic functions exist which you can't reproduce, for instance the _map_, of which both the type of key and stored value can be defined. This is not possible in any custom types, which sort of bugs me, as there is no form of type generics/templating in Go.

Some of these things can be overcome. Go has the notion of interfaces, which consist of a number of function signatures. Any type that has matching function signatures is automatically a member of that interface. Functions can then take in interfaces instead of types to be used more generically. Any type is a member of an empty interface. It also has type reflection and type based switches, which allows a function to react according to type, at a performance cost.

Overall though, I may well find myself using Go again, but I didn't gel with it as much as I would have liked.

# Enter Haskell

![Haskell Logo: using the lambda symbol in tribute to lambda calculus][haskell-logo]

Haskell is a very different kettle of fish to anything I has used before. In short, it is a pure, strongly typed, lazy functional language. Some things that I have come to like about it include:

- An interpreter, so you can play with and test things without having to go through a proper compile cycle. Haskell can also be used as a scripting language using the provided `runhaskell` binary, avoiding explicit compilation altogether.
- Laziness, which lets you keep your data and your processing separate, and pull off some rather cool feats like infinite lists.
- Purity. You can (almost always) guarantee in Haskell that, given the same arguments, a function will always return the same results. They don't modify global state or anything like that.
- An amazing type system. Coming from C++ templates, this is a breath of fresh air.
- Excellent type inference, while we're on topic. You could easy forget that Haskell wasn't a dynamic language at times, as you can often omit any mention of type and let Haskell figure out what they should be given the context.
- Functions can be glued together and reused very easily.
- Pattern matching and guards make writing functions more elegant.
- One can recreate imperative-like constructs in the language if they like, such is its versatility.
- In fact, almost anything in the language core can be created by the programmer.

Let's have a look at some of these points and see what the hell they actually mean, and why they are pretty neat. I'll start with laziness, as it's probably the most significant difference coming from an imperative world.

## Laziness can be an Asset

One of the most boggling concepts to get your head around when starting with Haskell, and something I am still learning, is this whole concept of laziness. One way to think about it is that in most languages, statements are evaluated inside out. That is, if you take a line like this in Javascript:

```
function doSomething(val){ return val + 4; }
function anotherFunc(val){ return val - 2; }
doSomething( anotherFunc(3)+12 );
```

before `doSomething` is called on line 3, first `anotherFunc(3)` is evaluated (which returns 1), and then `1+12` is evaluated, giving us 13. Finally, `doSomething(13)` can be evaluated, returning 17. We work from the inside out until everything is evaluated.

Haskell on the other hand is lazy, and only bothers to actually evaluate a statement when it needs the answer. You can think of it as working from the outside in when given an expression, but going only as far in as it needs to to get back what it needs.

The above would be something like this in Haskell:

```haskell
doSomething val = val + 4
anotherFunc val = val - 2
-- don't worry about the $. it just means, do everything to
-- the right of me before giving the result to what's on the left.
-- essentially, it saves needing extra brackets:
output = (doSomething $ anotherFunc 3)+12
```

where we have assigned a value to the final result. If `output` is not later used (for example, printed out to the console), the result of `(doSomething (anotherFunc 3))+12` is never computed. Laziness allows us to do things like:

```haskell
-- create an infinite list where every value is twice the previous one.
-- this just constructs a list where the first part is 1, and the next part
-- is the same list with values doubled in size:
myDoubles = 1 : map (*2) myDoubles
-- take the first 10 elements from this infinite list to use somewhere:
firstBatch = take 10 myDoubles

-- create a list of all fibonacci numbers:
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- ignoring the first 50 values, take 100 Fibonacci numbers:
someFibs = take 100 $ drop 50 $ fibs

-- create a list of all primes:
primes = 2: 3: sieve (tail primes) [5,7..]
   where sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /=0]
               where (h,~(_:t)) = span(< p*p) xs

```

Which I think you'll agree is pretty cool. There will be more efficient ways to get Fibonacci and prime numbers, but the end result is the same. These examples are helped by the fact that Haskell has an Integer number type that can be arbitrarily large.

Laziness can also be a stumbling block however, because if you don't understand how it works, seemingly simple programs can run into problems:

```haskell
-- countValue is given two numbers, val and count
-- if val > 0, it runs itself again, adding 1 to count and removing 1 from val
-- when val hits 0, count is returned.
countValue val count
	| val > 0 = countValue (val-1) (count+1)
	| otherwise = count

-- if I print output to my console, memory usage goes bonkers:
main = print $ countValue 1000000000 0

```

Why does the above program, which appears to be very simple, use up all of my RAM? Well, because `count` is not evaluated until it is necessary to print it to screen, a record showing what needs to be evaluated is instead built up every time we pass `count+1` to the next iteration of the function. Thus, instead of just incrementing the value of count each loop, Haskell stores something like:

```haskell
count = (((((0)+1)+1)+1)+1)...
```

It avoids actually performing the actual computation (adding 1) each step because its lazy and doesn't need it for anything yet, but it still has to remember what computation it'll eventually need to do! By the way, this record of what to compute is known as a _thunk_ in Haskell terminology.

Fortunately, when you understand the concept of laziness you can find ways to work around it if necessary. In this case, we can just tell Haskell that we want `count` to be evaluated when passed in to `countValue` by enabling a language extension called _BangPatterns_ and adding a `!` in front of count in the function declaration as so:

```haskell
{-# LANGUAGE BangPatterns #-}

countValue val !count
	| val > 0 = countValue (val-1) (count+1)
	| otherwise = count
```

All in all, laziness is one of the core fundamental differences between Haskell and any other language I've known. It makes it possible to do some pretty amazing things, like creating infinite lists representing things we might want to work with, like prime numbers or Fibonacci numbers. On the other hand, it can make it harder to reason about when your program will be using memory and CPU, especially if you are not used to the concept of laziness, which most of us won't be to begin with.

## Pure Functions

In Haskell, functions can't modify global state or anything like that. If you have a function that takes in some variables, then you can guarantee that it will return the same result whenever those variables are the same. This makes it very easy to test functions in isolation. Consider in Javascript I could do something like:

```
var a = 12
function doSomething(){
	return a++;
}
```
Every time `doSomething` is called, we get something different back. The language provides no guarantees about what will be returned. In Haskell, this sort of thing simply won't work:

```haskell
a = 12

main = do
	a = 13 --this line is not valid Haskell
	return ()

a = 13 --this line causes a compilation error, as "a" is defined already.
```

There is really no such thing as a variable at all. Everything is effectively a `const`, and once declared cannot be altered. As a result, there aren't really any loops either, because iterating over a loop requires a variable that can be updated. Instead, Haskell relies on recursion and list manipulation to take the place of loops.

Take the problem of printing out all numbers below 1000 that are multiples of 3 or 5 (problem 1 in [Project Euler][euler]). In Javascript one might do the following:

```
function getMultiples(){
	var out = [];
	for(var i = 0; i < 1000; i++){
		if(i % 3 == 0 || i % 5 == 0) out.push(i);
	}
	return out;
}

console.log( getMultiples() );
```

The equivalent in Haskell substitutes a traditional loop for a list which we filter to return the desired values:

```haskell
-- take a list from 0-999, and filter out all numbers not multiples of 3 or 5:
getMultiples = filter fn [1..999]
	where
		fn n = n `rem` 3 == 0 || n `rem` 5 == 0

-- print the sum of the above:
main = print $ sum getMultiples
```

Of course, sometimes you need to take in user input, print things out to the screen, etcetera. That's ok though, because you can write functions that take in a special object of type _IO_. `main`, the entry point in Haskell programs, has this type.

This semi-magical type _IO_ can be seen as a little box, which when asked for something from the real world, say a line of user input from the console, pops off and fetches it for you. Needless to say, it allows for the purity to hold in the sense that this box, like the real world, is always a little different, so if a function takes in this IO box, it is perfectly able to return a different value each time it does so.

### Pattern Matching

In Haskell and a bunch of other functional-esque languages, pattern matching is available to us. Pattern matching allows us to essentially overload functions based on the constructors used in arguments. The most common pattern match perhaps is matching a list by breaking it into its first and other elements:

```haskell

-- a list is really a recursive application of the list constructor,
-- where the thing to the left of (:) is a list item, and to the right
-- a list to append it to. [] is an empty list:
myList = 1 : 2 : 3 : []

-- this can also be written as:
myList = [1,2,3]
```

Knowing how lists are constructed, we can pattern match against values in
the constructor (seen either side of the constructor, `:`).


```haskell
-- lets redefine the map function, which applies some function to
-- every element of a list, returning the new list as a result.
-- we match using the list constructor (:) and the empty list []:
map' fn (a:rest) = (fn a):(map2 fn rest)
map' fn [] = []

-- usage same as map:

-- doubled = [2,4,6]:
doubled = map' (*2) [1,2,3]

-- added = [5,6,7]:
added = map' (+4) [1,2,3]
```

Pattern matching also extends to custom type constructors. Above, (:) is the list constructor. Here's how we declare a simple type:

```haskell
-- declare a 'Colour' type:
data Colour = Red | Orange | Yellow | Value Int

-- Colour constructors are on the right, and can be used to construct
-- Colour types as follows, noting that one of our constructors can take
-- an Int as an argument in this example:
iAmRed = Red
iAmValued = Value 100
```

Given this Colour type, we can write functions that pattern match according to constructor used:

```haskell
-- returns True if the Colour passed in is Red, False in every other case:
isRed Red = True
isRed _ = False

-- returns True if the Colour passed in is a Value over 100, False otherwise:
isOver100 (Value n) = n > 100
isOver100 _ = False

-- returns True if the Colour passed in is a Value of exactly 100, false otherwise:
isExactly100 (Value 100) = True
isExactly100 _ = False

-- thus, these statements are all true:
isOver100 (Value 101) == True
isOver100 Orange == False
isRed Red == True
isRed Yellow == false
```

As we can see, pattern matching helps ensure that by the time the body of the function even runs, we've already successfully matched against a constructor. Guards extend this idea to run code depending on conditional checks:

```haskell
numberToString n
	| n < 100 = "quite small"
	| n == 100 = "exactly 100"
	| n > 100 = "pretty big!"

-- given the above, these hold true:
numberToString 50 == "quite small"
numberToString 100 == "exactly 100"

```

Both guards and pattern matches are syntactic sugar for if/elseif and case expressions respectively, which are also available in Haskell. `if`, being an expression which always evaluates to a result, can be used inline in place of the ternary operator available in many other languages. This, for instance, is perfectly valid:

```haskell
-- return 1+2 if n < 100, or 100+2 otherwise:
getValue n = (if n < 100 then 1 else 100) + 2

-- also could be written as:
getValue n
	| n < 100 = 1 + 2
	| otherwise = 100 + 2
```

## An Amazing Type System

Haskell has the best type system I have used in a language, hands down. Basic types in Haskell can be defined and used really easily as we've done so above. Let's work from a similar example:

```haskell
-- define a type Colour, of which instances can be made by calling Red, Orange or Yellow,
-- which are the types constructor functions:
data Colour = Red | Orange | Yellow
```

Haskell has the notion of type classes, which are much like interfaces in other languages. A type class defines a set of functions that a type can then implement in order to be an instance of that class. Several built in type classes exist, for example `Eq`, which simply states that one can compare instances of the type against eachother to compare for equality (or non equality). To be a member, we need to define what either the `==` or `/=` operator does for our new type:

```haskell
--let's add colour to the Eq class:
instance Eq Colour where
	(==) Red Red = True
	(==) Orange Orange = True
	(==) Yellow Yellow = True
	(==) _ _ = False

--now, these hold true:
(Red == Red) == True
(Red == Green) == False
```

Haskell streamlines this by allowing the compiler to automatically figure out this instance code for some built in type classes like Eq, Read, Show, Ord, and Enum, for our custom type:

```haskell
data Colour = Red | Orange | Yellow deriving (Eq, Ord, Enum, Show, Read)

-- now, our Colour type is a member of these classes, so these hold True:

-- Eq, allows testing for equality or not:
(Red == Red) == True
(Red /= Orange) == true

-- Ord, allows for greater than/less than comparisons:
Red < Orange == True
Yellow > Orange == True

-- Enum, allows for ranges to be constructed:
[Red ..] == [Red,Orange,Yellow]

-- Show (allows output to console for this type):
show Red == "Red"
show Orange == "Orange"

-- Read (opposite of show; input from string to type). We need to tell
-- Haskell what type we're dealing with here with a ':: typename':
read "Red" :: Colour == Red

```

You may notice that things like Enum and Ord are based on the order in which the constructors were defined in the first place. As well as using built in type classes, we can define our own. This effectively lets us define custom interfaces for our types:

```haskell
-- define a silly class (think of it as an interface):
class Syllable a where
	syllables :: a -> Integer

-- make Colour a member of it:
instance Syllable Colour where
	syllables Red = 1
	syllables Orange = 2
	syllables Yellow = 2

```

As in other languages that have interfaces, functions in Haskell can be crafted that will only take in arguments that satisfy some type class or collection of type classes, for example:

```haskell
-- we give this function a type signture to make clear
-- what types it supports rather than let Haskell guess.
countSyllables :: (Syllable a) => [a] -> Integer
-- this function counts up all the syllables in whatever
-- we give it:
countSyllables list = foldl (+) 0 $ map syllables list
```

This only scratches the surface of types in Haskell, but needless to say they are very powerful and versatile yet remain simple to use. Up until this last example, I have not even added type signatures to anything; Haskell is perfectly happy to figure it out for itself on the whole, though occasionally you will want to be explicit to remove any ambiguity.


## Function Composition

The way in which functions work in Haskell makes them very flexible with regard to composing them together into larger functions. Let's look at some type signatures to get an idea for how they work:

```haskell
-- the type signature for a number could be:
simpleType :: Integer
-- which is followed by the actual declaration:
simpleType = 12

-- the type signature for a function could be more involved:
greaterThan :: (Ord a) => a -> a -> Bool
greaterThan a b = a < b

```

In the first case, we say that simplyType has a type of Integer. We then go on to define it as being 12. In the second case, we say that greaterThan is a function that takes in some type which must be a member of the Ord typeclass. This returns a function which takes in another thing of the same type, which returns a Bool. So, functions sort of take in one argument at a time. Using _ghci_, the Haskell interpreter, you can create functions on the fly (using notation akin to being inside a _do_ block, so adding let before definitions) and inspect their type:

```haskell
> let greaterThan a b = a < b
> :type greaterThan
greaterThan :: Ord a => a -> a -> Bool

> :type greaterThan 9
greaterThan 9 :: (Num a, Ord a) => a -> Bool

> :type greaterThan 9 12
greaterThan 9 12 :: Bool

```

Here, we see that to begin with, greaterThan is automatically assigned the type I explicitly gave it above. The type of `greaterThan 9` is more restricted, as we have provided an instance of `a`, and know now that it is a number of some sort. It has also lost an `a ->` from its type signature, as that argument has now been provided, so it can only take in one more `a`. Finally, `greaterThan 9 12` is fully applied, so the other `a ->` is gone, leaving something of the final type, Bool.

Given the nature of functions, we can do things like:

```haskell
-- this defines a new function by partially applying
-- greaterThan, which can take in one argument:
greaterThanNine = greaterThan 9

-- we provide the final argument to get the final
-- result back:
greaterThanNine 6 == False
greaterThanNine 12 == True
```

Haskell also has composition operators (which, by the way, you could define yourself). For instance:

```haskell
times2 a = a * 2
-- could also be written as: times2 = (*2)

minus4 a = a - 4
plus10 a = a + 10

-- this function multiples a value by 2, then minuses 4,
-- then adds 10, returning the result:
doTheAbove = plus10 . minus4 . times2
```

To do the same in Javascript would be rather more verbose:

```
function times2(a){ return a*2; }
function minus4(a){ return a-4; }
function plus10(a){ return a+10; }

// this is not only more verbose but not type checked at compile time,
// so doTheAbove([1,2,3]) would only throw an error at runtime, whereas
// haskell would catch the issue at compile time..
function doTheAbove(a){
	return plus10( minus4( times2(a) ) );
}
```

Functions can also take in other functions:

```haskell
-- this (silly) function takes in a thing, and a function which takes in
-- a thing of the same type and returns a Bool, and then returns a string:
isValid :: (a -> Bool) -> a -> String
isValid fn val
	| (fn val) == True = "Yup"
	| otherwise = "Nope"

-- we can partially apply this by giving it a function:
lessThan10 = isValid (\n -> if n < 10 then True else False)

-- then, these hold true:
lessThan10 13 == "Nope"
lessThan10 5 == "Yup"

```

The same (but not type checked) in Javascript would look something like:

```
function isValid(fn, a){
	if( fn(a) ) return "Yup";
	return "Nope";
}

// bind the first arg of isValid to a function:
var lessThan10 = isValid.bind(null, function(a){
	if(a < 10) return true;
	return false;
});

// these hold true:
lessThan10(13) == "Nope"
lessThan10(5) == "Yup"
```

We can see that the bracketless syntax of Haskell just makes sense when things like composing and partially applying functions are so common in it.

# Wrapping Things Up

Well, if you made it this far, well done! I hope that I have helped to demonstrate some of the things that make Haskell a language worthy of your attention. I have found it to be a rather steep learning curve to date, but only really because of how different it is from the languages I have learned to date, rather than because the language itself is overly complex. Things like Monads are still slightly scary territory, though I understand the principles behind them now (they just represent a more advanced combination of the above features rather than anything new).

It is unfortunate in a way that Haskell is so different from anything that I have learned before, in that given some task I will still feel more confident using a "conventional" language to get the job done, and thus straying away from the likes of Haskell. Regardless of whether Haskell carves out a solid place in my toolbox, it has definitely expanded my insight into what is possible.

[haskell-logo]: haskell.jpg
[euler]: https://projecteuler.net

