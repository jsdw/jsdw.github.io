+++
title = "Haskell: Another Lens Tutorial"
description = "As someone who just got lenses after several days of trying to find out about them, I thought I'd reflect on this new understanding by showing you what a lens really is, starting from scratch."
date = "2014-08-29"
+++

I recently stumbled across the `lens` package when I was working with JSON data. I was keen to start working with lenses more, but without really knowing what they were, at least at some level, I was hesitant. I spent the next few days on and off reading about lenses and trying to make sense of them. For me, the turning point was [this post][lenses] by Jakob Arnold, so I very much encourage reading that as well. Anyway, let's get on with it. All of the code used in the examples is available in a single file [here][code], ready to be loaded into ghci with a `ghci code.hs` command, so that you can easily follow along.

# What's so good about lenses anyway?

In most imperative languages, data is mutable, and so you are provided with a nice syntax to update fields in data structures. Take the Javascript below:

```js
//we have a "person":
var person = {
	name: "James",
	age: 28,
	address: {
		house: 42,
		street: "Some Road",
		city: "London"
	}
};

//it's simple to view his house number, or update it:
var houseNumber = person.address.house;
person.address.house = 29;
```

Haskell on the other hand doesn't allow you to mutate data like this. Instead, if you want to update a value you make a copy of the whole lot with the single value of interest changed. The Haskell equivalent to the above is something like:

```haskell
-- define our person and address data structures:
data Person = Person {
	name :: String,
	age :: Int,
	address :: Address
}

data Address = Address {
	house :: Int,
	street :: String,
	city :: String
}

-- make a new person:
james = Person {
	name = "James",
	age = 28,
	address = Address {
		house = 42,
		street = "Some Road",
		city = "London"
	}
}

-- get his house number by first getting the address from james,
-- and then getting the house number from address:
houseNumber = house (address james)

-- set his house number (creating a new person in the process):
updatedJames = james { address = (address james) { house = 43 } }
```

First, we had to create some data types, which although more verbose give us nice compile time guarantees that we won't ever set a house number to a `String` or something. Next, we use the accessor functions defined along with the record syntax to get at the address, and then the house number in that address. Setting is rather clumsy, and uses the record syntax style to update things. We can't compose these well, so if we wanted a shorthand way to access properties we'd have to define them all ourselves:

```haskell
-- get the persons address (address person) and update house/street to a new value,
-- also updating person to use this updated address:
setHouse person value = person { address = (address person) { house = value }  }
setStreet person value = person { address = (address person) { street = value }  }

newPerson1 = setHouse person 45
newPerson2 = setStreet person "New Street"
```

You can imagine that this could all get very tedious, especially when you have to go deeper and deeper into the structures to make a change.

# Creating our own lens type

Lenses are designed to make this sort of thing simpler. The general idea is that you define lenses to get at each property in your data structure (the lens package comes with a template Haskell function that'll do this for you, but we'll look at doing it all ourselves). You can then compose these lenses together to dig deeper and deeper into some structure, and then we can view and update the value focused in on.

on first attempt, you might define a general lens data type like:

```haskell
data Lens' thing prop = Lens' {
	view' :: thing -> prop,
	update' :: (prop -> prop) -> thing -> thing
}
```

So that we have this general type that supports the notion of viewing and updating values. `thing` and `prop` are both generic types that can refer to any concrete thing or prop. `thing` is the thing you want to dig into and `prop` is the value you want to get or update.

In our simple lens data type, we can see that `view` takes in some `thing`, and give us back the `prop` we're interested in. `update` takes in a function that changes a `prop` into a new `prop`, and a `thing` to perform this update on, and is expected to return a new `thing` that reflects this update. We could also define a set function which just updates some `prop` to a new value, based on this update function:

```haskell
set' :: Lens' thing prop -> prop -> thing -> thing
set' ln newValue thing = (update' ln) (\_ -> newValue) thing
```

We could define lenses using this type as follows:

```haskell
addressLens :: Lens' Person Address
addressLens = Lens' address (\fn thing -> thing { address = fn (address thing) })

cityLens :: Lens' Address String
cityLens = Lens' city (\fn thing -> thing { city = fn (city thing) })
```

You might wonder what the advantages of these are over the record syntax haskell already has. Well, for one thing, they compose a lot better. Here's how I might combine the address and city lens to create a new lens, or just a composite get/update function:

```haskell
personToCityLens :: Lens' Person String
personToCityLens = Lens' ((view' cityLens).(view' addressLens)) ((update' addressLens).(update' cityLens))

--alternately, don't create a new lens and just compose the city and address lenses on the fly:
viewCity person = (view' cityLens . view' addressLens) person
updateCity fn person = (update' addressLens . update' cityLens) fn person

--this could be abbreviated as was done in the lens definition to:
viewCity = view' cityLens . view' addressLens
updateCity = update' addressLens . update' cityLens
```

You might notice that function composition seems to be flipped between our `view` and `update` functions; we'll have a little look why later, but if you try creating an update function that composes two lenses in this way you'll naturally stumble into the same thing. Given our new lenses, we can have a play with them in `ghci` to make sure they do what we expect:

```haskell
> view' addressLens james
Address {house = 42, street = "Some Road", city = "London"}
> view' personToCityLens james
"London"
> set' personToCityLens "Paris" james
Person {name = "James", age = 28, address = Address {house = 42, street = "Some Road", city = "Paris"}}
> update' personToCityLens (++"!!!!") james
Person {name = "James", age = 28, address = Address {house = 42, street = "Some Road", city = "London!!!!"}}
> (update' addressLens . update' cityLens) (++"!!!!") james
Person {name = "James", age = 28, address = Address {house = 42, street = "Some Road", city = "London!!!!"}}
```
So, things are a little better already, but our simple lenses don't compose as well as we'd like still; for one thing it would be nicer if we could decouple the action (view/update in this case) from the lenses in our composition. Creating a data type also means that to use or create lenses elsewhere we have to import this type.

# Looking at the _real_ lens

One way to resolve both of these concerns is to represent this "lens" concept in terms of a single function, rather than a viewing and an updating function. That way, instead of creating a new data type, we just require that lenses match the function signature in order that they can be used together in a standard way. To make things a little simpler, we can create a type alias for this lens function signature.

As it turns out, that's exactly what's been done!

```haskell
-- we need to add this to the top of our file now:
{-# LANGUAGE RankNTypes #-}

--the lens type alias:
type Lens thing prop = Functor f => (prop -> f prop) -> thing -> f thing
```

## A brief interlude into RankNTypes

You'll need to use the `RankNTypes` extension for this to work. Briefly, if you pass a function into a function, we have to declare the types that the function takes in and returns (albeit with a generic letter). To pass in a function that will work on every type (potentially subject to some bounds like `Functor f`) we need to be a little more explicit in some cases. We enable `RankNTypes` to make this possible. Example code is the easiest way to summarize the issue:

```haskell
{-# LANGUAGE RankNTypes #-}

-- for any type signature, we need to declare the variables we'll use with forall.
-- and then declare any constraints on them (Functor f for instance). ghc does this
-- implicitly for us by declaring all variables at the top, so these are the same:
lens :: Functor f => (prop -> f prop) -> thing -> f thing
lens :: forall f prop thing. Functor f => (prop -> f prop) -> thing -> f thing

-- but what about if we used it as part of another function, say:
workWithLens a ln :: a -> (Functor f => (prop -> f prop) -> thing -> f thing)

-- the problem here is that the variable declaration and constraint (Functor f)
-- are no longer declared in the same place.
workWithLens a ln :: forall a prop thing f. a -> (Functor f => (prop -> f prop) -> thing -> f thing)

-- if we were writing this out by hand without a type signature, we'd avoid
-- the problem by moving the constraint to the front as in:
workWithLens a ln :: forall a prop thing f. Functor f => a -> ((prop -> f prop) -> thing -> f thing)

-- which is just the same as the following, as ghc adds our top level declarations:
workWithLens a ln :: Functor f => a -> ((prop -> f prop) -> thing -> f thing)

-- however the alternate approach is adding a new variable declaration in the
-- inner function, which is what RankNTypes would do for us when we add a class
-- constraint:
workWithLens a ln :: forall a prop thing. a -> (forall f. Functor f => (prop -> f prop) -> thing -> f thing)
```

It's important to note here that where the variable is declared with `forall` is important. If a variable is declared at the front of the type signature then it can only be one type throughout the signature. If the variable is declared with `forall` in a nested function, it can only be one type throughout that function, but the function itself could be called with many different types. Let's have a quick look at this with an example:

```haskell
-- we don't know anything about 'a', but we can, say, wrap it up:
justA :: a -> Maybe a
justA a = Just a

-- with implicit forall added to declare 'a', we have:
justA :: forall a. a -> Maybe a

-- say we want to wrap a pair of values; we'll declare a function like this
-- (adding an explicit forall as ghc would for clarity):
wrapPair :: forall a t x y. (a -> t a) -> (x,y) -> (t x, t y)
wrapPair wrappingFn (x,y) = (wrappingFn x, wrappingFn y)

-- this isn't good enough though, because wrappingFn now expects an 'a'
-- but is given a 'b' and a 'c', both potentially different! To fix, declare
-- a new variable inside the scope of wrappingFn and say that it can be anything:
wrapPair :: forall t x y. (forall a. a -> t a) -> (x,y) -> (t x, t y)

-- since ghc declares top level variables for us, this can be abbreviated to:
wrapPair :: (forall a. a -> t a) -> (x,y) -> (t x, t y)
```

By declaring new variables in nested functions, we essentially scope the variables to those nested functions, allowing that, just like if the function was declared at the top level, the variable can take on any type its allowed to throughout the life of its scope. `wrapPair`, above, now expects to be given a function which can work on any value `a`, and thus itself can pass any type to the function during the course of its execution.

## Anyway, back on topic..

So, we have our general lens type alias:

```haskell
type Lens thing prop = Functor f => (prop -> f prop) -> thing -> f thing
```

This is basically the exact function signature as we used for the `update` function, except that we have declared that the first function passed in to it `(prop -> f prop)` returns the `prop` type wrapped in some `Functor` type (but we don't know which!), and that we want the output `thing` to be wrapped in that same `Functor` f.

You might be wondering why we've bothered wrapping anything in a `Functor`. Surely it just makes everything more complicated! I believe the main reason is to allow a function that matches this lens signature to be used to view the property we focus on as well as just update it. Without functor wrapping, the signature is just `(prop -> prop) -> thing -> thing`; a function which takes in a function which updates or sets the `prop` to some value, and a `thing` to do the setting on, and then which returns a `thing`. To view the prop though, we really need it to return `prop`!

Functors allow us to get around this, as we can create a `Functor` instance that hides the `prop` inside of itself but looks otherwise like an `f thing`. We'll see how with the `view` function.

Since `f` could be _any_ Functor instance, the only thing we can use to work with it is a function that is common to all `Functor` instances; `fmap`. The other thing you might notice is that when the lens applies the first function `(prop -> f prop)` to some `prop`, it obtains this elusive Functor `f` with a prop inside. Since it doesn't know how to make an instance of this functor itself (which Functor would it make?), it can instead be cunning and use `fmap` to change the contents of the `f prop` it gets back to turn it into an `f thing`. In fact, this is the only way to make an `f thing`, so any lens function we create will inevitably end up using `fmap`.

# Defining a `view` function

Let's define a `view` function that can use a lens and some item to focus in on to give us back the thing we focus in on:

```haskell
-- our view function takes in a lens which has the above type, and a thing to use the lens on,
-- and needs to return the prop.
view :: Lens thing prop -> thing -> prop

-- which, expanded out, looks like:
view :: ((prop -> f prop) -> thing -> f thing) -> thing -> prop

-- well, the lens function expects a (prop -> f prop) function, and a thing, to be passed to it,
-- so lets start there:
view ln thing = ln (\p -> ???) thing

-- first off, what goes in place of ???. We need to wrap prop in some Functor (remember, the lens
-- function doesn't care which as it'll happily work with any) to match the lens signature.
-- We'll get the result back wrapped in the same functor (f thing), so whatever Functor we use,
-- we'll need to get our thing back out to return it. It'll have to look something like:
view ln thing = getFromFunctor $ ln (\p -> SomeFunctor p) thing
```

What we need here is something that can act as a `Functor` and hide away some value while pretending to be something else. Something that looks like this would do the trick:

```haskell
--a data type parametrised by two variables,
--but which only really cares about one:
data Const a b = Const { getConst :: a }

--a functor normally holds one type inside it, like a monad.
--so we partially apply the Const type to meet that criteria:
instance Functor (Const a) where
	fmap _ (Const a) = Const a

--this means that (Const a) can be our Functor, where in fact 'a'
--is the type we'll eventually want back, and b is just whatever
--type we need to make the Functor match 'f thing' when used above.
```
What this means is that our `Functor` is actually `(Const prop)`, so when we return `f thing`, we're actually going to be returning `(Const prop) thing`, which we can then unwrap to get our `prop` back out!

`Const` is already defined in `Control.Applicative`, but it's useful to see how simple it is to define. Using Const to hide away and then extract our prop, our final definition of view is:

```haskell
view :: Lens thing prop -> thing -> prop
view ln thing = getConst $ ln (\p -> Const p) thing

-- indeed, we can simplify it, noting that the `Const` constructor is a
-- valid function in itself to pass to a lens:
view ln thing = getConst $ ln Const thing
```

Awesome! So now we've defined a `view` function which has the same net result as our simpler `view'` one, but instead makes use of our single general purpose lens function to do so.

# Defining our `update` and `set` functions

We can also define an `update` function (actually called `over` in the real lens library) which modifies a property:

```haskell
-- our original update was very similar to the lens type signature. We just
-- need to mangle some Functor thing in to match the lens funcs type signature:
update :: Lens thing prop -> (prop -> prop) -> thing -> thing
update ln fn thing = ??? $ ln (\p -> ??? (fn p)) thing

-- we give the lens a function which applies our function to the property, but
-- then has to wrap it into a functor as well. As with view, we get back a thing
-- inside that same functor, so we need to unwrap it. It'll look a bit like:
update ln fn thing = getFromFunctor $ ln (\p -> SomeFunctor (fn p)) thing
```

Once again we need a Functor to wrap our value in, but this time we don't need it to do anything at all. A simple definition of a Functor that just holds a value for us is (defined in `Control.Monad.Identity`):

```haskell
data Identity a = Identity { runIdentity :: a }

instance Functor Identity where
	fmap fn (Identity a) = Identity (fn a)
```

With this, we can define update as:

```haskell
update :: Lens thing prop -> (prop -> prop) -> thing -> thing
update ln fn thing = runIdentity $ ln (\p -> Identity (fn p)) thing

-- defining 'set', to just set the property to a value rather than
-- apply some function to it, becomes very easy now too in terms of
-- update:
set :: Lens thing prop -> prop -> thing -> thing
set ln newValue thing = update ln (\_ -> newValue) thing

-- indeed, we can simplify update and set a little if that floats your
-- boat. A little point free notation for update:
update ln fn thing = runIdentity $ ln (Identity . fn) thing

-- and the const function for set (not to be confused with Const),
-- along with a little point free style to remove thing:
set ln newValue = update ln (const newValue)
```

Now we have functions that, given some lens, can view, set and update the value that the lens points to. The next step is to actually define lenses for things.

# Defining lenses

so, to recap, defining a lens means defining a function of the type `Functor f => (prop -> f prop) -> thing -> f thing`, where `prop` is the value we want to focus in on and `thing` is the thing that we're starting from. Remember out `Person` and `Address` data? let's make a couple of lenses for those:

```haskell
-- to recap, our simple data types:
data Person = Person {
	name :: String,
	age :: Int,
	address :: Address
}

data Address = Address {
	house :: Int,
	street :: String,
	city :: String
}

-- we made simple lenses to get to the city before. let's use our
-- better, more general lens formula now to do the same!

-- our alias type signature neatly tells us what we're starting
-- with and what we're focusing on:
betterAddressLens :: Lens Person Address

-- so, our lens function takes in some function fn, which acts to update
-- the thing we're focusing on, but also wraps it in a functor. We'll
-- do that bit first then, by applying it to the persons address:
betterAddressLens fn person = ?? (fn $ address person)

-- that function returns a new Address wrapped in some functor (we dont know which, remember).
-- We need to turn our functor wrapped Address into a functor wrapped Person then, remembering
-- to update the Person's Address to the new address in the process:
betterAddressLens fn person = fmap (\newAddy -> person { address = newAddy }) (fn $ address person)

-- now we know the format, do the same sorta thing to go from an Address to a city:
betterCityLens :: Lens Address String
betterCityLens fn addy = fmap (\newAddy -> addy { city = newAddy }) (fn $ city addy)

-- and again to go from a Person to a name:
betterNameLens :: Lens Person String
betterNameLens fn person = fmap (\newName -> person { name = newName }) (fn $ name person)

-- in fact, this can be done so mechanically that the lens package provides a template
-- makeLens'' function to do it for you.
```

Now we have some lenses, and our `view`, `update` and `set` functions to work with them, let's have a play in ghci:

```haskell
> james
Person {name = "James", age = 28, address = Address {house = 42, street = "Some Road", city = "London"}}
> view betterAddressLens james
Address {house = 42, street = "Some Road", city = "London"}
> view betterNameLens james
"james"
> set betterNameLens "Bob" james
Person {name = "Bob", age = 28, address = Address {house = 42, street = "Some Road", city = "London"}}
```

Digging into nested properties is also amazingly simple, as lenses compose in a way reminiscent of property access in an imperative language (in other words, backwards compared to most things):

```haskell
> view (betterAddressLens.betterCityLens) james
"London"
> set (betterAddressLens.betterCityLens) "Paris" james
Person {name = "James", age = 28, address = Address {house = 42, street = "Some Road", city = "Paris"}}
> update (betterAddressLens.betterCityLens) (++"!!!!!") james
Person {name = "James", age = 28, address = Address {house = 42, street = "Some Road", city = "London!!!!!"}}
```

We seem to have achieved our lens related aims then! our lens functions compose easily, and are separate from the actions that we wish to take (eg `view`, `set` or `update`) given the focus that the lenses give us. Let's take a quick detour and see how they compose the way they do.

# How do lenses compose?

Let's look at a concrete example to work this out, using our `betterAddressLens` and `betterCityLens`. let's look at some type signatures:

```haskell
-- the general lens type signature:
type Lens thing prop :: Functor f => (prop -> f prop) -> thing -> f thing

-- betterAddress:
betterAddressLens :: (Address -> f Address) -> Person -> f Person

-- betterCity:
betterCityLens :: (City -> f City) -> Address -> f Address

-- and the dot operator (.):
(.) :: (b -> c) -> (a -> b) -> a -> c
```

Plugging our lenses into the type signature for the `(.)` operator leads to the following:

```haskell
-- using the dot operator and lining up its type signature with our lenses,
-- we can see that the result is something of the same signature (ie still a lens)
-- but that goes from a Person to a City (String):
((         b             ->         c           ) . (      a          ->          b            )) -> (      a          ->         c           )
(((Address -> f Address) -> (Person -> f Person)) . ((City -> f City) -> (Address -> f Address))) -> ((City -> f City) -> (Person -> f Person))
```

So we can see here that lenses compose forwards as a result of the functions type signature. This is just like how our simple lens update function happened to compose forwards like this in our first lens composition examples, except now our lenses are independent of the actions used on them.

# Where do I go from here?

Well, one thing that I have not mentioned is that the _real_ Lens type alias has four parameters, not two! The reason is very simple though, so lets have a quick look at that:

```haskell
-- the lens signature we've been working on:
type Lens thing prop :: Functor f => (prop -> f prop) -> thing -> f thing

-- the full lens signature looks more like:
type Lens thing newthing prop newprop :: Functor f => (prop -> f newprop) -> thing -> f newthing
```

This is basically so that, if needed (and possible), the property can be changed into a different type when it's updated. This of course means the whole thing's type will also change, so we add an extra parameter for each. This has no use in records as the types can't change, but if we take something like a tuple, there is no reason why an update cant result in a tuple with different types inside, for example going from `(12, "hello")` to `([1,2,3], "hello")` in the course of an update. The function signatures of both types of lens are otherwise identical, and so they can be used interchangeably (although using a simple lens in the mix somewhere will prevent any types from changing).

Now you (hopefully) have an understanding of what lenses are (just functions that match a certain signature), you would be wise to explore more of the `lens` package. There are loads of infix operators to sugar coat things, and additional functions defined that build on this simple foundation to offer a whole range of possibilities. There are also convenience functions like `makeLens''` which uses template Haskell to magically generate you lenses for record types to save you the legwork, among others.

# Summary

Some tidbits I hope you take away from this:

1. This `lens` thing is **just a function** type alias.
2. Because anything matching the right function signature is a lens, we can make lenses (as above) without depending on any package!
3. The fundamental concept being the `lens` idea, as I hope I've shown, is actually a pretty simple one.

Finally, all of the code used above is available [here][code] ready to be loaded into `ghci` and played with. Thanks for reading!


[lenses]: http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html
[code]: code.hs