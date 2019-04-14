+++
title = "Haskell: Some Awesome Language Extensions Explained"
description = "A guide covering some of the more awesome/useful/interesting looking Haskell language extensions that I've come across so far, with plenty of examples."
date = 2015-08-22
[extra]
created = "2015-07-04"
toc = 1
+++

Haskell language extensions each add something new to the language. Unlike most languages where such extensions would typically be discouraged for reasons of support, stability or unstable API, many haskell extensions have been around for a while, are used often, and will probably make it into some future revision of the Haskell language spec.

Let's take a look at some of the haskell extensions out there and see what they are good for. This is going to be code heavy, as it is mostly a translation of example code and comments to structured blog post, and I expect I'll update it as I come across more extensions I find interesting!

> Go [here][example-code] to get all of the code used below, ready for you to load straight into GHCI and have a play with. Tested using GHC 7.10.1

# ViewPatterns

View patterns let you run arbitrary computations anywhere you can use a pattern match, performing the actual pattern match on the result of this computation. Lets see what this looks like:

```haskell
someList = [("london",10), ("paris",12), ("sydney",4)]
findIn = flip lookup

-- no view patterns: extract a value from the map, using default if it's not there.
getOrDefault name = case mRes of
    Just val -> val
    Nothing -> 5
  where mRes = findIn someList name

-- view patterns: applies input argument to "findIn someList" and looks to see
-- whether the result matches the pattern Just val. falls to next case if not.
getOrDefault' (findIn someList -> Just val) = val
getOrDefault' _ = 5
```

View patterns can also make use of variables declared before them in the list of arguments to a function. In this case, the `list` variable:

```haskell
-- no view patterns: take in list as well.
getOrDefaultList list name = case mRes of
    Just val -> val
    Nothing -> 5
  where mRes = findIn list name

-- view patterns: partial application can use prior arguments as well:
getOrDefaultList' list (findIn list -> Just val) = val
getOrDefaultList' _ _ = 5
```

They can be arbitrarily nested and exist anywhere a pattern match can:

```haskell
-- no view patterns: use list above to convert values to integers and
-- sum them up. 0 if not found in list.
-- eg. sumList ["london","paris"] == 22
sumList (a : as) = case mRes of
    Just val -> val + sumList as
    Nothing -> sumList as
  where mRes = findIn someList a
sumList [] = 0

-- view patterns simplify it a little:
sumList' ((findIn someList -> Just val) : as) = val + sumList as
sumList' (_ : as) = sumList as
sumList' _ = 0
```



# PatternSynonyms

Pattern synonyms allow you define aliases for pattern matches. They allow you to abstract away details of your data structure without introducing the extra overhead of converting to other types, and play nice with view patterns too! First we'll declare a few basic types and functions to play with:

```haskell
data Day = Sunday
         | Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
      deriving (Eq,Show)

data Time = Time {
        hour :: Int,
        minute :: Int
    } deriving (Eq,Show)

data DayTime = DayTime {
        day :: Day,
        time :: Time
    } deriving (Eq,Show)

-- print the time, padding it.
printTime :: Time -> String
printTime (Time h m) = pad (show h)++":"++pad (show m)
  where pad l@(_:_:_) = l
        pad l@(_:_) = "0"++l

-- a function to print the day and time given a DayTime:
printDayTime :: DayTime -> String
printDayTime (DayTime d t) = "It's "++show d++", "++printTime t
```

Now, lets create some basic pattern aliases:

```haskell
-- some simple patterns to act as aliases for certain DayTimes:
pattern SundayNoon     = DayTime Sunday (Time 12 00)
pattern MidnightFriday = DayTime Friday (Time 00 00)
```

Making use of these in our console:

```haskell
Main> printDayTime SundayNoon
"It's Sunday, 12:00"
Main> printDayTime MidnightFriday
"It's Friday, 00:00"
```

We can use variables in patterns as well:

```haskell
pattern Sun h m = DayTime Sunday    (Time h m)
pattern Mon h m = DayTime Monday    (Time h m)
```

These can be used as follows:

```haskell
Main> printDayTime (Sun 15 30)
"It's Sunday, 15:30"
```

So far, there has been a one to one mapping between pattern and data. Thus, we can treat patterns as if they were data.
`Sun 12 00 == SundayNoon == DayTime Sunday (Time 12 00)`. However, some patterns are one way; one pattern could match
many values. One way patterns use `<-` instead of `=` to denote this.

```haskell
-- match any DayTime that is sunday or monday using these:
pattern AnytimeSun <- DayTime Sunday _
pattern AnytimeMon <- DayTime Monday _

-- a function that prints the day when given some DayTime value:
whichDay AnytimeSun = "Sunday!"
whichDay AnytimeMon = "Monday!"
whichDay _          = "Some other day!"
```

These patterns no longer represent a value but a range of values - anything that matches them - so we can use them in place of function parameters to destructure them, but we cant use them in place of values.

```haskell
main> printDayTime AnytimeSun
BZZZT! error! what time should I print??
```
We can match the whole daytime as in other pattern matches:

```haskell
whichDay' d@AnytimeSun = "Sunday "++printTime (time d)
whichDay' d@AnytimeMon = "Monday "++printTime (time d)
whichDay' _            = "Other."
```

One way patterns can also hand back data in variables which might look a little nicer, especially with complex matches. Here we give `t` back so that we can make use of it when we destructure someting:

```haskell
pattern SunT t <- DayTime Sunday    t
pattern MonT t <- DayTime Monday    t

whichDay'' (SunT t) = "Sunday, "++printTime t
whichDay'' (MonT t) = "Monday, "++printTime t

--we can of course mix and match pattern synonyms and normal
--destructuring too. first success wins as normal:
whichDay'' (DayTime _ t) = "Other day, "++printTime t
```

One of the really cool things about `PatternSynonyms` is that they can be used in conjunction with `ViewPatterns` to run arbitrarily complex functions at match time. Here we run `isMorningHour` and see whether the answer matches `True`:

```haskell
isMorningHour t = t < 12

pattern Morning <- DayTime _ (Time (isMorningHour   -> True) _)

timeOfDay Morning = "It's morning"
timeOfDay _       = "It's not morning"
```

View patterns can also give back arbitrary variables. here we expect a tuple of `(True,h)` back, where we allow `h` to be made use of at match time.

```haskell
isMorningHour' h = (h < 12, h)

pattern MorningT h m <- DayTime _ (Time (isMorningHour' -> (True,h)) m)

timeOfDay' (MorningT h m) = "It's morning, "++show h++":"++show m
timeOfDay' _              = "It's not morning"
```



# FlexibleInstances

Allows you to define typeclass instances in a more flexible way. Let's look at the problem they solve by setting up a simple class with a few instances.

```haskell
-- a simple class for seeing whether something is "truthy"
class Truthy a where
    truthy :: a -> Bool

instance Truthy Bool where
    truthy = id

instance Truthy Char where
    truthy c = not $ elem c [' ','\n','\t']

instance Truthy Int where
    truthy i = i /= 0
```

Without flexible instances, we could not do:

```haskell
instance Truthy String where
    truthy s = length s /= 0
```

We'd get told that instance declarations need to take the form `T a1..an` where `a1..an` are type *variables*, but `String == [Char] == [] Char`, and `Char` is not a type variable but a fixed type.  So basically, `[] a` would be fine, but `[] Char` would not. `FlexibleInstances` removes this restriction, and more generally adds flexibility to the format that your instances can take.

## Avoiding `FlexibleInstances` (in some cases).

An alternate way around the above issue is to enforce that `a` has to be `Char` via some constraint, for example:

```haskell
class CharType a
instance CharType Char

instance CharType a => Truthy [a] where
    truthy s = length s /= 0
```

If you wanted to define several instances for lists of different things without `FlexibleInstances`, you'd end up delegating the behaviour with regard to the items in the array to another (or the same) typeclass's instances for those individual items, for example:

```haskell
class Count a where
  countLen :: a -> Int

instance Count Char where
  countLen a = 1
instance Count Int where
  countLen i = i

instance Count a => Count [a] where
  countLen as = foldl' (\sum a -> sum + countLen a) 0 as
```

Here, we treat the items in the array polymorphically by requiriing that they also be instances of `Count`, and fold over them to sum up the results in either case. Usage:

```haskell
main> countLen [1::Int,2,3,4]
10
main> countLen "hello"
5
```

List types are still bound by the general `Count [a]` instance here. However if needbe one can delegate all of the behaviour to a separate typeclass for handling, in this example different `[]` types:

```haskell
class Count a where
    countLen :: a -> Int

-- catches all array types and delegates
-- to CountArr based on contained type:
instance CountArr a => Count [a] where
    countLen as = countLenArr as

-- handle different underlying types:
class CountArr a where
    countLenArr :: [a] -> Int

instance CountArr Char where
    countLenArr as = length as
instance CountArr Int where
    countLenArr as = sum as
```

Here, running `countLen ([1,2,3] :: [Int])` for example results in a call being made to the `countLenArr` instance for `Int`, and running `countLen "hello"` the `countLenArr` instance for `Char`, each which could have completely independant handling.



# MultiParamTypeClasses

Simply, type classes with more than one param allowed (or none!).

```haskell
-- typeclass with 2 params, a and b.
class LooseEq a b where
  looseEq :: a -> b -> Bool

-- now we can define instances for both
instance LooseEq Int Float where
   looseEq a b = a == round b
instance LooseEq Float Int where
    looseEq a b = round a == b
```

Usage:

```haskell
main> looseEq (10.2 :: Float) (10 :: Int)
True
main> looseEq (10 :: Int) (10.2 :: Float)
True
```

Notice that we have to provide types otherwise the compiler won't know which instance to make use of. Even if there is only one instance, the assumption is that more could be created, and so probably to avoid that changing existing behaviour, the compiler is pretty cautious.

Often with `MultiParamTypeClasses` you'll also end up wanting to use `FunctionalDependencies`, which can ease the burden on adding type annotation.



# FunctionalDependencies

For use in multi param type classes; functional dependencies provide a way to state that one type is dependant on others, so that the compiler can infer it without you having to explicitly tell it. Let's see why we need this:

```haskell
class Adder' a b c where
  add' :: a -> b -> c

instance Adder' Int Int Int where
  add' a b = a + b
```

This will fail to type check:

```haskell
add' (1::Int) (2::Int)
```

The problem is that at any point, someone could define an instance of `Adder'` with, say, `Int Int Double`
as its 3 params. To play it safe, the type checker requires then that you explicitly provide the final type to prevent future instances from breaking things. Thus, this is fine but pretty ugly:

```haskell
add' (1::Int) (2::Int) :: Int
```

Functional dependencies add a little syntax in the class definition to allow us to avoid this issue:

```haskell
class Adder a b c | a b -> c where
  add :: a -> b -> c

instance Adder Int Int Int where
  add a b = a + b
```

This says that the type `c` is uniquely determined from the types `a` and `b`. This means that there cannot exist an instance with the same types `a` and `b` but different type `c` (for instance, `Adder Int Int Double`). As such, the compiler is safe to infer what `c` will be based on what it knows `a` and `b` to be, and so

```haskell
add (1::Int) (2::Int)
```

now works.


# OverlappingInstances

In actuality, `OverlappingInstances` has been depracated in GHC 7.10. Instead, we gain a few pragmas that allow us to achieve the same but allow us to enable overlap on a per instance basis rather than globally. Let's look back at our `LooseEq` class from earlier to see why we might need this. Given that we've defined specific instances for `LooseEq` already, we could try to define a more general one that works on multiple numbers types. First, a supporting class and instances:

```haskell
-- a quick class to convert numbers to integers.
class Num a => ToInteger a where
  numToInteger :: a -> Integer

instance ToInteger Int where
    numToInteger i = toInteger i
instance ToInteger Double where
    numToInteger d = round d
instance ToInteger Float where
    numToInteger f = round f
```

Making use of this, we declare a more general instance of `LooseEq` as follows:

```haskell
instance {-# OVERLAPPABLE #-} (ToInteger a, ToInteger b) => LooseEq a b where
    looseEq a b = numToInteger a == numToInteger b

-- These overlap the more general case so don't need notation unless they too
-- will be overlapped eg by "LooseEq Int Bool":
instance (ToInteger a) => LooseEq a Bool where
    looseEq a b = (numToInteger a /= 0) == b

instance (ToInteger a) => LooseEq Bool a where
    looseEq b a = (numToInteger a /= 0) == b

-- this requires FlexibleInstances but doesnt overlap with anything else:
instance (LooseEq a b, Foldable f) => LooseEq (f a) (f b) where
    looseEq fa fb = foldl' cmp True (zip (toList fa) (toList fb))
      where toList = foldr (:) []
            cmp False _ = False
            cmp _ (a,b) = a `looseEq` b

-- this is more specific than above and so I use OVERLAPPING to say it's OK:
instance {-# OVERLAPPING #-} (LooseEq a b) => LooseEq [a] [b] where
    looseEq as bs = length as == length bs
```

Notice the language pragmas `OVERLAPPING` and `OVERLAPPABLE`. Without these, we'd get an overlap error when we tried to make use of these instances, as they overlap with others. In other words, for some use of `looseEq` there might be more than one instance that matches (ranging from a more general one to a more specific one).

The three pragmas of interest are:

- `OVERLAPPABLE`, which when applied to a more general instance means that more specific instances (so, those declared earlier) will be fine.
- `OVERLAPPING`, which when applied to a more specific instance means that it is allowed to be overlapping a more general one.
- `OVERLAPS`, which is equal to adding both of the above.

Use of these pragmas is **potentially dangerous** because you could be relying on some instance in your code, and then later overlap it with a more specific instance that still matches your usage of it but potentially behaves completely differently. It would seem to be less risky to use the `OVERLAPPING` pragma than the `OVERLAPPABLE` one, since the former prevents others from creating an overlapping instance without acknowledging it as such, but the latter would not kick up a fuss about it and so potentially alter behaviour silently.



# ExistentialQuantification

Allows you to remove types variables on the left of a `data` declaration. This is made useful by the fact that you can then constrain types variables on the right hand side to belonging to certain classes (otherwise, you couldnt interact with them at all). Here's the canonical example (where the `Typeable` constraint is used only for casting later and is not necessary here):

```haskell
-- we want to store anything "showable" in some type (Typeable not required yet):
-- notice the lack of 'a' on the left, and instead a 'forall a' on the right to bring
-- 'a' into scope, and then a class constraint on it so that we always know we can
-- use 'show':
data Showable = forall a. (Show a, Typeable a) => Showable a

-- define show instance to just apply show to the contained type,
-- since we know that's always allowed:
instance Show Showable where
    show (Showable a) = show a

-- now, we can put any arbitrary type into an array and show them all:
showables = [Showable (2 :: Int), Showable (3 :: Float), Showable "hello", Showable 'c']
-- show showables == [2,3.0,"hello",'c']
```

A neat trick here is that, if we have constrained the contained type to be `Typeable` (as we did above), we can extract the original type back out of the `Showable` container using `Data.Typeable`'s `cast` method:

```haskell
castShowable (Showable a) = cast a
```

Usage:

```haskell
main> castShowable (Showable "hi") :: Maybe String
Just "hi"
main> castShowable (Showable "hi") :: Maybe Int
Nothing
```

We also need `ExistentialQuantification` if we want to add class constraints to a data type, even if we don't want to hide the variable from the left hand side, for example:

```
data ShowableT a = Show a => ShowableT a
```



# GADTs

Short for _Generalized Algebraic Data Types_, these allows you to declare new `data` types using a function signature style. The advantage of this is that it allows you to decide exactly what the final type will be in each case, which is not possible with the current syntax.

A simple illustration of the difference:

```haskell
-- take a simple type constructor:
data MyData a = Something a | Otherthing
```

Here, `Otherthing` is of type `MyData a`; There is no way to change that `a` to something more specific. However, with `GADT` syntax, we can. This is exactly equivalent to the above, but now we can specify what `a` is if we like:

```haskell
data MyData' a where
    Something' :: a -> MyData' a
    Otherthing' :: MyData' a
```

Why would we want to do this? One case is for when we want to use those parameters to encode extra information for us. Let's look at a larger example:

```haskell
data Expr'' = S'' String
            | I'' Int
            | Add'' Expr'' Expr''
            | Append'' Expr'' Expr''
```

A basic AST representation, but notice that `Add` can take any `Expr` even though the `Expr` produced from `I Int` is the only one that makes sense. We can use a type on `Expr` to encode this, so that `Add` only takes `Expr Int`s, and `Append` only takes `Exp String`s:

```haskell
data Expr' a = S' String
             | I' Int
             | Add' (Expr' Int) (Expr' Int)
             | Append' (Expr' String) (Expr' String)
```

But without `GADTs` we have no way to enforce that the type of, say, `I Int` will actually be `Expr Int`. With GADT's we express constructors as functions, and so we provide that final type:

```haskell
data Expr a where
    I      :: Int         ->                Expr Int
    Add    :: Expr Int    -> Expr Int    -> Expr Int
    S      :: String      ->                Expr String
    Append :: Expr String -> Expr String -> Expr String
```

Now, we can whip up a quick evaluator for our tree:

```haskell
eval :: Expr a -> a
eval (S s) = s
eval (I i) = i
eval (Add a b) = eval a + eval b
eval (Append a b) = eval a ++ eval b
```

And notice that silly expressions are sanely rejected by the type system for us:

```haskell
main> eval $ (I 1 `Add` I 2) `Add` (I 3)
6
main> eval $ (S "Hello" `Append` S " ") `Append` (S "World")
"Hello World"
main> eval $ (S "Hello" `Append` S " ") `Add` (S "World")
BZZZZT! Type Error. Add expects Expr Int, got Expr String's!
```



# ImplicitParams

Allow functions to require that some value exists in the scope it's used in, without one having to actually explicitly pass in said value. Allows one to define global things in `main` for instance and not have to think about threading them through everywhere explicitly, just adding an implicit requirement in functions where they are actually used. A simple example:

```haskell
-- define implicit param as variable with question mark in front of:
fn1 a = fn2 a where ?world = " world"
fn2 a = fn3 a
fn3 a = fn4 a

-- add constraint that implicit param called ?world exists in the scope this
-- it used. Now, calling fn1 will work, but calling fn2, fn3 and fn4 directly
-- will fail as the implicit param would not be defined if not for fn1.
fn4 :: (?world :: String) => a -> String
fn4 a = a ++ ?world
```

Now, to test:

```haskell
main> fn1 "hello"
"hello world"
main> fn2 2
BZZZT! ERROR! implicit param ?showIt not bound!
```

The downside? If you want to annotate your types, you have to explicitly thread your implicit parameters through in the type signatures, so why not just pass them normally? Calling something like `tyfn1 "hello"` in this example would not work for instance:

```haskell
tyfn1 :: String -> String
tyfn1 a = tyfn2 a where ?showIt = "world"

--this needs to mention implicit param type
--in order to work, so why not just pass it?
tyfn2 :: String -> String
tyfn2 a = tyfn3 a

tyfn3 :: (?showIt :: String) => String -> String
tyfn3 a = a ++ ?showIt
```

All in all, I think I'll steer clear of this one.



# TypeFamilies

One of the most exciting language extensions I'm aware of so far, this one lets you teach Haskell's type system new tricks by defining relationships between types. This language extension actually introduces type families as well as data families, each with syntax do that you can use them inside classes. Let's start with type families themselves:

## Type Families

This is a simple beginning example of an *open* type family. Think of type families as type aliases on steroids. This one maps the alias `AddResult a b` to some corresponding type. It's called an *open* type family because we can add new instances to it later on if we want to extend it.

```haskell
type family AddResult a b
type instance AddResult Double Double = Double
type instance AddResult Int Double = Double
type instance AddResult Double Int = Double
type instance AddResult Int Int = Int
```

If we were to use this, we'd probably end up wanting to define a type class, so let's do that:

```haskell
class MyAdder a b where
    adder :: a -> b -> AddResult a b

instance MyAdder Double Double where
    adder a b = a + b

instance MyAdder Int Double where
    adder a b = realToFrac a + b

instance MyAdder Double Int where
    adder a b = a + realToFrac b

instance MyAdder Int Int where
    adder a b = a + b
```

Here, we set the return type of this multi-parameter type class to be `AddResult a b`, so that depending on what `a` and `b` are in the specific instances we can output completely different things. In this case we stick to `Int`s and `Double`s.

Naturally, we might feel that this type is associated to the type class, and want to actually state that part of the `MyAdder a b` type class is the type `AddResult a b`. Well, we can do exactly that:

```haskell
class MyAdder' a b where
    type AddResult' a b
    adder' :: a -> b -> AddResult' a b

instance MyAdder' Double Double where
    type AddResult' Double Double = Double
    adder' a b = a + b

instance MyAdder' Int Double where
    type AddResult' Int Double = Double
    adder' a b = realToFrac a + b

instance MyAdder' Double Int where
    type AddResult' Double Int = Double
    adder' a b = a + realToFrac b

instance MyAdder' Int Int where
    type AddResult' Int Int = Int
    adder' a b = a + b
```

Now, `AddResult a b` is an associated type within the `MyAdder` type class, and so each instance will require a definition of it just as for any required functions. Functionally though it is the same as the original `MyAdder` example.

Sometimes we might not want others to be able to arbitrarily extend a type family. Fortunately, GHC 7.8 introduced closed type families. Defining `AddResult` again as a closed type family would look like:

```haskell
type family AddResult'' a b where
    AddResult'' Double Double = Double
    AddResult'' Int Double = Double
    AddResult'' Double Int = Double
    AddResult'' Int Int = Int
```

One advantage of closed type families is that the evaluation order is now obvious; it works from top to bottom until your type is matched. This means that more general type aliases can come below more specific ones, which isn't possible in an open type family.

Type families can be recursive, too. This one takes some arbitrary function signature, and gives back its return type by recursing through it:

```haskell
type family ReturnVal ty where
    ReturnVal (a -> b) = ReturnVal b
    ReturnVal b = b
```

Now, this would type check ('a' is a `Char`):

```haskell
'a' :: ReturnVal (Int -> String -> Char)
```

But this would not ('a' is not an `Int`):

```haskell
'a' :: ReturnVal (Char -> String -> Int)
```

In my next example I turn a nested 2-tuple into a function signature using another recursive type family. You may find you need to enable the extension `UndecidableInstances` with certain recursive definitions when the type checker can't be sure that the recursion will ever end; the compiler will tell you if it is necessary and give this reason.

```haskell
type family TupleFn ty out where
    TupleFn () output = output
    TupleFn (a,b) output = a -> (TupleFn b output)
```

We could use this definition to actually construct a function that takes a tuple, and a function generated from the tuple's type using the above (of whatever parity is required!), and applies the function to each value in the tuple, returning the result. We'll need another type class for this as we'll do something different depending on the shape (and thus type) of the tuple:

```haskell
class ApplyFnToTuple a where
    applyFnToTuple :: a -> TupleFn a out -> out

instance ApplyFnToTuple b => ApplyFnToTuple (a,b) where
    applyFnToTuple (a,b) fn = applyFnToTuple b (fn a)

instance ApplyFnToTuple () where
    applyFnToTuple _ fn = fn
```

The function signature asks for a tuple of type `a`, and then some function of type `TupleFn a out`, which is a type dictated by the shape of `a`, and returns some `out`, which is whatever the tuple function gives back. These would now both be true:

```haskell
applyFnToTuple ('a',('b',())) $ \a b -> [a,b] == "ab"
applyFnToTuple ("hello",(12,('r',()))) $ \h n r -> h ++ show n ++ [r] == "hello12r"
```

We don't always need type classes to take advantage of type families in this way. If I create my own version of the above, I can avoid using them entirely:

```haskell
-- we use these types to "tag" our list values. essentially,
-- they form a type level description that mirrors what we are
-- doing with values. We can improve on this with DataKinds, later.
data Cons a b
data Nil

-- the list itself, where the type 'a' is built from the above tags
data MyList a where
    LCons :: itemty -> MyList a -> MyList (Cons itemty a)
    LNil  :: MyList Nil

-- this type family converts that type 'a' to a function signature.
type family MyListFn a output where
    MyListFn (Cons a b) output = a -> (MyListFn b output)
    MyListFn Nil output = output

-- this function applies items in `MyList a` to a `MyListFn a` just
-- like we did with tuples. Note no type family, because
-- no type dependant differences in behaviour needed:
applyFnToMyList :: MyList a -> MyListFn a out -> out
applyFnToMyList (LCons a b) fn = applyFnToMyList b (fn a)
applyFnToMyList LNil fn = fn
```

The crucial steps here are:

1. We create a basic list type to use, here of type `MyList a`.
2. We want to record the type of each item we put into the list in the type `a` in some way that is easy to work with. Here we use a couple of new types, `Cons a b` and `Nil` to represent at the type level what my list's `LCons` and `LNil` constructors are doing at the value level.
3. Now that we have recorded the types in `a`, we use a type family to take the type `a` and convert it into some desired type `b`, here a function signature.
4. Finally, we use this in `applyFnToMyList`, which takes in first a `MyList a` (remember, that `a` is the record of all types within), second a function of type `MyListFn a out`, and finally returns `out`. The definition of this function basically applies the function to each item in the list in the same order as the signature dictates.

We can use it just like the earlier tuple version:

```haskell
applyFnToMyList (LCons 'a' (LCons 'b' LNil)) $ \a b -> [a,b] == "ab"
applyFnToMyList (LCons "hello" (LCons 12 (LCons 'r' LNil))) $ \h n r -> h ++ show n ++ [r] == "hello12r"
```

Awesome.


## The Equality Constraint

Before looking at data families, another goodie that comes with the TypeFamily extension (and also with `GADTs`) is the type equality constraint `~`. This basically allows us to tell the compiler that some type `a` is equal to some other type `b`. As an example, our earlier example avoiding `FlexibleInstances` could be redefined from the original, which was:

```haskell
class Truthy a where
    truthy :: a -> Bool

class CharType a
instance CharType Char

instance CharType a => Truthy [a] where
    truthy s = length s /= 0
```

To (now using an equality constraint):

```haskell
class Truthy' a where
    truthy' :: a -> Bool

instance a ~ Char => Truthy' [a] where
    truthy' s = length s /= 0
```

Here, we define an instance for the general case of `Truthy [a]`, but then add the constraint that `a` must be a `Char`. I expect this comes in particularly handy when you wish to define a function that operates on some type family but only when certain types match up (thus enabling the use of functions specific to those types). At times, it is useful just to provide the compiler with a little extra information if it is struggling to deduce some types.


## Data Families

Also enabled with the TypeFamilies extension, these are the `data` equivalent to type families as described above. Unlike type families where multiple type aliases can map to the same thing, each data instance is expected to have unique constructors; just like in any other data declaration we cant have multiple constructors with the same name.

An example I've seen before which I liked was that of storing different types in specialised key/value stores. Here, we define a new type `MyMap key val` and then a constructor for each of `MyMap Bool Val` and `MyMap Int Val`. This shares the property of `GADTs` in that we can set the type of `key` and `val` in `MyMap key val` to whatever we like for each constructor:

```haskell
data family MyMap key val
data instance MyMap Bool val = BoolMap (Maybe val) (Maybe val)
    deriving Show
data instance MyMap Int val = IntMap [(Int,val)]
    deriving Show
```

The equivalent in GADT form would be:

```
data MyMap key val where
    BoolMap :: Maybe val -> Maybe val -> MyMap Bool val
    IntMap :: [(Int,val)] -> MyMap Int val
```

Though you are unable to automatically derive `Show` in the GADT version unlike for the data family declarations. The main advantage of data families however is that they are *open* and can be extended later on, for example if you create a new `key` type you want a specialised `MyMap` instance for.

We could create a type class and a couple of instances to put our `MyMap` to use:

```haskell
class MapKey k where
    insert :: MyMap k v -> k -> v -> MyMap k v
    find :: MyMap k v -> k -> Maybe v
    remove :: MyMap k v -> k -> MyMap k v

instance MapKey Bool where
    insert (BoolMap _ f) True v = BoolMap (Just v) f
    insert (BoolMap t _) False v = BoolMap t (Just v)
    find (BoolMap t _) True = t
    find (BoolMap _ f) False = f
    remove (BoolMap _ f) True = BoolMap Nothing f
    remove (BoolMap t _) False = BoolMap t Nothing

instance MapKey Int where
    insert (IntMap list) k v = IntMap ((k,v):L.filter (\(k1,v1) -> k /= k1) list)
    find (IntMap list) k = case L.find (\(k1,v1) -> k == k1) list of
        Nothing -> Nothing
        Just (k,v) -> Just v
    remove (IntMap list) k = IntMap (L.filter (\(k1,v1) -> k /= k1) list)
```

For our two map types, we define some basic operations to add, find and remove entries. If we add a new instance to our `MyMap` data family at a later date for some new type, we can add a new instance to the associated type class for that same type and off we go. Just like type families, we can keep the data type that is clearly tied to this class inside it as an associated data type:

```haskell
class MapKey' k where
    data MyMap' k v
    insert' :: MyMap k v -> k -> v -> MyMap k v
    find' :: MyMap k v -> k -> Maybe v
    remove' :: MyMap k v -> k -> MyMap k v

instance MapKey' Bool where
    data MyMap' Bool val = BoolMap' (Maybe val) (Maybe val)
    insert' (BoolMap _ f) True v = BoolMap (Just v) f
    insert' (BoolMap t _) False v = BoolMap t (Just v)
    find' (BoolMap t _) True = t
    find' (BoolMap _ f) False = f
    remove' (BoolMap _ f) True = BoolMap Nothing f
    remove' (BoolMap t _) False = BoolMap t Nothing

-- ...other instances...
```

This works exactly as before, except now the requirement that you need to define a new data instance is imposed on adding new type class instances.


# KindSignatures

This extension allows you to provide *kind* signatures to types, just as we would add type signatures to values. Basically in Haskell every type (I believe there are exceptions) has the *kind* `*`. Just as types are categories of values (`Int` is the category containing 0, 1, 2, 3.., `Char` is the category containing 'a', 'b', 'c' and so on), kinds are categories of types (so the kind `*` is the category containing type types `Int`, `Char`, `Bool` and so on). A function type like `Int -> Char -> Int` would have the kind `* -> * -> *`. You could imagine categorising kinds into something too, and so on; some languages let you do this but Haskell currently stops at kinds (every kind is in the same category, and you can't change that).

It makes sense to use kind signatures when defining types and type families, as the variables you'd use instead are somewhat meaningless, for example in the data type:

```haskell
data MyMap key val
```

What do `key` and `val` mean? Nothing, really. They are just place holders implying that the type `MyMap` is actually of kind `* -> * -> *`; it takes in two values (which can each be any normal Haskell type) and returns a normal Haskell type, itself. thus, we could write it instead as:

```haskell
data MyMap :: * -> * -> *
```

Being explicit about the *kind* of `MyMap` without using random variable names.

This becomes particularly useful with DataKinds and PolyKinds.


# DataKinds

With this extension, we can actually create new kinds of our own that are separate from the existing kind `*`. This is done by way of automatically promoting any suitable data type declarations one level.

Any basic types (not GADT's, and not using already promoted types for instance) are *promoted* to kinds, and their constructors promoted to types. Thus, if we write the simple data declaration:

```haskell
data Number = One | Two
```

We get, in addition to our type `Number` with constructors `One` and `Two`, the **kind** `Number` and the **types** `One` and `Two`. But wait, wouldn't that potentially be ambiguous. Consider I also write:

```haskell
data One
data Two
```

Now when I use the *type* `One` somewhere, so I mean the latter type or the promoted type `One` that belongs to our new `Number` kind? The answer is the former. To resolve the ambiguity we prefix a promoted type with a single quote if we want instead to use that, so `'One` would be our promoted type.

How can we use this ability to create new kinds? Consider this definition:

```haskell
data SomeData :: Number -> * where
    OneS :: SomeData 'One
    TwoS :: SomeData 'Two
```

Here, we say that our type `SomeData` is of kind `Number -> *` (using KindSignatures). This means that the first type it takes in must now be of kind `Number`, so either `'One` or `'Two` (single quotes to disambiguate). Thus, the following would be an invalid definition:

```haskell
a :: SomeData Int
a = undefined
```

Since `Int` is of kind `*`, rather than our new kind `Number`. Remember our `MyList` example from earlier, where we built up a type corresponding to the types of values we input. Let's rewrite that taking advantage of DataKinds:

```haskell
-- use this as the new *kind* ListK with types ConsTy(..) and NilTy:
data ListK a = ConsTy a (ListK a) | NilTy

-- our MyList must use the above kind as its first argument:
data MyList' :: ListK * -> * where
    LCons' :: itemty -> MyList' a -> MyList' (ConsTy itemty a)
    LNil'  :: MyList' NilTy
```

This all works exactly as it did before, but by doing this we've locked `MyList` down so that the compiler will throw a hissy if we try using any invalid types like `MyMap Int` or `MyMap Bool`, whereas before the compiler would have let those types slip (and gone on to complain about the values of those nonsense types instead!)


# TypeOperators

Type operators let us declare operators at the type level just like we would at the value level. For example, rather than our `ListK`s `ConsTy` type , we could use an operator. Here's a tweaked version of `ListK` which does that:

```haskell
data ListK2 a = a :+: ListK2 a | NilTy2
```

Now, we can use this new type operator in place of `ConsTy`:

```haskell
data MyList2 :: ListK2 * -> * where
    LCons2 :: itemty -> MyList2 a -> MyList2 (itemty :+: a)
    LNil2  :: MyList2 NilTy2
```

And our types look a little nicer as well:

```haskell
LCons2 'a' (LCons2 'b' LNil2) :: MyList2 (Char ':+: (Char ':+: 'NilTy2))
```

The DataKinds extension also promoted Haskell's own list type to a kind, and so its operators and syntax become available at the type level (though we need this extension to use the now type-level list operator). Here's yet another implementation of `MyList`, this time using Haskell's promoted list kind:

```haskell
data MyList_ :: [*] -> * where
    LCons_ :: itemty -> MyList_ a -> MyList_ (itemty ': a)
    LNil_  :: MyList_ '[]
```

And our types get yet nicer:

```haskell
LCons_ 'a' (LCons_ 12 LNil_) :: MyList2 '[Char,Int]
```

Sweet.


# PolyKinds

Now we can introduce new kinds, we'll quickly find that we are restricted to working with one kind at a time, unlike types where we can define polymorphic functions to work across different types.

PolyKinds gives us this polymorphism at the kind level. Rather than explicitly naming the kind we'll be using (eg `*` or `Number`), we can use a variable eg `k` just as we would for types in polymorphic functions.

Let's see why we'd want it with a type family example that reverses a promoted list:

```haskell
type Reverse (a :: [*]) = DoReverse a '[]

--this type takes a type level array eg [Int,Char,String] and reverses
--it to eg [String,Char,Int].
type family DoReverse (a :: [*]) (b :: [*]) :: [*] where
    DoReverse (a ': as) out = DoReverse as (a ': out)
    DoReverse '[] out = out
```

Thanks to this type family, these two types would now be equivalent:

```haskell
Reverse [Int,Char,String] ~ [String,Char,Int]
```

As we've done before when playing with Typefamilies, we could convert this list type into something more useful, for example a nested tuple:

```haskell
type family TupFromList (a :: [*]) where
    TupFromList (a ': as) = (a, TupFromList as)
    TupFromList '[] = ()
```

So these would be now be perfectly valid ways to define tuples:

```haskell
(12,('a',("hello",()))) :: TupFromList [Int,Char,String]

-- note that we can next type family applications just like functions:
("hello",('a',(12,()))) :: TupFromList (Reverse [Int,Char,String])
```

But I digress. What's PolyKinds got to do with all this? Well, this would fail to compile:

```haskell
data LetterK = A | B

a :: Reverse [A,B]
a = undefined
```

Because we are trying to reverse an array of types of kind `[LetterK]`, but our `Reverse` type wants arrays of kind `[*]`. Just like functions that take `Int`s can't be passed `Char`s, types expecting to be of the kind `*` can't actually be of the kind `LetterK`. With PolyKinds, we'd tell `DoReverse` that it was of the kind `[k]`, to denote that we don't care what concrete kind `k` is, and any will do.

```haskell
type Reverse_ (a :: [k]) = DoReverse_ a '[]

type family DoReverse_ (a :: [k]) (b :: [k]) :: [k] where
    DoReverse_ (a ': as) out = DoReverse_ as (a ': out)
    DoReverse_ '[] out = out
```

# ConstraintKinds

This extension gives constraints, the things that ordinarily appear only to the left of `=>`, their own _kind_, `Constraint`. This allows them to be talked about like other kinds, expanding how we can make use of them somewhat. Let's have a quick look at a little of what we can do with this newfound power.

For a start, we can talk about them in type synonyms:

```haskell
type ReadShow a = (Show a, Read a)

-- we can use these as we would the orignals:
readAndShow :: ReadShow a => a -> a
readAndShow val = read $ show val
```

This extends to associated types, which means we can (with `Typefamilies`) use them in type classes to allow instances to alter the constraints used based on the input type. I found a use for this when I wanted to create a type class recently that did the following transformations:

```text
Bool       ==>  a -> Bool
a -> Bool  ==>  a -> Bool
```

The typeclass to do this and its instances ended up looking like the following:

```haskell
-- at this point, I dont know what the input to the function
-- will be, but I can make a constraint that in some way
-- connects it with the instance type:
class WithFunc ty where
    type WithFuncC ty input :: Constraint
    withFunc :: WithFuncC ty input => ty -> (input -> Bool)

-- given some function, I now know the input type,
-- and can thus connect it to the input type of the
-- returned function using the equality constraint (~):
instance WithFunc (a -> Bool) where
    type WithFuncC (a -> Bool) input = a ~ input
    withFunc = id

-- I don't care what type the input is, since
-- I know what i'll be outputting. () as a constraint
-- means that any type will be fine!
instance WithFunc Bool where
    type WithFuncC Bool input = ()
    withFunc out = \_ -> out
```

The `Constraint` type is used here to restrict the `input` type mentioned in the class definition in some way, depending on the instance types.

- The `(a -> Bool)` instance is able to say that the input to this provided function `a` will equal the input type of the returned function `(input -> Bool)` mentioned in the class (since we'll just return it unchanged).
- For the `Bool` instance, I can say that I don't care what the input type will be by effectively providing no constraint (since I have the output to the function, the boolean, already!).

Using this class, all of these are now perfectly valid:

```haskell
withFunc True     1   == True
withFunc False    'd' == False
withFunc (== 'a') 'b' == False
withFunc (== 'a') 'a' == True
withFunc (< 7)    5   == True
```

Which is awesome.

Generally, it seems that `ConstraintKinds` will play a useful role alongside `TypeFamilies` for allowing the instances of some typeclass to dictate more of the details of their parent type classes definitions. Given some type mentioned in a class definition, `TypeFamilies` allows it to be transformed depending on the type of each instance, whereas `ConstraintKinds` can impose different per instance restrictions on it.

# The End (for now)

That's all for now! I'll update this post as and when I feel the desire to summarize more language extensions, but for now thanks for stopping by! Let me know if you have any comments or suggestions below!

[example-code]: examples.hs
