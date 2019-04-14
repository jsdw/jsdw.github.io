+++
title = "Haskell: A Look at the Continuation Monad"
description = "Some notes I developed on the continuation monad in Haskell as a result of exploring how they work and how to use them, complete with plenty of examples along the way."
date = 2015-06-07
+++

Some notes on Continuations in Haskell. What are they, how are they used and what are they good for? All of the following code can be found [here][example-code] ready to be plugged into GHCI and played with.

# Introduction to Continuations

Continuations work by providing their result to a callback that's passed in, rather than directly. Starting simple, here is how values can be presented in the form of continuations.

```haskell
twoC = \out -> out 2
helloC = \out -> out "hello"
```

These take the form `(a -> r) -> r`, where `a` is the output from the continuation and `r` the final result after the callback is applied. `id` is a function that simply takes a value and returns it (`\a -> a`), and so I'll use it to pull values out of continuations quite a lot below.

We can use it to extract values from the above continuations as follows:

```haskell
twoC id == 2
helloC id == "hello"
```

We could simplify creating these values with a simple function that takes a value and gives back a continuation just like one of the above:

```
ret val = \out -> out val
```

So now we can create continuations like so:

```
anotherTwoC = ret 2
anotherHelloC = ret "hello"
```

# Chaining Continuations

Once we can create continuations, the next thing to do that would be useful is joining them together so that we can work with them. We'll do as Monads would do and create a function called `bind` that takes a continuation and a function which is provided the value of it and returns a new continuation as a result:

```haskell
inC `bind` fn = \out -> inC (\inCval -> (fn inCval) out)
```

Since continuations work by passing their result into the provided function, to make use of the value given back from the first continuation we pass it a function that will be provided this value (`inC (\inCval -> ...)`). We then pass the value to our `fn`, which is expected to return a continuation itself. Finally, we wrap this in a continuation whose callback we pass to the new continuation created from `fn inCval`.

Things will become more clear when we see this `bind` function in action:

```haskell
-- a simple example that doubles the value handed back from twoC:
fourC = twoC `bind` \two -> ret (two*2)

-- a more complex example combining the results of two continuations:
twoHelloC = twoC `bind` \two ->
              helloC `bind` \hello ->
                ret $ (show two)++hello
```

The result of running these new continuations is:

```haskell
fourC id == 4
twoHelloC id == "2hello"
```

The interesting thing is that when chaining continuations with `bind`, we alter the effect of the callback on the result of each previous continuations value. What happens then if you pass a function that doesn't use its callback?

```haskell
badC = \out -> "boom!"

twoBadC = twoC `bind` \two ->
            badC `bind` \hello ->
              ret $ (show two)++hello
```

Since we never use the callback, all subsequent continuations (that each effect only the callback function provided to those before them), including the final function we apply when we want to get the value out (above that's been "id") are simply ignored, and thus we end up with:

```haskell
twoBadC id == "boom!"
```

# Mapping a Function over some Continuation

The same approach we took to chain continuations with our `bind` function can be used to write a map function, that runs some function on the eventual result of some continuation. Once again, it does this by wrapping the callback provided to the input continuation to alter the value before returning it to `out`:

```haskell
mapC fn inC = \out -> inC (\inCval -> out (fn inCval))

-- Note that we are applying inCval to fn, and then out. Thus,
-- this can be simplified using point free notation to:
mapC' fn inC = \out -> inC (out . fn)
```

To test it works, make sure it has the right result with continuations that use their callback **and** those that don't. Thus, these should hold true:

```haskell
mapC (*2) (\out -> out 5) id == 10
mapC (*2) (\out -> 5) id == 5
```

A continuation that doesn't use its callback is essentially ignoring all subsequent functions and exiting early. A naive approach to mapping might have been to run the input continuation with id to get its result and then work on that:

```haskell
badMapC fn inC = \out -> let val = fn (inC id)
                         in out val
```

While this seems to work at first, this would apply map regardless of whether the continuation uses its callback or not, and so would result in:

```haskell
badMapC (*2) (\out -> out 5) id == 10
badMapC (*2) (\out -> 5) id == 10 -- wrong, should be 5
```

# The Cont Monad

This continuation framework already exists in the form of the Cont Monad. The entire basic definition of the Cont monad (with applicative and map lark to satisfy the Monad typeclass) can be seen below. In effect, we take exactly what we have devised above and wrap it into a new type so that we can create typeclass instances for it.

Haskell itself defines this in terms of monad transformers. I have removed that aspect here for simplicity:

```haskell
-- wrap continuation functions into Cont type:
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
    -- same as our mapC' but unwrapping/wrapping Cont:
    fmap fn (Cont inC) = Cont $ \out -> inC (out . fn)

instance Applicative (Cont r) where
    -- same as ret but wrapping in Cont
    pure val = Cont $ \out -> out val
    -- similar to fmap excapt we need to get fn out of a Cont as well.
    (Cont fnC) <*> (Cont inC) = Cont $ \out -> fnC $ \fn -> inC (out . fn)

instance Monad (Cont r) where
    -- same as ret but wrapping in Cont
    return = pure
    -- same as `bind` but wrapping/unwrapping Cont:
    (Cont inC) >>= fn = Cont $ \out -> inC (\a -> (runCont (fn a)) out)

--
-- We get these functions from the Cont definition as well, the
-- latter being a simple alias in our non-transformer version
-- (in the transformer version it hides away the identity monad).
-- We'll see callCC in action later.
--
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC fn = Cont $ \out -> runCont (fn (\a -> Cont $ \_ -> out a)) out

cont :: ((a -> r) -> r) -> Cont r a
cont = Cont
```

The `do` notation that the `Monad` typeclass provides gives us a nicer way to write our earlier examples:

```haskell
twoC' = return 2
helloC' = return "hello"

twoHelloC' = do
    two <- twoC'
    hello <- helloC'
    return $ (show two)++hello

-- remember this desugars to the following, which should look very similar
-- to our first non-wrapped attempt:
twoHelloC'' = twoC' >>= \two ->
                helloC' >>= \hello ->
                  return $ (show two)++hello
```

Other than getting to use nicer syntax, the only other change is that, since our continuations are wrapped up in this `Cont` type, we need to unwrap them to pull values out now:

```haskell
(runCont twoHelloC') id == "2hello"
```

# Playing with Multiple Callback Invocations

As well as escaping early, we can also call the callback more than once. Remember, the callback has (as a result of how `bind` works) wrapped into it all of our surrounding computations, including that from the final function we use to pull out a value. Lets see:

```
twoMultiC = do
    two <- twoC'
    hello <- cont $ \out -> out "hello" ++ out "hello"
    return $ (show two)++hello
```

Here, we call our continuation callback twice and join the results. Each invocation of `out "hello"` runs the rest of the computation and returns the end result, both of which are concatenated. Thus, the following holds true:

```
runCont twoMultiC id == "2hello2hello"
runCont twoMultiC (++"BOOM!") == "2helloBOOM!2helloBOOM!"
```

Repetition anywhere has the same effect, the result of running these being identical to above:

```
twoMultiC' = do
    two <- cont $ \out -> out 2 ++ out 2
    hello <- helloC'
    return $ (show two)++hello

twoMultiC'' = do
    two <- twoC'
    hello <- helloC'
    cont $ \out -> out ((show two)++hello) ++ out ((show two)++hello)
```

In either case, running `out` with some value leads to every subsequent computation also being run on that value, which will amount to the same result regardless of where you call it. It's a bit mind bending but the more you play with it, the more it starts to make sense! It might help to look at a de-sugared (and in-lined) version of the above:

```haskell
desugaredTwoMultiC' =
    (cont $ \out1 -> out1 2 ++ out1 2) >>= \two ->
        (cont $ \out2 -> out2 "hello") >>= \hello ->
            (cont $ \out3 -> out3 $ (show two)++hello)
```

Each time `out1` is passed a result, the continuation that is provided the callback `out2` is called, and each time `out2` is passed a result, the continuation provided out3 is called. the result of `out1 2` then is `out2 "hello"`, which itself is `out3 $ (show 2)++"hello`. `out3` is whatever we pass to the continuation when we use `runCont`, in most examples above `id`. We can see that it too is then run every time a callback is called.

Using this branching feature continuations provide us we can generate combinations of values, for instance:

```haskell
multiMultiC = do
    n <- cont $ \out -> out "1" ++ out "2"
    l <- cont $ \out -> out "a" ++ out "b"
    x <- cont $ \out -> out "X" ++ out "Y"
    return $ n++l++x++" "
```

For which the below is true:

```haskell
runCont multiMultiC id == "1aX 1aY 1bX 1bY 2aX 2aY 2bX 2bY "
```

## Exiting Early from Branches

Exiting early at different points would limit the amount of branching that takes place. Crucially though, we could not guarantee that we would only exit early once, as other branches would still run. Thus, the following would be true:

```haskell
boom1C = do
    n <- cont $ \out -> "boom! "
    l <- cont $ \out -> out "a" ++ out "b"
    x <- cont $ \out -> out "X" ++ out "Y"
    return $ n++l++x++" "

-- here, our first line never calls out, so we just return the string:
-- runCont boom1C id == "boom! "

boom2C = do
    n <- cont $ \out -> out "1" ++ out "2"
    l <- cont $ \out -> "boom! "
    x <- cont $ \out -> out "X" ++ out "Y"
    return $ n++l++x++" "

-- here, our we call out twice, each one hitting the second continuation and
-- exiting with boom, so we get two of them appended because out "1" and out "2" are:
-- runCont boom2C id == "boom! boom! "

boom3C = do
    n <- cont $ \out -> out "1" ++ out "2"
    l <- cont $ \out -> out "a" ++ out "b"
    x <- cont $ \out -> "boom! "
    return $ n++l++x++" "

-- each contnuation calls its callback twice, so we end up hitting boom 4 times:
-- runCont boom3C id == "boom! boom! boom! boom! "
```

# What about `callCC`?

`callCC` takes a function as an argument, and expects it to return a continuation. It passes the function an exit callback which, when called, returns a continuation that ignores its own callback and just returns a value to the outer continuation, breaking the inner continuation chain.

Essentially, callCC gives you a named escape hatch that you can use at any point to break free of the current continuation chain and immediately return a value.

These all return True, and never hit undefined (which would result in a runtime error):

```haskell
callCCex1 = do
    val <- callCC $ \exit -> do

        exit True
        undefined

    return val

-- by unraveling callCC (flip for aesthetics) you can see it could be written as:
callCCex2 = do

    val <- cont $ \out -> flip runCont out $ do

        cont $ \_ -> out True
        undefined

    return val

-- or in this simple case just by exiting the inner continuation early as we have
-- done in previous examples
callCCex3 = do

    val <- do

        cont $ \_ -> True
        undefined

    return val
```

By giving you an explicit callback to use to exit the continuation, `callCC` has the added advantage that you can nest uses of callCC and exit from any depth to any other with ease:

```haskell
callCCex4 = do
    val <- callCC $ \exit -> do

        innerval <- callCC $ \innerExit -> do

            exit True
            undefined

        undefined

    return val
```

Note that other structures can emulate breaking free of the control flow but none escape immediately as this does. For instance, the `Maybe` Monad ignores everything once it receives a `Nothing` value, but still runs though everything else in the chain (though that could well be optimised away in some cases).

# What else can we do with Continuations?

Here are some samples of what one can create with continuations. You are encouraged to have a play with them yourself however and see what else you can create.

## 1. A for loop that can be broken out of

Many imperative languages have for loops that can be broken out of early if desired. Continuations are one way to do the same in Haskell:

```haskell
import qualified Control.Monad.Trans.Cont  as C
import           Control.Monad.Trans.Class (lift)

forLoop :: Monad m => [a] -> (a -> C.ContT () m c) -> m ()
forLoop items fn =
    let contArr = fmap fn items
    in C.runContT (sequence_ contArr) return

breakOut :: Monad m => C.ContT () m c
breakOut = C.ContT $ \_ -> return ()

breakOutIf :: Monad m => Bool -> C.ContT () m ()
breakOutIf b = if b then C.ContT $ \_ -> return () else return ()
```

Here's the loop in action. To test breaking out, its given an infinite array but breaks out safely after 10 iterations.

```haskell
infiniteLoop = forLoop [1..] $ \i -> do
    if i > 10
        then breakOut
        else lift $ putStrLn $ show i

infiniteLoop2 = forLoop [1..] $ \i -> do
    breakOutIf (i > 10)
    lift $ putStrLn $ show i
```

## 2. `goto`

This one is generally discouraged in most languages, but just for fun here it is:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Monad.Trans.Cont  as C
import           Control.Monad.Trans.Class (lift)
import           System.Random             as R

--simple goto
goto = C.callCC $ \out -> let fn = out fn
                          in return fn

-- we can also provide back other arguments, in this
-- case some number, to allow more intelligent looping:
gotoC = C.callCC $ \out -> let fn num = out (fn, num)
                           in return (fn, 0)
```

Our `goto`s in action:

```haskell
-- based on the output of a random number generator,
-- we either go back to marker1, marker2, or finish
gotoEx1 = flip C.runContT return $ do

    marker1 <- goto
    lift $ putStrLn "one"

    marker2 <- goto
    lift $ putStrLn "two"

    (num :: Int) <- lift $ R.randomRIO (0,2)

    if num < 1 then marker1
    else if num < 2 then marker2
    else lift $ putStrLn "done"

-- loop back some number of times before continuing on:
gotoEx2 = flip C.runContT return $ do

    (marker1,num) <- gotoC
    lift $ putStrLn ("count: "++show num)

    if num < 10 then marker1 (num+1)
    else lift $ putStrLn "done"
```

## 3. The List Monad

We came quite close to this earlier when playing with multiple callback invocations.

```haskell
-- we use (:[]) to make the output of running any callback
-- an array, to ensure we can concatenate results at each stage.
makeList = flip runCont (:[])
each arr = cont $ \out -> mconcat $ fmap out arr

-- break out of the continuation if some Bool is true by
-- immediately returning an empty array which equates
-- to nothing when our arrays are concatenated
ignoreIf b = cont $ \out -> if b then [] else out ()
```

Now we've defined a couple of helpers, here it is in action:

```
eachEx1 :: [(Int,Int,Int)]
eachEx1 = makeList $ do
    n1 <- each [1,2,3]
    n2 <- each [4,5,6]
    n3 <- each [7,8,9]
    ignoreIf (n3 == 8) --ignore any combinations where n3 is 8
    return (n1,n2,n3)

-- outputs:
-- [(1,4,7),(1,4,9),(1,5,7),(1,5,9),(1,6,7),(1,6,9),(2,4,7),(2,4,9),(2,5,7),(2,5,9),(2,6,7),(2,6,9),(3,4,7),(3,4,9),(3,5,7),(3,5,9),(3,6,7),(3,6,9)]
```

# Summary

Continuations are a powerful tool that are quite mind bending on first encounter. Their main applications seem to be, manipulating control flow, combinatory work and early escaping. The latter has been used to generate more efficient versions of existing monads. With great power comes great responsibility though, and in many cases using Continuations may not be worth the added mental complexity over other simpler approaches.

Most of this code (and a couple of extra bits) is wrapped up into a file right [here][example-code] ready to be plugged into GHCI. Have a play and let me know what you think!

[example-code]: examples.hs