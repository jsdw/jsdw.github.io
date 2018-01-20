{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Monad.Trans.Cont  as C
import           Control.Monad.Trans.Class (lift)
import           Control.Monad             (guard)
import           System.Random             as R

--
-- starting simple, here is how values can be presented in the form
-- of continutatons. We return a function that when passed some other function,
-- which I'll refer to as a callback, hands back a value to it.
--
twoC = \out -> out 2
helloC = \out -> out "hello"

-- these take the form (a -> r) -> r. In other words, provide
-- them some function eg id (which is just \res -> res) as a final
-- continuation, to get back the value. tN's all eval to True..

t1 = twoC id == 2
t2 = helloC id == "hello"

-- we could simplify creating these values with a simple function
-- that takes a value and gives back a continutation like the above:
ret val = \out -> out val

-- so now we can create continuations like so:
anotherTwoC = ret 2
anotherHelloC = ret "hello"

--
-- next, we can write a method to join continuations together.
-- this returns a new continuation from f1 that is the result of passing
-- the value from inC to fn.
--
-- We want to act on the result of inC calling its callback with some value,
-- so we pass inC a callback that itself takes the value of inC (inCval), passes that to 
-- fn (which we expect to return a new continuation), and then passes the final out
-- function to it (which is eventually provided to the continuation to extract a
-- final result.
--
inC `bind` fn = \out -> inC (\inCval -> (fn inCval) out)

-- a simple example that doubles the value handed back from twoC:
fourC = twoC `bind` \two -> ret (two*2)

-- a more complex example combining the results of two continuations:
twoHelloC = twoC `bind` \two -> 
              helloC `bind` \hello -> 
                ret $ (show two)++hello

t3 = fourC id == 4
t4 = twoHelloC id == "2hello"

--
-- The interesting thing is that when chaining continuations with `bind`,
-- all that is actually altered is the effect of the callback provided to the previous
-- continuation.
--
-- what happens then if you pass a function that doesnt use its callback?
badC = \out -> "boom!"

twoBadC = twoC `bind` \two -> 
            badC `bind` \hello -> 
              ret $ (show two)++hello

-- since we never use the callback, all subsequent continuations (that each
-- effect only the callback function provided to those before them), including
-- the final function we apply (above that's been "id") are not made use of,
-- since their effects are all wrapped into that callback. Thus:
t5 = twoBadC id == "boom!"

-- The same approach we took to combine continuations can be used to write
-- a map function, that runs some function on the eventual result of some
-- continuation. Once again, it does this by wrapping the callback provided
-- to the input continuation to alter the value before returning it to "out".
mapC fn inC = \out -> inC (\inCval -> out (fn inCval))

-- Note that we are basically applying inCval to fn, and then out. Thus,
-- this can be simplified using point free notation to:
mapC' fn inC = \out -> inC (out . fn)

-- to test it works, make sure it has the right result with contiuations
-- that use their callback AND those that dont:
t6 = mapC (*2) (\out -> out 5) id == 10
t7 = mapC (*2) (\out -> 5) id == 5

-- a naive approach to mapping might have been to run the input continuation
-- with id to get its result and then work on that:
badMapC fn inC = \out -> let val = fn (inC id) 
                         in out val

-- whle this seems to work at first, this would apply map regardless of
-- whether the continuation uses its callback or not, and so would result in:
t8 = badMapC (*2) (\out -> out 5) id == 10
t9 = badMapC (*2) (\out -> 5) id == 10 -- wrong, should be 5

--
-- This continuation framework already exists in the form of the Cont Monad.
--
-- The entire basic definition of the Cont monad (with applicative and
-- map lark to satisfy Monad) basically wraps our functions in a type,
-- Cont, where return is our ret and >>= is our bind.
--
-- This is a non-transformer version of the real definition for simplicity.
--
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
-- (in the transformer version it hides away the identity monad):
--
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC fn = Cont $ \out -> runCont (fn (\a -> Cont $ \_ -> out a)) out

cont :: ((a -> r) -> r) -> Cont r a
cont = Cont

--
-- Now our earlier example can be rewritten using monadic do notation as:
--
twoC' = return 2
helloC' = return "hello"

twoHelloC' = do
    two <- twoC'
    hello <- helloC'
    return $ (show two)++hello

-- remember this desugars to the following, which should look very similiar
-- to our first non-wrapped attempt:
twoHelloC'' = twoC' >>= \two -> 
                helloC' >>= \hello -> 
                  return $ (show two)++hello

-- because our continuation is now wrapped in Cont, we 
-- now use runCont to pull it out before we can apply it
-- (parentheses not needed):
t10 = (runCont twoHelloC') id == "2hello"

-- we can still escape as we could before:
twoBadC' = do
    two <- twoC'
    hello <- cont $ \out -> "boom!"
    return $ (show two)++hello

t11 = runCont twoBadC' id == "boom!"

-- we can also call the callback more than once. Remember, the
-- callback has wrapped into it all of our surrounding state, including
-- the final function we use to pull out a value. Lets see:
twoMultiC = do
    two <- twoC'
    hello <- cont $ \out -> out "hello" ++ out "hello"
    return $ (show two)++hello

t12 = runCont twoMultiC id == "2hello2hello"
t13 = runCont twoMultiC (++"BOOM!") == "2helloBOOM!2helloBOOM!"

-- repetition anywhere has the same effect, the result of running
-- these being identical to above:
twoMultiC' = do
    two <- cont $ \out -> out 2 ++ out 2
    hello <- helloC'
    return $ (show two)++hello

twoMultiC'' = do
    two <- twoC'
    hello <- helloC'
    cont $ \out -> out ((show two)++hello) ++ out ((show two)++hello) 

-- this is because bind wraps each continuations callback with that
-- produced as a result of the next continuation, so calling the callback
-- from the point of view of any continuation has the effect of running
-- all subsequent continuations. The do notation obscures things a bit.
-- this is the desugared version of twoMultiC' with inlined twoC and helloC
-- continuations:
desugaredTwoMultiC' =
    (cont $ \out1 -> out1 2 ++ out1 2) >>= \two ->
        (cont $ \out2 -> out2 "hello") >>= \hello ->
            (cont $ \out3 -> out3 $ (show two)++hello)

-- each time out1 is called, the continuation provided out2 is called, and
-- each time out2 is called, the continuation provided out3 is called. the result
-- of "out1 2" then is "out3 $ (show 2)++"hello". "out3" is whatever we pass
-- to the continuation to run it, in most examples above "id".

-- we can generate combinations, much like the list monad, by splitting the 
-- flow like this. for example:
multiMultiC = do
    n <- cont $ \out -> out "1" ++ out "2"
    l <- cont $ \out -> out "a" ++ out "b"
    x <- cont $ \out -> out "X" ++ out "Y"
    return $ n++l++x++" "

t14 = runCont multiMultiC id == "1aX 1aY 1bX 1bY 2aX 2aY 2bX 2bY "
-- Each time "out" is run, it leads to all subsequent "out"s bring run,
-- which end up resolving in depth first order to:
--
-- out "1"
--   out "a"
--     out "X"
--       "1"++"a"++"X"++" "
--     out "Y"
--       "1"++"a"++"Y"++" "
--   out "b"
--     out "X"
--       "1"++"b"++"X"++" "
--     out "Y"
--       "1"++"b"++"Y"++" "
-- out "2"
--   out "a"
--     out "X"
--       "2"++"a"++"X"++" "
--     out "Y"
--       "2"++"a"++"Y"++" "
--   out "b"
--     out "X"
--       "2"++"b"++"X"++" "
--     out "Y"
--       "2"++"b"++"Y"++" "
--
-- Essentailly, each time out is called, a result is generated from the last continuation
-- in this case n++l++x++" ". At each step we just append the strings together.
--
-- What if we exit early?

boom1C = do
    n <- cont $ \out -> "boom! "
    l <- cont $ \out -> out "a" ++ out "b"
    x <- cont $ \out -> out "X" ++ out "Y"
    return $ n++l++x++" "

-- here, our first line never calls out, so we just return the string:
t15 = runCont boom1C id == "boom! "


boom2C = do
    n <- cont $ \out -> out "1" ++ out "2"
    l <- cont $ \out -> "boom! "
    x <- cont $ \out -> out "X" ++ out "Y"
    return $ n++l++x++" "

-- here, our we call out twice, each one hitting the second continuation and
-- exiting with boom, so we get two of them appended because out "1" and out "2" are:
t16 = runCont boom2C id == "boom! boom! "


boom3C = do
    n <- cont $ \out -> out "1" ++ out "2"
    l <- cont $ \out -> out "a" ++ out "b"
    x <- cont $ \out -> "boom! "
    return $ n++l++x++" "

-- each contnuation calls its callback twice, so we end up hitting boom 4 times:
t17 = runCont boom3C id == "boom! boom! boom! boom! "

-- so we can exit early, but cant guarantee how many times that exit will be hit
-- if we split the computation on any prior steps.

--
-- let's look at callCC (call with current continuation)
--

--
-- This simple example returns True. Why? callCC takes a function as an
-- argument, and expects it to return a continuation. It passes the function
-- an exit callback which, when called, returns a continuation that ignores
-- its callback and just returns a value to the outer continuation, breaking
-- the inner continuation chain.
--
-- These all return True, and never hit undefined (and thus error):
--
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

-- or in this simple case just by exiting the inner continuation early
callCCex3 = do

    val <- do

        cont $ \_ -> True
        undefined

    return val

-- by giving you an explicit callback to use to exit the continuation, callCC has the
-- added advantage that you can nest uses of callCC and exit from any depth to any other with ease

callCCex4 = do
    val <- callCC $ \exit -> do

        innerval <- callCC $ \innerExit -> do

            exit True
            undefined

        undefined

    return val


--
-- Other things we can do with continuations?
--

--
-- we can make a for loop that one can break out of
-- immediately. This uses ContT which allows other
-- monads to be embedded within the continuation:
--
forLoop :: Monad m => [a] -> (a -> C.ContT () m c) -> m ()
forLoop items fn =
    let contArr = fmap fn items
    in C.runContT (sequence_ contArr) return

breakOut :: Monad m => C.ContT () m c
breakOut = C.ContT $ \_ -> return ()

breakOutIf :: Monad m => Bool -> C.ContT () m ()
breakOutIf b = if b then C.ContT $ \_ -> return () else return ()

-- heres the loop in action. To test breaking out, its given an 
-- infinite array but breaks out safely after 10 iterations
infiniteLoop = forLoop [1..] $ \i -> do
    if i > 10 
        then breakOut 
        else lift $ putStrLn $ show i

infiniteLoop2 = forLoop [1..] $ \i -> do
    breakOutIf (i > 10)
    lift $ putStrLn $ show i

--
-- or how about goto (implemented by returning a function
-- that is equal to the curent continuation's state (which
-- itself has to return the same function):
--

goto = C.callCC $ \out -> let fn = out fn 
                          in return fn

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

-- we can also provide back other arguments, in this
-- case some number, to allow more intelligent looping:
gotoC = C.callCC $ \out -> let fn num = out (fn, num) 
                           in return (fn, 0)

gotoEx2 = flip C.runContT return $ do

    (marker1,num) <- gotoC
    lift $ putStrLn ("count: "++show num)

    if num < 10 then marker1 (num+1)
    else lift $ putStrLn "done"

-- we can recreate a list monad by repeat calls to the
-- continuation callback and concatenating the result.
-- we use (:[]) to make the output of running any callback
-- an array, to ensure we can concatenate results at each stage.
makeList = flip runCont (:[])
each arr = cont $ \out -> mconcat $ fmap out arr

-- break out of the continutaion if some bool is true by
-- immediately returning an empty array which equates
-- to nothing when our arrays are concatenated
ignoreIf b = cont $ \out -> if b then [] else out ()

eachEx1 :: [(Int,Int,Int)]
eachEx1 = makeList $ do
    n1 <- each [1,2,3]
    n2 <- each [4,5,6]
    n3 <- each [7,8,9]
    ignoreIf (n3 == 8) --ignore any combinations where n3 is 8
    return (n1,n2,n3)

-- outputs:
-- [(1,4,7),(1,4,9),(1,5,7),(1,5,9),(1,6,7),(1,6,9),(2,4,7),(2,4,9),(2,5,7),(2,5,9),(2,6,7),(2,6,9),(3,4,7),(3,4,9),(3,5,7),(3,5,9),(3,6,7),(3,6,9)]