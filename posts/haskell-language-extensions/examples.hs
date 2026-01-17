{-# LANGUAGE ViewPatterns,
             PatternSynonyms,
             RankNTypes,
             FlexibleInstances,
             MultiParamTypeClasses,
             FunctionalDependencies,
             ExistentialQuantification,
             GADTs,
             ImplicitParams,
             TypeFamilies,
             UndecidableInstances,
             DataKinds,
             TypeOperators,
             PolyKinds,
             ConstraintKinds #-}

-- needed for ConstraintKinds:
import GHC.Exts (Constraint)

import qualified Data.List as L
import           Data.Typeable (cast, Typeable(..))
import           Data.Foldable (foldl', sum)

--
-- #####################
-- ### VIEW PATTERNS ###
-- #####################
--
-- view patterns let you run arbitrary computation in a pattern match,
-- performing the actual pattern match on the result of this computation.
--

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

-- no view patterns: take in list as well.
getOrDefaultList list name = case mRes of
    Just val -> val
    Nothing -> 5
  where mRes = findIn list name

-- view patterns: partial application can use prior arguments as well:
getOrDefaultList' list (findIn list -> Just val) = val
getOrDefaultList' _ _ = 5

-- view patterns can be nested and exist anywhere a pattern match can:

-- no view patterns: use list above to convert values to integers and
-- sum them up. 0 if not found in list.
-- eg. sumList ["london","paris"] == 22
sumList (a : as) = case mRes of
    Just val -> val + sumList as
    Nothing -> sumList as
  where mRes = findIn someList a
sumList [] = 0

-- view patterns simplify it a bunch:
sumList' ((findIn someList -> Just val) : as) = val + sumList as
sumList' _ = 0

--
-- ########################
-- ### PATTERN SYNONYMS ###
-- ########################
--
-- pattern synonyms allow you define aliases for pattern matches. They
-- allow you to abstract away details of your data structure without
-- introducing the extra overhead of converting to other types, and play
-- nice with view patterns too!
--
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

-- some simple patterns to act as aliases for certain DayTimes:
pattern SundayNoon     = DayTime Sunday (Time 12 00)
pattern MidnightFriday = DayTime Friday (Time 00 00)

-- Main> printDayTime SundayNoon
-- "It's Sunday, 12:00"
-- Main> printDayTime MidnightFriday
-- "It's Friday, 00:00"

-- we can use variables in patterns as well:
pattern Sun h m = DayTime Sunday (Time h m)
pattern Mon h m = DayTime Monday (Time h m)
-- ...

-- Main> printDayTime (Sun 15 30)
-- "It's Sunday, 15:30"

-- so far, there has been a one to one mapping between pattern
-- and data. Thus, we can treat patterns as if they were data.
-- Sun 12 00 == SundayNoon == DayTime Sunday (Time 12 00)
-- however, some patterns are one way; one pattern could match
-- many values. One way patterns use <- to denote this.

-- match any DayTime that is sunday or monday using these:
pattern AnytimeSun <- DayTime Sunday _
pattern AnytimeMon <- DayTime Monday _

-- these patterns no longer represent a value but a range of values,
-- anything that matches them, so we can use them to destructure but
-- not pass in.
-- printDayTime AnytimeSun == error! what time would it print??

-- print the day when given some DayTime value:
whichDay AnytimeSun = "Sunday!"
whichDay AnytimeMon = "Monday!"
whichDay _          = "Some other day!"

-- we can match the whole daytime as in other pattern matches:
whichDay' d@AnytimeSun = "Sunday "++printTime (time d)
whichDay' d@AnytimeMon = "Monday "++printTime (time d)
whichDay' _            = "Other."

--one way patterns can also hand back data in variables which
--might look a little nicer, esp with complex matches. Here we
--give t back so that we can use it in the match
pattern SunT t <- DayTime Sunday    t
pattern MonT t <- DayTime Monday    t
-- ...

--we can of course mix and match pattern synonyms and normal
--destructuring too. first success wins
whichDay'' (SunT t)      = "Sunday, "++printTime t
whichDay'' (MonT t)      = "Monday, "++printTime t
whichDay'' (DayTime _ t) = "Other day, "++printTime t

--patterns can also use view patterns to help matching:
isMorningHour   t = t < 12
pattern Morning   <- DayTime _ (Time (isMorningHour   -> True) _)
timeOfDay Morning = "It's morning"
timeOfDay _       = "It's not morning"

--view patterns can also give back arbitrary variables. here we expect
--a tuple of True,_ back, but match on the secondparam. Our matching can
--be as powerful and rbitrary as we like given this; any structure could
--be viewed through patterns in a totally different way.
isMorningHour' t = (t < 12, t)
pattern MorningT h m <- DayTime _ (Time (isMorningHour' -> (True,h)) m)
timeOfDay' (MorningT h m) = "It's morning, "++show h++":"++show m
timeOfDay' _              = "It's not morning"


--
-- ##########################
-- ### FLEXIBLE INSTANCES ###
-- ##########################
--
-- Allows you to define typeclass instances in a more flexible way
--

-- a simple class for seeing whether something is "truthy"
class Truthy a where
    truthy :: a -> Bool

instance Truthy Bool where
    truthy = id

instance Truthy Char where
    truthy c = not $ L.elem c [' ','\n','\t']

instance Truthy Int where
    truthy i = i /= 0

-- without flexible instances, we could not do:
--
-- instance Truthy String where
--   truthy s = length s /= 0
--
-- we'd get told that instance declarations need to
-- take the form T a1..an where a1..an are type *variables*
-- but String == [Char] == [] Char.
--
-- [] a would be fine, but [] Char would not. Flexible instances
-- removes this restriction. otherwise we'd make it so that a
-- had to be Char by introducing a dependency:

class CharType a
instance CharType Char

instance CharType a => Truthy [a] where
    truthy s = length s /= 0

-- handle items in list polymorphically if we want several [] a
-- versions for different a's eg:

class Count a where
    countLen :: a -> Int

instance Count Char where
    countLen a = 1
instance Count Int where
    countLen i = i

instance Count a => Count [a] where
    countLen as = foldl' (\sum a -> sum + countLen a) 0 as

-- this now works for Strings or arrays of numbers:
-- main> countLen [1::Int,2,3,4]
-- 10
-- main> countLen "hello"
-- 5

--
-- Completely general case; totally different implementations
-- for different list types can be delegated to another typeclass:
--
class Count' a where
    countLen' :: a -> Int

instance CountArr a => Count' [a] where
    countLen' as = countLenArr as

class CountArr a where
    countLenArr :: [a] -> Int

instance CountArr Char where
    countLenArr as = length as
instance CountArr Int where
    countLenArr as = sum as

--
-- ################################
-- ### MULTI PARAM TYPE CLASSES ###
-- ################################
--
-- type classes with more than one param allowed (or none!)
--

-- typeclass with 2 params, a and b.
class LooseEq a b where
    looseEq :: a -> b -> Bool

-- now we can define instances for both
instance LooseEq Int Float where
   looseEq a b = a == round b
instance LooseEq Float Int where
    looseEq a b = round a == b

-- looseEq (10.2 :: Float) (10 :: Int)
-- looseEq (10 :: Int) (10.2 :: Float)
--
-- we have to provide types otherwise it doesnt know what types
-- the number literaly are. Even with just one instance, the
-- compiler assumes that more can be created and requires types.

--
-- ###############################
-- ### FUNCTIONAL DEPENDENCIES ###
-- ###############################
--
-- For use in multi param type classes; provides a way to state
-- that one type is dependant on others, so that the compiler
-- can infer it without you having to explicitly tell it. example:
--

--no fundeps:
class Adder' a b c where
    add' :: a -> b -> c

instance Adder' Int Int Int where
    add' a b = a + b

-- this will fail to type check:
--
--   add' (1::Int) (2::Int)
--
-- the problem is that at any point, someone could
-- define an instance of Adder' with, say, Int Int Double
-- as its 3 params, so no way for the type checker to be
-- sure what output type is. providing it explcitly does the
-- job, but is more verbose than hoped:
--
--   add' (1::Int) (2::Int) :: Int

--with fundeps:
class Adder a b c | a b -> c where
   add :: a -> b -> c

instance Adder Int Int Int where
    add a b = a + b

-- note the new notation which says that the type c is uniquely
-- determined from the types a and b. This means we can no
-- longer declare another instance of Adder with "Int Int x",
-- and thus the compiler knows that given Int and Int, the third
-- type will be Int too. this now works:
--
--   add (1::Int) (2::Int)

--
-- #############################
-- ### OVERLAPPING INSTANCES ###
-- #############################
--
-- now we've defined specific instances for looseEq, we could
-- try to define a more general one that works on multiple numbers..
--

-- a quick class to convert numbers to integers for use in our general example
class Num a => ToInteger a where
   numToInteger :: a -> Integer

instance ToInteger Int where
    numToInteger i = toInteger i
instance ToInteger Double where
    numToInteger d = round d
instance ToInteger Float where
    numToInteger f = round f

--
-- We declare a less specific version, and say that it is overlappable, meaning
-- more specific versions (those above) will be fine. We could also go the other
-- way round and add OVERLAPPING to the more specific instances above, to say
-- that they are allowed to be overlapping this. Thirdly, we could use OVERLAPS
-- in either position as it is equal to both.
--
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

--
-- ##################################
-- ### EXISTENTIAL QUANTIFICATION ###
-- ##################################
--
-- allows you to remove types on the left of a data declaration and instead
-- restrict the contained type to belonging to some class instead, say.
--
-- we want to store anything "showable" in some type (Typeable not required yet):
--

data Showable = forall a. (Show a, Typeable a) => Showable a

-- define show instance to just apply show to the contained type,
-- since we know that's always allowed:
instance Show Showable where
    show (Showable a) = show a

-- now, we can put any arbitrary type into an array and show them all:
showables = [Showable (2 :: Int), Showable (3 :: Float), Showable "hello", Showable 'c']
-- show showables == [2,3.0,"hello",'c']

-- a neat trick here is that we can extract the original type back out
-- of the Showable container even though it's been erased, using Data.Typeable
-- and "cast":

castShowable (Showable a) = cast a

-- this would return true:
isHi = castShowable (Showable "hi") == Just "hi"
-- and this false:
isTwo = castShowable (Showable "hi") == Just (2 :: Int)

--
-- this extension is also required to be able to add constraints but still expose
-- the contained type(s) in a more normal way eg:
--
data ShowableT a = Show a => ShowableT a

--
-- #############
-- ### GADTs ###
-- #############
--
-- (Generalized ALgebraic Data Types)
--
-- Allows you to declare data types using a function signature style.
-- This allows you to decide exactly what the final type will be in each
-- case, which is otherwise not possible.
--

-- take a simple data type:
data MyData a = Something a | Otherthing

-- Otherthing is of type "MyData a".
-- But what if we wanted to constrain its type a bit? The equivalent
-- decalration using GADT syntax:
data MyData' a where
    Something' :: a -> MyData' a
    Otherthing' :: MyData' a

-- but now, insetad of returning MyData' a, Otherthing could return MyData' Int
-- or something instead. one example of this is when using types to describe an
-- AST.

data Expr'' = S'' String
            | I'' Int
            | Add'' Expr'' Expr''
            | Append'' Expr'' Expr''

-- a basic AST, but notice that Add can take any expression even though "I Int" is
-- the only one that makes sense. We can use a type on Expr to encode this, so that
-- Add only takes Expr Ints, and Append only takes Exp Strings:

data Expr' a = S' String
             | I' Int
             | Add' (Expr' Int) (Expr' Int)
             | Append' (Expr' String) (Expr' String)

-- but without GADTs we have no way to enforce that the type of, say, "I Int" will
-- actually be 'Expr' Int'. with GADT's we express constructors as functions,
-- and so we provide that final type:

data Expr a where
    I      :: Int         ->                Expr Int
    Add    :: Expr Int    -> Expr Int    -> Expr Int
    S      :: String      ->                Expr String
    Append :: Expr String -> Expr String -> Expr String

eval :: Expr a -> a
eval (S s) = s
eval (I i) = i
eval (Add a b) = eval a + eval b
eval (Append a b) = eval a ++ eval b

-- eval $ (I 1 `Add` I 2) `Add` (I 3) == 6
-- eval $ (S "Hello" `Append` S " ") `Append` (S "World") == "Hello World"
-- eval $ (S "Hello" `Append` S " ") `Add` (S "World") -- BZZZZT! Type Error.

--
-- #######################
-- ### IMPLICIT PARAMS ###
-- #######################
--
-- allow functions to require that some value exists in the scope it's used in,
-- without one having to actually explicitly pass in said value. Allows one to
-- define global things in 'main' and not have to think about threading them
-- through everywhere.
--

-- define implicit param as variable with question mark in front of.
fn1 a = fn2 a where ?showIt = \x -> show x
fn2 a = fn3 a
fn3 a = fn4 a

-- add constraint that implicit param called ?showIt exists in the scope this
-- it used. Now, calling fn1 will work, but calling fn2, fn3 and fn4 directly
-- will fail as the implicit param would not be defined if not for fn1.
fn4 :: (?showIt :: a -> String) => a -> String
fn4 a = ?showIt a

--
-- The downside? You can't type annotate anything and still thread your
-- implicit params through, without adding them to every annotation.
-- this wouldnt work:
--

--tyfn1 :: String -> String
--tyfn1 a = tyfn2 a where ?showIt = "world"

--tyfn2 :: String -> String
--tyfn2 a = tyfn3 a

--tyfn3 :: (?showIt :: String) => String -> String
--tyfn3 a = a ++ ?showIt


--
-- #####################
-- ### TYPE FAMILIES ###
-- #####################
--
-- Theis extension introduces type and data families, both in and out of
-- type classes. I'll look at each.
--
-- Type families
-- =============
--
-- type aliases on steroids. Allows you to express relationships between types
-- such as, if this type is `a`, that type should be `b`. Can be closed (define
-- all of the mappings once and then can't extend them) or open (can add new
-- mappings elsewhere). As a syntactic sugar almost, type instances can also
-- be defined in classes rather than on their own, and indeed are often used
-- in conjunction with type classes.
--

-- a simple beginning example of an OPEN type family.
-- this one maps two input types to one output type.
type family AddResult a b
type instance AddResult Double Double = Double
type instance AddResult Int Double = Double
type instance AddResult Double Int = Double
type instance AddResult Int Int = Int

-- since, to get the desired output, we need to handle
-- different inputs separately, we define a typeclass.
-- some uses of type families dont require type classes
-- but many do as, in order to satisfy your dynamic type
-- signature, you need to handle different inputs differently
-- depending on their type.
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

-- naturally, we might want to keep the result type with the
-- instance, since they work together. We can rewrite the above
-- using associated types instead of free ones, like so:

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

-- either of the above can be extended by others. If we don't want
-- this to be the case, we can declare a closed type family:
type family AddResult'' a b where
    AddResult'' Double Double = Double
    AddResult'' Int Double = Double
    AddResult'' Double Int = Double
    AddResult'' Int Int = Int


-- Type families can be recursive, too. This one takes some arbitrary
-- function signatue, and gives back its return type by recursing
-- through it:
type family ReturnVal ty where
    ReturnVal (a -> b) = ReturnVal b
    ReturnVal b = b

-- this typechecks
-- ===============
-- 'a' :: ReturnVal (Int -> String -> Char)
--
-- this does not
-- =============
-- 'a' :: ReturnVal (Char -> String -> Int)

-- Here I turn a nested 2-tuple into a function signature
-- by recursively applying my TupleFn family.
--
-- (Int,(Char,String,()))) => Int -> Char -> String -> out
type family TupleFn ty out where
    TupleFn () output = output
    TupleFn (a,b) output = a -> (TupleFn b output)

-- we want to do something different depending on the input
-- type, so we use a type class to give us this:
class ApplyFnToTuple a where
    applyFnToTuple :: a -> TupleFn a out -> out

instance ApplyFnToTuple b => ApplyFnToTuple (a,b) where
    applyFnToTuple (a,b) fn = applyFnToTuple b (fn a)

instance ApplyFnToTuple () where
    applyFnToTuple _ fn = fn

-- valid
-- =====
-- applyFnToTuple ('a',('b',())) $ \a b -> [a,b] == "ab"
-- applyFnToTuple ("hello",(12,('r',()))) $ \h n r -> h ++ show n ++ [r] == "hello12r"
--
-- invalid
-- =======
-- applyFnToTuple ('a',('b',())) $ \a -> [a] -- not enough args
-- applyFnToTuple ('a',(12,())) $ \a n -> [a,n] -- n is number, wrong type.

--
-- tuples require different handlign based on input type, and so we
-- need to use a type class. we can define our own type that does not..
--

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

-- this function applies items in MyList a to a MyListFn a just
-- like we did with tuples. Note no type family, because
-- no type dependant differences in behaviour needed:
applyFnToMyList :: MyList a -> MyListFn a out -> out
applyFnToMyList (LCons a b) fn = applyFnToMyList b (fn a)
applyFnToMyList LNil fn = fn

-- just like tuple:
-- ================
-- applyFnToMyList (LCons 'a' (LCons 'b' LNil)) $ \a b -> [a,b] == "ab"
-- applyFnToMyList (LCons "hello" (LCons 12 (LCons 'r' LNil))) $ \h n r -> h ++ show n ++ [r] == "hello12r"

-- #############################
-- ### Equality Constraint ~ ###
-- #############################
--
-- the equality constraint '~'. This comes with GADTs too. Basically,
-- it allows you to asset that two types are the same before allowing
-- some function to work. This is a neat way to provide the compiler
-- extra information as well to clear up certain ambiguities.

-- our early Truthy example, where we defined a CharType instance,
-- could be replace with this:
class Truthy' a where
    truthy' :: a -> Bool

-- we tell GHC that a is a Char as a constraint. This is less
-- ambiguous than using a CharType constraint, since other instances
-- could be added to CharType, making `a` still ambiguous, whereas
-- here `a` can be only Char.
instance a ~ Char => Truthy' [a] where
    truthy' s = length s /= 0

-- #####################
-- ### Data Families ###
-- #####################
--
-- Also enabled with the TypeFamilies extension, these give data
-- equivalent to type families as described above. Unlike type
-- families where multiple type aliases can map to the same thing,
-- each data instance is expected to have unique constructors;
-- just like in any other data declaration we cant have multiple
-- constructors with the same name

-- open ended data families:

data family MyMap key val
data instance MyMap Bool val = BoolMap (Maybe val) (Maybe val)
    deriving Show
data instance MyMap Int val = IntMap [(Int,val)]
    deriving Show

-- equivalent to:
--
--data MyMap key val where
--    BoolMap :: Maybe val -> Maybe val -> MyMap Bool val
--    IntMap :: [(Int,val)] -> MyMap Int val

-- to use our new map we might end up creating a typeclass
-- to operate on maps differently based on the key used.
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

-- Now we effectively have a type, MyMap k v, specialised (albeit poorly) for when
-- the key is an Int or a Bool. The beauty of these open ended instances are that
-- if we create new types, we can create new MyMap instances (both classes and data)
-- to efficiently handle those types after the fact.

-- as with type families, we can stick these into our type classes as well to
-- keep everything together:

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

-- #######################
-- ### Kind Signatures ###
-- #######################
--
-- Allows you to add signatures to type declarations
-- describing the kinds. TypeFamilies gives you this
-- as well as KindSignatures
--
-- data family MyMap key val
--
-- could become:

data family MyMap'' :: * -> * -> *
data instance MyMap'' Int val = IntMap'' [(Int,val)]

--
-- where * is a kind that represents any type. Basically
-- one level above types. types categorise values, kinds
-- categorise types. Data kinds let us define our own
-- categories and the types that fit into them..
--


-- ##################
-- ### Data Kinds ###
-- ##################
--
-- With DataKinds, we can create new "kinds"; new categories
-- to fit our types into that are distinct from the normal
-- kind of *.
--
-- Any basic types (not GADT's, and not using already promoted
-- types for instance) are "promoted" to kinds, and their
-- constructors promoted to types. Thus, for:

data Number = One | Two

-- potential ambiguity?
-- if we ask for One, do we mean the type One below of
-- kind * or the promoted constructor One above of kind
-- Number? 'One == promoted constructor, One = Type below.
data One
data Two

-- we'd end up getting, as well as the standard data definition,
-- the **kind** MyData which is inhabited by two **types**, One
-- and Two. More specific tagging eg:

data SomeData :: Number -> * where
    OneS :: SomeData 'One
    TwoS :: SomeData 'Two

-- Now the type of a in 'SomeData a' must be one that falls into
-- the "Number" kind, ie 'One or 'Two. Compiler rejects anything else
-- eg this:
--
-- a :: SomeData Int
-- a = undefined

-- we can use our MyList example from earlier as an example of
-- where DataKinds are handy. Instead of
--
-- data Cons
-- data Nil
--
-- we define a ListK with ConsTy and NilTy, which is then promoted
-- giving us also the **kind** ListK and the types ConsTy a a and NilTy
-- that belong to that kind:

data ListK a = ConsTy a (ListK a) | NilTy

data MyList' :: ListK * -> * where
    LCons' :: itemty -> MyList' a -> MyList' (ConsTy itemty a)
    LNil'  :: MyList' NilTy

-- we'd use this like our original MyList:
--
-- l1 = LCons' 12 (LCons' 'a' LNil') :: MyList' ('ConsTy Int ('ConsTy Char 'NilTy))
--
-- except now in MyList' a, 'a' is restricted to using our promoted ConsTy and NilTy
-- types of kind ListK, rather than being any type (anything of kind *).
--
-- ######################
-- ### Type Operators ###
-- ######################
--
-- Type operators let us declare operators at the type level just
-- like we would at the value level. Rather than ConsTy, we could
-- make our list kind a little more like the haskell list type:

data ListK2 a = a :+: ListK2 a | NilTy2

-- yet another version of our MyList, this time using the new
-- operator instead of ConsTy:
data MyList2 :: ListK2 * -> * where
    LCons2 :: itemty -> MyList2 a -> MyList2 (itemty :+: a)
    LNil2  :: MyList2 NilTy2

-- now when we use our MyList constructors we get a prettier type:
--
-- LCons2 'a' (LCons2 'b' LNil2) :: MyList2 (Char ':+: (Char ':+: 'NilTy2))

-- DataKinds also promotes things like the haskell list to type level,
-- so we can construct types using that rather than our own version
-- Here's another version of MyList using the new promoted list type
-- and promoted cons operator (:)..

data MyList_ :: [*] -> * where
    LCons_ :: itemty -> MyList_ a -> MyList_ (itemty ': a)
    LNil_  :: MyList_ '[]

-- now this type assertion would be fine. Notice the prettier type:
--
-- LCons_ 'a' (LCons_ 12 LNil_) :: MyList2 '[Char,Int]

-- ##################
-- ### Poly Kinds ###
-- ##################
--
-- Now we can introduce new kinds, we'll quickly find that we are restricted
-- to working with one kind at a time, unlike types where we can define
-- polymorphic functions to work across different types.
--
-- PolyKinds gives us this polymorphism at the kind level. Let's see why we'd
-- want it with a type family example to reverse a promoted list:


type Reverse (a :: [*]) = DoReverse a '[]

--this type takes a type level array eg [Int,Char,String] and reverses
--it to eg [String,Char,Int].
type family DoReverse (a :: [*]) (b :: [*]) :: [*] where
    DoReverse (a ': as) out = DoReverse as (a ': out)
    DoReverse '[] out = out

-- these two types are now equivalent:
-- Reverse [Int,Char,String] == [String,Char,Int]

-- to use our type level list, we could use another type family which
-- converts it to, say, a tuple:
type family TupFromList (a :: [*]) where
    TupFromList (a ': as) = (a, TupFromList as)
    TupFromList '[] = ()

-- now, these are true:
-- ====================
--
-- (12,('a',("hello",()))) :: TupFromList [Int,Char,String]
-- ("hello",('a',(12,()))) :: TupFromList (Reverse [Int,Char,String])
--
-- So, why PolyKinds? This would fail:
--
-- data LetterK = A | B
--
-- a :: Reverse [A,B]
-- a = undefined
--
-- using our promoted types A and B (of kind LetterK) isn't allowed
-- with our type function Reverse, as Reverse only supports kind *
-- (ie normal haskell types, not our custom ones):
--
--
-- PolyKinds lets us use a letter in plance of */LetterK to say it'll
-- work on any kind, making our type altering functions like Reverse
-- more flexible (since they don't care what kind the types are).
-- our Reverse taking advantage of PolyKinds:

type Reverse_ (a :: [k]) = DoReverse_ a '[]

type family DoReverse_ (a :: [k]) (b :: [k]) :: [k] where
    DoReverse_ (a ': as) out = DoReverse_ as (a ': out)
    DoReverse_ '[] out = out

-- ########################
-- ### Constraint Kinds ###
-- ########################
--
-- This extension unifies constraints with the typesystem, giving them
-- their own kind, Constraint. This allows us to talk about constraints
-- like we can other types and kinds. Basically, any constraint, or
-- tuple of constraints, now has the *kind* Constraint, and is no longer
-- relegated to appearing only to the left of "=>". Lets see what this
-- extension allows us to do:

-- we can create type synonyms for constraints to save on repetition:
type ReadShow a = (Show a, Read a)

-- and then use these as we would the orignals:
readAndShow :: ReadShow a => a -> a
readAndShow val = read $ show val

-- we can now vary constraints across different instances of
-- typeclasses. I did something like this recently, whereby I
-- wanted to wrap things such that we went from:
--
-- out       ==>  a -> out
-- a -> out  ==>  a -> out
--

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

-- These are all fine:
--
-- withFunc True     1   == True
-- withFunc False    'd' == False
-- withFunc (== 'a') 'b' == False
-- withFunc (== 'a') 'a' == True
-- withFunc (< 7)    5   == True
