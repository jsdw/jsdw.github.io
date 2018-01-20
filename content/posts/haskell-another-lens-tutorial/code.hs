{-# LANGUAGE RankNTypes #-}

-- define our person and address data structures:
data Person = Person {
	name :: String,
	age :: Int,
	address :: Address
} deriving Show

data Address = Address {
	house :: Int,
	street :: String,
	city :: String
} deriving Show

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

--make functions to ease the pain a little:
setHouse person value = person { address = (address person) { house = value }  }
setStreet person value = person { address = (address person) { street = value }  }

newPerson1 = setHouse james 45
newPerson2 = setStreet james "New Street"

--a starting attempt to make a general lens "shape" to use:
data Lens' thing prop = Lens' {
	view' :: thing -> prop,
	update' :: (prop -> prop) -> thing -> thing
}

--we can define a simple set function based on the update function:
set' :: Lens' thing prop -> prop -> thing -> thing 
set' ln newValue thing = (update' ln) (\_ -> newValue) thing 

addressLens :: Lens' Person Address
addressLens = Lens' address (\fn thing -> thing { address = fn (address thing) })

cityLens :: Lens' Address String
cityLens = Lens' city (\fn thing -> thing { city = fn (city thing) })

personToCityLens :: Lens' Person String
personToCityLens = Lens' ((view' cityLens).(view' addressLens)) ((update' addressLens).(update' cityLens))

--alternately, don't create a new lens and just compose the city and address lenses on the fly.
--we could abbreviate this in point free style as was done above:

viewCity person = (view' cityLens . view' addressLens) person
updateCity fn person = (update' addressLens . update' cityLens) fn person

--brief interlude into RankNTypes...

-- we don't know anything about a, but we can, say, wrap it up:
justA :: forall a. a -> Maybe a
justA a = Just a

-- let's try wrapping up a tuple of arbitrary things with this.
wrapPair :: (forall a. a -> t a) -> (b,c) -> (t b, t c)
wrapPair wrappingFn (x,y) = (wrappingFn x, wrappingFn y)

--our real lens type alias:
type Lens thing prop = Functor f => (prop -> f prop) -> thing -> f thing

-- ######## trying to define view

-- we need to turn `f thing` into `prop`,
-- so lets make a functor f that can do this for us:

--a data type parameterised by two variables,
--but which only really cares about one:
data Const a b = Const { getConst :: a }

--a functor normally holds one type inside it, like a monad.
--so we partially apply the Const type to meet that criteria:
instance Functor (Const a) where
	fmap _ (Const a) = Const a

--this means that (Const a) can be our Functor f, and then b can be
--whatever type we need; we don't actually use it.

--for the record, Const is already defined in Control.Applicative,
--and it does exactly what we need and has more instances for other uses.

--view defined with the help of Const:
view :: Lens thing prop -> thing -> prop
view ln thing = getConst $ ln (\p -> Const p) thing

-- ######## trying to define update:

-- we need a Functor that just holds our value, so that we can
-- match the lens function signature:
data Identity a = Identity { runIdentity :: a }

instance Functor Identity where
	fmap fn (Identity a) = Identity (fn a)

update :: Lens thing prop -> (prop -> prop) -> thing -> thing
update ln fn thing = runIdentity $ ln (\p -> Identity (fn p)) thing

-- set easy to define in terms of update:
set :: Lens thing prop -> prop -> thing -> thing
set ln newValue thing = update ln (\_ -> newValue) thing

--making some lenses:

betterAddressLens :: Lens Person Address
betterAddressLens fn person = fmap (\newAddy -> person { address = newAddy }) (fn $ address person)

-- now we know the format, do the same sorta thing to go from an Address to a city:
betterCityLens :: Lens Address String
betterCityLens fn addy = fmap (\newAddy -> addy { city = newAddy }) (fn $ city addy)

-- and again to go from a Person to a name:
betterNameLens :: Lens Person String
betterNameLens fn person = fmap (\newName -> person { name = newName }) (fn $ name person)
