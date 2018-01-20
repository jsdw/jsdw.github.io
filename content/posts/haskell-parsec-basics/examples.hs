{-# LANGUAGE FlexibleContexts #-}

-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as Parsec

-- I am the choice and optional error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

-- alias parseTest for more concise usage in my examples:
parse rule text = Parsec.parse rule "(source)" text

-- we can catch success and failure by matching Left or Right
-- from something returned by the parse function:
error_example = do
    let result = parse (Parsec.char 'H') "Hello"
    case result of
        Right _ -> putStrLn "success!"
        Left _ -> putStrLn "whoops, error"

-- ### combining rules ###

-- this looks for letters, then spaces, then digits.
-- we then return letters and digits in a tuple.
myParser :: Parsec.Parsec String () (String,String)
myParser = do
    letters <- Parsec.many1 Parsec.letter
    Parsec.spaces
    digits <- Parsec.many1 Parsec.digit
    return (letters,digits)

-- these type signatures are equivalent:
myParser1 :: Parsec.ParsecT String () Identity (String,String)
myParser1 = myParser

myParser2 :: Parsec.Parsec String () (String,String)
myParser2 = myParser

-- separate by comma (optionally surrounded by spaces)
mySeparator :: Parsec.Parsec String () ()
mySeparator = Parsec.spaces >> Parsec.char ',' >> Parsec.spaces

--the above is the same as:
-- mySeparator = do
--  Parsec.spaces
--  Parsec.char ','
--  Parsec.spaces

-- I want to return a list of pairs, this time.
myPairs :: Parsec.Parsec String () [(String,String)]
myPairs = Parsec.many $ do
    pair <- myParser
    mySeparator
    return pair

-- I want to return a list of pairs with an optional end separator.
myPairs2 :: Parsec.Parsec String () [(String,String)]
myPairs2 = Parsec.many $ do
    pair <- myParser
    Parsec.choice [Parsec.eof, mySeparator]
    return pair

-- I want to return a list of pairs as above using a built in helper:
myPairs2a :: Parsec.Parsec String () [(String,String)]
myPairs2a = Parsec.endBy myParser mySeparator

-- I want to return a list of pairs without a final separator:
myPairs2b :: Parsec.Parsec String () [(String,String)]
myPairs2b = Parsec.sepBy myParser mySeparator

-- The abovem but using the choice infix operator instead
myPairs3 :: Parsec.Parsec String () [(String,String)]
myPairs3 = Parsec.many $ do
    pair <- myParser
    Parsec.eof <|> mySeparator
    return pair

-- parsing either hello or howdy without lookahead:
helloOrHowdy :: Parsec.Parsec String () String
helloOrHowdy = do
    first <- Parsec.char 'h'
    rest <- Parsec.string "ello" <|> Parsec.string "owdy"
    return (first:rest)

-- hello or howdy made more concise by using Parsec.try to catch failure and rewind:
helloOrHowdy2 :: Parsec.Parsec String () String
helloOrHowdy2 = Parsec.try (Parsec.string "hello") <|> Parsec.string "howdy"


-- parse a basic greeting:
greeting :: Parsec.Parsec String () String
greeting = do
    Parsec.char 'h'
    Parsec.string "olla" <|> Parsec.string "ello"
    return "greeting"

--parse a custom greeting with a custom error message on failure:
greeting2 :: Parsec.Parsec String () String
greeting2 = Parsec.try greeting <?> "a greeting!"



-- applicative style:
myParserApp :: Parsec.Parsec String () (String,String)
myParserApp = (,) <$> Parsec.many1 Parsec.letter <*> (Parsec.spaces *> Parsec.many1 Parsec.digit)

-- using basic state in parsers:

-- matches char 'h', incrementing int state by 1
-- each time one is seen.
hCountParser :: Parsec.Parsec String Int ()
hCountParser = do
    Parsec.char 'h'
    c <- Parsec.getState
    let c' = c+1
    Parsec.putState c'
    return ()

hCountParser' :: Parsec.Parsec String Int ()
hCountParser' = do
    Parsec.char 'h'
    Parsec.modifyState (\c -> c+1)
    return ()

-- parse as many h's as we can, then return the state
-- to see how many there were
getHs = Parsec.runParser (Parsec.many hCountParser >> Parsec.getState) 0 "" "hhhhhhhhhhhhellooo"
