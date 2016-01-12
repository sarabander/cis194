{-# OPTIONS_GHC -Wall #-}

{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative
import           Control.Monad (void) -- void :: Functor f => f a -> f ()
import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Ex. 1

first :: (a -> b) -> (a,c) -> (b,c)
first g (x, y) = (g x, y)

instance Functor Parser where
  fmap g (Parser h) = Parser (fmap (fmap (first g)) h)

-- Ex. 2

instance Applicative Parser where
  pure x = Parser (\str -> Just (x, str))
  p1 <*> p2 = Parser f where
    f str = case runParser p1 str of
      Nothing -> Nothing
      Just (g, xs) -> case runParser p2 xs of
        Nothing -> Nothing
        Just (v, ys) -> Just (g v, ys)

type Name = String
data Employee = Emp { name :: Name, phone :: String } deriving Show

-- Example of extracting some info from a string:
st :: Parser [Char]
st = (\x y -> [x] ++ [y]) <$> char 's' <*> char 't' 

eel :: Parser [Char]
eel = (\x y z -> [x] ++ [y] ++ [z]) <$> char 'e' <*> char 'e' <*> char 'l'

ph :: Parser String
ph = show <$> posInt

--runParser (Emp <$> eel <*> ph) "eel2507914factory"
-- > Just (Emp {name = "eel", phone = "2507914"},"factory")

--phone . fst <$> runParser (Emp <$> eel <*> ph) "eel2507914factory"
-- > Just "2507914"

-- A parser for three-letter names beginning with capital letter:
parseName :: Parser Name
parseName = (\a b c -> [a] ++ [b] ++ [c]) <$>
            satisfy isUpper <*> satisfy isLetter <*> satisfy isLetter
             
-- A parser for phone numbers containing one dash:
parsePhone :: Parser String
parsePhone = (\a b c -> a ++ b ++ c) <$>
             (show <$> posInt) <*>
             ((:[]) <$> char '-') <*>
             (show <$> posInt)

--runParser (Emp <$> parseName <*> parsePhone) "Max449-6200desert"
-- > Just (Emp {name = "Max", phone = "449-6200"},"desert")

-- Ex. 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- runParser abParser "absolve"
-- > Just (('a','b'),"solve")

-- runParser abParser "aebcdf"
-- > Nothing

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\m _ n -> [m,n]) <$> posInt <*> char ' ' <*> posInt

-- Ex. 4

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  p1 <|> p2 = Parser f where
    f str = runParser p1 str <|> runParser p2 str

-- Ex. 5

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)

