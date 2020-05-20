{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char
import           Data.Maybe
import           Data.Monoid

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


first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)


-- We need a function String -> Maybe (a, String ) -> Maybe(f a, String)
instance Functor Parser where
--  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser g) = Parser $ fmap (first f) . g  

-- Type of p1 String -> Maybe (a -> b, String)
-- Type of p2 String -> Maybe (a, String)
-- Type of p1 <*> p2 String -> Maybe (b, String)
instance Applicative Parser where
  pure f = Parser (\str -> Just (f, str))
  (Parser p1) <*> (Parser p2) = Parser (\str -> do 
						(f, str1) <- p1 str	
						fmap (first f) (p2 str1)
					)

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'


null1 = const ()
null2 = const . null1
abParser_:: Parser ()
abParser_ = null2 <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt


instance Alternative Parser where 
	empty = Parser (\_ -> Nothing)
	Parser p1 <|> Parser p2 = Parser $ fmap ((<|>) p1) <*> p2


upperParser :: Parser Char
upperParser = satisfy isUpper

intOrUppercase = (null1 <$> upperParser) <|> (null1 <$> posInt)

{-case p2 str1 of
						Nothing -> Nothing
						Just (b, str2) -> Just (f b, str2)
-}

