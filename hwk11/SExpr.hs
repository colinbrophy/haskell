{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import Data.Char
import AParser
import Control.Applicative

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------
{-
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = undefined

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = undefined
-}
------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------
{-
spaces :: Parser String
spaces = undefined

ident :: Parser String
ident = undefined
-}
------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore  :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p $ zeroOrMore p

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) $ zeroOrMore (satisfy isAlphaNum)

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser SExpr
parseAtom = A <$> ((N <$> posInt) <|> (I <$> ident))
parseSExpr :: Parser SExpr
parseSExpr = parseAtom <|> (Comb <$> ((char '(') *> zeroOrMore (spaces *> parseSExpr <* spaces) <* (char ')')))
