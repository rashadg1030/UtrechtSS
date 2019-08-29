{-# LANGUAGE InstanceSigs #-}

module MTA.Teletype where

import Prelude hiding (getLine)


data Teletype a = End a
                | Get (Char -> Teletype a)
                | Put Char (Teletype a)

echo :: Teletype String
echo = Get (\c -> Put c echo)

-- Exercise 1. Write a Teletype-program getLine which reads characters until it finds a newline character,
-- and returns the complete string.

-- getLine :: Teletype String
-- getLine = Get (\c -> if isNewline c then End else Put c getLine )

isNewline :: Char -> Bool
isNewline '\n' = True
isNewline _    = False

instance Functor Teletype where
    fmap f (End x)   = End (f x)
    fmap f (Get g)   = Get (fmap f . g)
    fmap f (Put c x) = Put c (fmap f x)

-- Exercise 2. Define sensible Applicative and Monad instances for Teletype.

instance Applicative Teletype where
    pure :: a -> Teletype a
    pure x = End x

    (<*>) :: Teletype (a -> b) -> Teletype a -> Teletype b
    End f <*> End x       = End (f x)
    Get f <*> Get g       = Get (\c -> f c <*> g c)
    Put c t <*> Put c' t' = Put c $ t <*> t'

instance Monad Teletype where
    return = pure

    (>>=) :: Teletype a -> (a -> Teletype b) -> Teletype b
    End x >>= f = f x

