{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module MTA.Parser where

import Control.Monad


newtype ErrorMsg = ErrorMsg String
newtype Parser a = Parser (String -> Either ErrorMsg (a, String))

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Monad Parser where
    return :: a -> Parser a
    return x = Parser $ \s -> Right (x, s)
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser $ \s -> case p s of
                                          Left errMsg     -> Left errMsg
                                          Right (val, s') -> case f val of
                                              Parser p' -> p' s'
