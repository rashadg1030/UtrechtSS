{-# LANGUAGE InstanceSigs #-}

module Selective where

class Applicative f => Selective f where
    select :: f (Either a b) -> f (a -> b) -> f b

instance Selective Maybe where
    select :: Maybe (Either a b) -> Maybe (a -> b) -> Maybe b
    select Nothing Nothing        = Nothing
    select (Just either) (Just f) = case either of
        Left  a -> Just (f a)
        Right b -> Just b
    select (Just either) Nothing  = case either of
        Left _  -> Nothing
        Right b -> Just b

whenS :: Selective f => f Bool -> f () -> f ()
whenS = undefined

branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
branch = undefined


type ModuleName = String

type Path = String

data Build a = Build a

data Action = Action

ocamlc :: ModuleName -> Action
ocamlc = undefined

ocamlDep :: ModuleName -> Build [Path]
ocamlDep = undefined

compileModule :: ModuleName -> [ModuleName] -> Build Action
compileModule = undefined
