{-# LANGUAGE InstanceSigs #-}

module MTA.DecisionTree where

import Data.Foldable


data DecisionTree a = Result a | Decision [DecisionTree a]
    deriving (Eq, Show)

instance Monad DecisionTree where
    (>>=) :: DecisionTree a -> (a -> DecisionTree b) -> DecisionTree b
    (Result x) >>= f    = f x
    (Decision ds) >>= f = Decision $ [ d >>= f | d <- ds ]

instance Applicative DecisionTree where
    pure :: a -> DecisionTree a
    pure x = Result x

    (<*>) :: DecisionTree (a -> b) -> DecisionTree a -> DecisionTree b
    Result f <*> Result x    = Result $ f x
    Result f <*> Decision ds = Decision $ map (fmap f) ds
    Decision ds <*> dTree    = Decision $ [ d <*> dTree | d <- ds ]

instance Functor DecisionTree where
    fmap f (Result a) = Result (f a)
    fmap f (Decision ds) = Decision $ map (fmap f) ds

instance Foldable DecisionTree where
    foldMap :: Monoid m => (a -> m) -> DecisionTree a -> m
    foldMap f (Result x)    = f x
    foldMap f (Decision ds) = fold $ [ foldMap f d | d <- ds ]

instance Traversable DecisionTree where
    traverse :: Applicative f => (a -> f b) -> DecisionTree a -> f (DecisionTree b)
    traverse f dTree = sequenceA $ f <$> dTree

testTraverse :: Bool
testTraverse = Just list == traverse someFunc list
  where
    someFunc :: Int -> Maybe Int
    someFunc x = if x < 0 then Nothing else Just x
    list = [1,2,3,4,5]

-- Applicative Laws

dTreeApLawTest :: Bool
dTreeApLawTest = and [test1, test3, test4]
  where
    test1 = apLaw1 dTree1
    test2 = undefined
    test3 = apLaw3 dTreeFunc3 5
    test4 = apLaw4 dTreeFunc3 dTreeFunc4 $ pure 8

apLaw1 :: (Applicative ap, Eq (ap a)) => ap a -> Bool
apLaw1 v = (pure id <*> v) == v   -- Identity

-- dTreeApLaw2 :: (a -> b) -> a -> Bool
-- dTreeApLaw2 f x = (pure f <*> pure x) == (pure (f x)) -- Homomorphism

apLaw3 :: (Applicative ap, Eq (ap b)) => ap (a -> b) -> a -> Bool
apLaw3 u y = (u <*> pure y) == (pure ($ y) <*> u) -- Interchange

apLaw4 :: (Applicative ap, Eq (ap c)) => ap (a -> b) -> ap (b -> c) -> ap a -> Bool
apLaw4 v u w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w)) -- Composition

-- Monad Laws

dTreeMonadLawTest :: Bool
dTreeMonadLawTest = and [test1, test2, test3]
  where
    test1 = monadLaw1 False dTreeFunc1
    test2 = monadLaw2 dTree2
    test3 = monadLaw3 dTree1 dTreeFunc1 dTreeFunc2

monadLaw1 :: (Monad m, Eq (m b)) => a -> (a -> m b) -> Bool
monadLaw1 val k = (return val >>= k) == k val

monadLaw2 :: (Monad m, Eq (m a)) => m a -> Bool
monadLaw2 m = (m >>= return) ==  m

monadLaw3 :: (Monad m, Eq (m c)) => m a -> (a -> m b) -> (b -> m c) -> Bool
monadLaw3 m f g = (m >>= (\x -> f x >>= g)) == ((m >>= f) >>= g)

-- Test Data/Functions

dTree1 :: DecisionTree Bool
dTree1 = Result True

dTree2 :: DecisionTree Int
dTree2 = Decision [Result 0, Decision [Result 1, Result 2]]

dTreeFunc1 :: Bool -> DecisionTree Int
dTreeFunc1 False = Result 0
dTreeFunc1 True  = Result 1

dTreeFunc2 :: Int -> DecisionTree Char
dTreeFunc2 0 = Result 'a'
dTreeFunc2 1 = Result 'b'
dTreeFunc2 2 = Result 'c'
dTreeFunc2 _ = Result 'z'

dTreeFunc3 :: DecisionTree (Int -> Bool)
dTreeFunc3 = Decision [Result even, Decision [Result even, Result odd]]

dTreeFunc4 :: DecisionTree (Bool -> Char)
dTreeFunc4 = Result boolToChar
  where
    boolToChar True  = 'T'
    boolToChar False = 'F'
