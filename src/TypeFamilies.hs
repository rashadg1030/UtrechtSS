{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module TypeFamilies where

-- functional dependencies are also used in DB terminology
-- class Summable m n s | m n -> s where
--     makeSum :: Sum m n s

-- instance Summable Zero n n where
--     makeSum = SumZero

-- instance Summable m n s => Summable (Succ m) n (Succ s) where
--     makeSum = SumSucc makeSum

data Zero
data Succ n

data Nat = Zero | Succ Nat

-- Associated Types

-- class Collects c where
--     type Elem c
--     empty :: c
--     insert :: Elem c -> c -> c
--     toList :: Elem c -> [c]

-- class Summable m n where
--     type TheSum m n
--     makeSum :: Sum m n (TheSum m n)

-- instance Summable Zero n where
--     type TheSum Zero n = n

-- instance Summable m n => Summable (Succ m) n where
--     type TheSum (Succ m) n = Succ (TheSum m n)

-- Open vs. Closed type families

-- MultiParam Typeclasses
-- FunctionalDependencies
-- TypeFamilies
-- FlexibleInstances
-- UndecideableInstances
-- IncoherentInstances!!!
-- DataKinds

-- Kind polymorphism

type family Sum m n where
    Sum Zero     n = n
    Sum (Succ m) n = Succ (Sum m n)

-- Data Kinds
-- Haskell In-Depth
