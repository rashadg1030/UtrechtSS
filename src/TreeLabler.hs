{-# LANGUAGE FlexibleContexts #-}

module TreeLabler where

import State'


data Tree a = Branch (Tree a) a (Tree a) | Leaf
    deriving (Eq, Show)

-- Labels a tree with integers increasingly, using a depth-first in-order traversal.
label :: MonadState m Int => Tree a -> m (Tree (Int, a))
label Leaf                         = return Leaf
label (Branch lBranch val rBranch) = do
    newLBranch <- label lBranch
    num <- get
    put (num + 1)
    newRBranch <- label rBranch
    return $ Branch newLBranch (num, val) newRBranch

run :: State' s a -> s -> (a, Counts)
run (State' rs) initial = let (a, _, counts) = rs (initial, mempty) in (a, counts)
