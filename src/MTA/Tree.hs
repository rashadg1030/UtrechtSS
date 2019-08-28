module MTA.Trees where

data Tree a = Leaf a | Node (Tree a) (Tree a)

data RoseTree a = RoseNode a [RoseTree a] | RoseLeaf
