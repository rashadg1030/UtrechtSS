{-# LANGUAGE InstanceSigs #-}

module MTA.Trees where

-- | Binary Tree

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf x)           = Leaf $ f x
    fmap f (Node lTree rTree) = Node (f <$> lTree) (f <$> rTree)

instance Applicative Tree where
    pure :: a -> Tree a
    pure x = Leaf x

    (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    Leaf f <*> Leaf x         = Leaf $ f x
    leaf <*> Node lTree rTree = Node (leaf <*> lTree) (leaf <*> rTree)
    Node lTreeF rTreeF <*> leaf = Node (lTreeF <*> leaf) (rTreeF <*> leaf)

instance Monad Tree where
    (>>=) :: Tree a -> (a -> Tree b) -> Tree b
    Leaf x >>= f           = f x
    Node lTree rTree >>= f = Node (lTree >>= f) (rTree >>= f)

    return = pure

-- | Rose Tree

data RoseTree a = RoseNode a [RoseTree a] | RoseLeaf

instance Functor RoseTree where
    fmap :: (a -> b) -> RoseTree a -> RoseTree b
    fmap _ RoseLeaf         = RoseLeaf
    fmap f (RoseNode x rts) = RoseNode (f x) $ [ f <$> rt | rt <- rts ]

instance Applicative RoseTree where
    pure :: a -> RoseTree a
    pure x =  RoseNode x []

    (<*>) :: RoseTree (a -> b) -> RoseTree a -> RoseTree b
    RoseLeaf <*> RoseLeaf            = RoseLeaf
    RoseNode _ _ <*> RoseLeaf        = RoseLeaf
    RoseLeaf <*> RoseNode x rts      = RoseLeaf
    RoseNode f fs <*> RoseNode x rts = RoseNode (f x) [f' <*> rt | f' <- fs, rt <- rts]

instance Monad RoseTree where
    return :: a -> RoseTree a
    return x = RoseNode x []

    (>>=) :: RoseTree a -> (a -> RoseTree b) -> RoseTree b
    RoseLeaf       >>= _ = RoseLeaf
    RoseNode x rts >>= f = case f x of
        RoseNode x' rts' -> RoseNode x' $ rts' <> [rt >>= f | rt <- rts]
        RoseLeaf         -> RoseLeaf
