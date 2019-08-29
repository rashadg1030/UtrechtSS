module TL.SmoothPermsTree where


data PermTree a = PermLeaf | PermBranch a [PermTree a]
    deriving (Eq)

listToPermTree :: [a] -> PermTree a
listToPermTree []     = PermLeaf
listToPermTree (x:xs) = PermBranch x $ [listToPermTree xs]

permTreeToPerms :: PermTree a -> PermTree a
permTreeToPerms PermLeaf             = PermLeaf
permTreeToPerms (PermBranch val [])  = PermBranch val [PermLeaf]
--permTreeToPerms (PermBranch val pts) = PermBranch

testListToPermTree :: Bool
testListToPermTree = listToPermTree [1,2,3,4,5] == permTree
  where
    permTree :: PermTree Int
    permTree = PermBranch 1 [PermBranch 2 [PermBranch 3 [PermBranch 4 [PermBranch 5 [PermLeaf]]]]]




-- Define a data type PermTree to represented a permutation tree.
-- Define a function listToPermTree which maps a list onto this tree.
-- Define a function permTreeToPerms which generates all permutations represented by a tree.

-- At this point the perms functions given above should be the composition of listToPermTree and permTreeToPerms.

-- Define a function pruneSmooth, which leaves only smooth permutations in the tree.
-- Redefine the function smoothPerms.
