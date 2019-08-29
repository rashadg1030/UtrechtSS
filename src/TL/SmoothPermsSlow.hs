module TL.SmoothPermsSlow where


split []     = []
split (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- split xs]

perms []     = [[]]
perms xs     = [(v:p) | (v, vs) <- split xs, p <- perms vs]

smooth n (x:y:ys) = abs (y - x) < n && smooth n (y:ys)
smooth _ _        = True

smoothPerms :: Int -> [Int] -> [[Int]]
smoothPerms n xs = filter (smooth n) (perms xs)
