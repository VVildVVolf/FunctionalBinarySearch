data Tree a = Tree (Tree a) a (Tree a) | Empty deriving Show
data Comparator = Less | Bigger | Success

search :: Tree a -> (a -> Comparator) -> Maybe a
search Empty _ = Nothing
search (Tree left val right) f = case f val of
    Success -> Just val
    Less -> search left f
    _ -> search right f

treeToStr :: (Show a) => Tree a -> String
treeToStr tr = treeToStr' 0 tr where
    treeToStr' _ Empty = ""
    treeToStr' n (Tree a b c) = (treeToStr' (succ n) c) ++ (concat $ replicate n "    ") ++ (show b) ++ "\n" ++ (treeToStr' (succ n) a)

fromList :: [a] -> Tree a
fromList list = createTreeUntilPossible 0 (Empty, list) where
    createTree _ [] = (Empty, [])
    createTree 0 xs = (Empty, xs)
    createTree 1 (x:xs) = (Tree Empty x Empty, xs)
    createTree h (xs) = let
        (lt, lr) = createTree (h-1) xs
        val = head lr
        isEmpty = case lr of
            [] -> True
            _ -> False
        (rt, rr) = createTree (h - 1) (tail lr)
        in if isEmpty then (lt, lr) else (Tree lt (head lr) rt, rr)

    createTreeUntilPossible _ (left, []) = left
    createTreeUntilPossible n (left, (x:rest)) = let
        (right, rrest) = createTree n rest
        nextRight = Tree left x right
        in createTreeUntilPossible (succ n) (nextRight, rrest)
