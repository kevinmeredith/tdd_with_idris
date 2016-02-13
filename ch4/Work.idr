data Tree elem = Empty
                | Node (Tree elem) elem (Tree elem)

-- left decreasing, right increasing
insert : Ord a => a -> Tree a -> Tree a
insert x Empty               = Node Empty x Empty
insert x (Node left y right) = case compare x y of
                                GT => Node left y (insert x right)
                                LT => Node (insert x left) y right
                                EQ => Node left y right

listToTreeHelper : Ord a => List a -> Tree a -> Tree a
listToTreeHelper [] tree        = tree
listToTreeHelper (x :: xs) tree = listToTreeHelper xs (insert x tree)

listToTree : Ord a => List a -> Tree a
listToTree xs = listToTreeHelper xs Empty

treeToList : Tree a -> List a
treeToList Empty               = []
treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right
