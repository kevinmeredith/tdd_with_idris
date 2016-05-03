data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

implementation (Eq elem) => Eq (Tree elem) where
   (==) Empty           Empty         = True
   (==) (Node l1 a r1) (Node l2 b r2) = a == b && l1 == l2 && r1 == r2
   (==) _               _             = False

  --  foldr : Foldable t => (elem -> acc -> acc) ->
  --          acc -> t elem -> acc

implementation Foldable Tree where
  foldr _ acc Empty               = acc
  foldr f acc (Node left e right) = foldr f (foldr f (f e acc) left) right
