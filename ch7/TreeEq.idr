data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

implementation (Eq elem) => Eq (Tree elem) where
   (==) Empty           Empty         = True
   (==) (Node l1 a r1) (Node l2 b r2) = a == b && l1 == l2 && r1 == r2
   (==) _               _             = False
