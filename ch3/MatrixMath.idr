module MatrixMath

import Data.Vect

-- improvement by Cactus (http://stackoverflow.com/q/34889808/409976)
addHelper : (Num n) => Vect k n -> Vect k n -> Vect k n
addHelper = zipWith (+)

-- (Vect 2 (Vect 3 Int)) = [[1,2,3], [4,5,6]]

headHelper : Vect (S n) a -> a
headHelper (x :: _) = x

tailHelper : Vect (S n) a -> Vect n a
tailHelper (_ :: xs) = xs

zerosVect : (n : Nat) -> Vect n (Vect 0 a)
zerosVect Z     = []
zerosVect (S n) = [] :: zerosVect n

create_empties : Vect n (Vect 0 elem)
create_empties = replicate _ []

-- [[1,2], [4,5]]

-- [1,2]  [[4], [5]]

-- [1,2,3]    [[4,7], [5,8], [6,9]]

myZip : (x : Vect n elem) -> (xs: Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
myZip []        _         = ?fff --create_empties
myZip (y :: ys) (z :: zs) = (y :: z) :: myZip ys zs

transpose_helper : (x : Vect n elem) -> (xs : Vect k (Vect n elem)) ->
            (xs_trans : Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
transpose_helper x []      xs_trans = ?hole -- map (::) x $ xs_trans
transpose_helper x (y::ys) xs_trans = ?hole2 -- transpose_helper y ys $ map (::) x $ xs_trans

-- [[1,2], [4,5], [7,8]] : 3 x 2
-- [[1,4,7], [2,5,8]]    : 2 x 3

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat []      = create_empties
transpose_mat (x::xs) = let xs_trans = transpose_mat xs in
                        transpose_helper x xs xs_trans

-- foo : (n : Nat) -> Vect (S n) a -> Vect (S n) a
-- foo n xs = xs

headOrEmpty : Vect n a -> Maybe a
headOrEmpty []       = Nothing
headOrEmpty (x :: _) = Just x

f : Vect m a -> Vect m a
f [] = []
f xs = xs

-- f : Vect (S Z) a -> Vect (S Z) a
-- f x = x
