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

-- got skeleton from book, and then figured out implementation
transpose_helper : (x : Vect n elem) -> (xs_trans : Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
transpose_helper [] []               = []
transpose_helper (y :: ys) (z :: zs) =  (y :: z) :: (transpose_helper ys zs)

-- [[1,2], [4,5], [7,8]] : 3 x 2
-- [[1,4,7], [2,5,8]]    : 2 x 3

zippy : Vect n elem -> Vect n elem -> Vect n (Vect 2 elem)
zippy x y = zipWith (\a, b => [a, b]) x y

-- autor: TDD with Idris
transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat []      = create_empties
transpose_mat (x::xs) = let xs_trans = transpose_mat xs in
                        transpose_helper x xs_trans

-- foo : (n : Nat) -> Vect (S n) a -> Vect (S n) a
-- foo n xs = xs

headOrEmpty : Vect n a -> Maybe a
headOrEmpty []       = Nothing
headOrEmpty (x :: _) = Just x

f : Vect m a -> Vect m a
f [] = []
f xs = xs

create_empties_foo : Vect n (Vect 0 a)
create_empties_foo {n = Z} = []
create_empties_foo {n = (S k)} = [] :: create_empties_foo

-- f : Vect (S Z) a -> Vect (S Z) a
-- f x = x
