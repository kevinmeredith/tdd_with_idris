module MatrixMath

import Data.Vect

-- improvement by Cactus (http://stackoverflow.com/q/34889808/409976)
addHelper : (Num n) => Vect k n -> Vect k n -> Vect k n
addHelper = zipWith (+)

-- add : (Num n) => Vect rows (Vect cols n) -> Vect rows (Vect cols n) -> Vect rows (Vect col n)
-- add [] []               = []
-- add (x :: xs) (y :: ys) = (addHelper x y) :: (add xs ys)

-- (Vect 2 (Vect 3 Int)) = [[1,2,3], [4,5,6]]

headHelper : Vect (S n) a -> a
headHelper (x :: _) = x

tailHelper : Vect (S n) a -> Vect n a
tailHelper (_ :: xs) = xs

zerosVect : (n : Nat) -> Vect n (Vect 0 a)
zerosVect Z     = []
zerosVect (S n) = [] :: zerosVect n

tpose : (n : Nat) -> Vect m (Vect n a) -> Vect n (Vect m a)
tpose n []                    = zerosVect n
tpose _ ([] :: _)             = []
tpose _ xxs@(x@(_ :: _) :: _) = map headHelper xxs :: map tailHelper xxs

--
-- foo : (n : Nat) -> Vect (S n) a -> Vect (S n) a
-- foo n xs = xs

-- headOrEmpty : Vect n a -> Vect n a
-- headOrEmpty []       = []
-- headOrEmpty (x :: _) = [x]

f : Vect m a -> Vect m a
f [] = []
f xs = xs

-- f : Vect (S Z) a -> Vect (S Z) a
-- f x = x
