module Exercises

import Data.Vect

-- Exercise 1: Reimplement transpose_mat using zipWith instead of
--              transpose_helper.
-- transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
-- transpose_mat []        = replicate _ []
-- transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in
--                           zipWith (\a, b -> a :: b) x xs_trans

-- Exercise 2
addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix []        []        = []
addMatrix (x :: xs) (y :: ys) = (zipWith (+) x y) :: addMatrix xs ys

-- [1,2,3] || [[7,9,11], [8,10,12]]

helper : Num a => Vect n a -> Vect p (Vect n a) -> Vect p a
helper _ [] = []
helper x (y :: ys)  = (sum $ zipWith (*) x y) :: helper x ys

multHelper : Num a => Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
multHelper [] _         = []
multHelper (x :: xs) ys = (helper x ys) :: multHelper xs ys

-- matrix mult sizes
-- (n x m) X (m x p) === (n x p)

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [] [] = []
multMatrix xs ys = multHelper xs (transpose ys)
