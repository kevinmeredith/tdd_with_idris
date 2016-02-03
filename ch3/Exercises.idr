module Exercises

import Data.Vect

-- Exercise 1: Reimplement transpose_mat using zipWith instead of
--              transpose_helper.
-- transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
-- transpose_mat []        = replicate _ []
-- transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in
--                           zipWith (\a, b -> a :: b) x xs_trans

-- Exerfise 2
addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix []        []        = []
addMatrix (x :: xs) (y :: ys) = (zipWith (+) x y) :: addMatrix xs ys

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix 
