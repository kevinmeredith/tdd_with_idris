import Data.Vect

rev : Vect n a -> Vect n a
rev []        = []
rev (x :: xs) = rev xs ++ [x]