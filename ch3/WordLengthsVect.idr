import Data.Vect

word_lengths : Vect len String -> Vect len Nat
word_lengths []      = []
word_lengths (x::xs) = length x :: word_lengths xs
