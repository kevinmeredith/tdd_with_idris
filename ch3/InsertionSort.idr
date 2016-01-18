import Data.Vect

-- Insertion sort - got initial type from TDD in Idris text
insert : (Ord elem) => (x : elem) -> (xs_sorted : Vect k elem) -> Vect (S k) elem
insert x []        = [x]
insert x (y :: xs) = if (x > y) then y :: (insert x xs) else x :: y :: xs

ins_sort : (Ord elem) => Vect n elem -> Vect n elem
ins_sort []        = []
ins_sort (x :: xs) = let xs_sorted = ins_sort xs in
                     insert x xs_sorted
