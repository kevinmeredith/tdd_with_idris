word_lengths : List String -> List Nat
word_lengths []        = []
word_lengths (x :: xs) = length x :: ?word_lengths_rhs
