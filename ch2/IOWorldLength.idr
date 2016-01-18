-- credit: TDD with Idris
module Average

average : String -> Double
average str = let num_words = word_count str
                  total_length = sum (word_lengths (words str)) in
                  cast total_length / cast num_words
   where
    word_count : String -> Nat
    word_count str = length (words str)

    word_lengths : List String -> List Nat
    word_lengths strs = map length strs

showAverage : String -> String
showAverage str = "The average word length of " ++ str ++ "is " ++ (show $ average str)

main : IO ()
main = repl "Enter String." showAverage
