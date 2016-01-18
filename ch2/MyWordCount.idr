-- Given a group of words, i.e. separated by
--  whitespace, return the average length of all words.
wordCount : String -> Double
wordCount input = let numWords    = length (words input)
                      totalLength = sum ( map length numWords )
                  in
                   cast totalLength / cast numWords
