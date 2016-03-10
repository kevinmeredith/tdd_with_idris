import Data.Vect

printLength : IO ()
printLength = getLine >>= (\result => putStrLn $ show $ length result)

printLengthDo : IO ()
printLengthDo = do x <- getLine
                   putStrLn $ show $ length x

larger : String -> String -> String
larger x y = case (length x) `compare` (length y) of
              LT => y
              GT => x
              EQ => x

biggerOfTwoDo : IO String
biggerOfTwoDo = do putStrLn "Enter String 1"
                   x <- getLine
                   putStrLn "Enter String 2"
                   y <- getLine
                   pure $ larger x y


-- m a -> (a -> m b) -> m b
readNumber : Char -> Maybe Nat
readNumber '1' = Just 1
readNumber '2' = Just 2
readNumber '3' = Just 3
readNumber '4' = Just 4
readNumber '5' = Just 5
readNumber '6' = Just 6
readNumber '7' = Just 7
readNumber '8' = Just 8
readNumber '9' = Just 9
readNumber '0' = Just 0
readNumber _   = Nothing

convertNumbersHelper : List Char -> Maybe (Nat, Nat)
convertNumbersHelper (x :: y :: Nil) = readNumber x >>= (\num1 => (readNumber y >>= \num2 => (Just (num1, num2))))
convertNumbersHelper _               = Nothing

-- not good enough, need `String -> Maybe (Nat)`
stringToListNat : String -> Maybe (List Nat)
stringToListNat str = let chs = (unpack str) in
                      let nats = do c   <- chs
                                    num <- pure $ readNumber c
                                    pure num in
                      sequence nats

convertNumbers : String -> Maybe (Nat, Nat)
convertNumbers xs = convertNumbersHelper $ unpack xs

readNumbers : IO (Maybe (Nat, Nat))
readNumbers = getLine >>= (pure . convertNumbers)

-- guessHelper : List Nat -> Nat -> IO () -> IO ()
-- guessHelper guessed target f = case guessed `compare` target of
--                         EQ => putStrLn "win!" >>= \_ => pure()
--                         LT => putStrLn "too small!" >>= \_ => f
--                         GT => putStrLn "too large!" >>= \_ => f

-- guess : Nat -> IO ()
-- guess target = do xs  <- getLine
--                   num <- pure $ convertNumbers xs
--                   case num of Nothing      => do putStrLn "Enter a valid number"
--                                                  guess target
--                               Just guessed => guessHelper guessed target (guess target)


read_vect_len : (len : Nat) -> IO (Vect len String)
read_vect_len Z = pure []
read_vect_len (S k) = do x <- getLine
                         xs <- read_vect_len k
                         pure (x :: xs)
