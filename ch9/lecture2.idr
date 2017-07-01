import Data.Vect

data WordState : (guesses_remaining : Nat) ->
                 (letters : Nat) -> Type where
   MkWordState : (word : String)               ->
                 (missing : Vect letters Char) ->
                 WordState guesses_remaining letters

data Finished : Type where
  Lost : (game : WordState 0 (S letters))  -> Finished
  Won  : (game : WordState (S guesses) 0 ) -> Finished

game : WordState (S guesses) (S letters) -> IO Finished
game st = ?game_rhs

data ValidInput : List Char -> Type where
   Letter : (c : Char) -> ValidInput [c]

isValidNil : ValidInput [] -> Void
isValidNil (Letter _) impossible

isValidTwo : ValidInput (x :: y :: xs) -> Void
isValidTwo (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput []             = No isValidNil
isValidInput (x :: [])      = Yes (Letter x)
isValidInput (x :: y :: xs) = No isValidTwo

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

removeElem : (value : a) -> (xs : Vect (S n) a) -> (prf : Elem value xs) -> Vect n a
removeElem value (value :: ys) Here                  = ys
removeElem {n = Z} value (y :: []) (There later)     = absurd later
removeElem {n = (S k)} value (y :: ys) (There later) = y :: (removeElem value ys later)

readGuess : IO (x ** ValidInput x)
readGuess = do
  input <- getLine
  case isValidString input of
    Yes prf   => ?yes_rhs
    No contra => do putStrLn "Invalid guess"
                    readGuess

processGuess : (letter : Char) ->
               WordState (S guesses) (S letters) ->
               Either (WordState guesses (S letters))
                      (WordState guesses letters)
processGuess letter (MkWordState word missing) = case isElem letter missing of
                                                    Yes prf   => Right (MkWordState word (removeElem letter missing prf))
                                                    No contra => Left (MkWordState word missing)
