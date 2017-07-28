import Data.Vect
import Prelude.Monad

data WordState : (guesses_remaining : Nat) ->
                 (letters : Nat) -> Type where
   MkWordState : (word : String)               ->
                 (missing : Vect letters Char) ->
                 WordState guesses_remaining letters

data Finished : Type where
  Lost : (game : WordState 0 (S letters))  -> Finished
  Won  : (game : WordState (S guesses) 0 ) -> Finished

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
  case isValidString (toUpper input) of
    Yes prf   => pure (_ ** prf)
    No contra => putStrLn "Invalid guess" >>= (\_ => readGuess)

processGuess : (letter : Char) ->
               WordState (S guesses) (S letters) ->
               Either (WordState guesses    (S letters))
                      (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) = case isElem letter missing of
                                                    Yes prf   => Right (MkWordState word (removeElem letter missing prf))
                                                    No contra => Left (MkWordState word missing)


game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do
   (_ ** (Letter c)) <- readGuess
   case (processGuess c st) of
     Right r => putStrLn "Right!" >>= (\_ =>
                                    case letters of
                                      Z   => pure (Won r)
                                      S n => game r
                                )
     Left l  => putStrLn "wrong!" >>= (\_ =>
                                           case guesses of
                                              Z   => pure (Lost l)
                                              S n => game l
                                        )
