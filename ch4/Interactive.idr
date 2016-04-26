import Data.Vect
import Data.String

data Command = Get Nat | Add String | Quit
data Action = Got String (Vect n String) | Added Nat (Vect n String) | ToQuit
data CommandError = IdDoesNotExist
data ParseError = GetArgNotNat String | FailedParse String

implementation Show CommandError where
  show IdDoesNotExist = "IdDoesNotExist"

implementation Show ParseError where
  show (GetArgNotNat x) = "GetArgNotNat " ++ x
  show (FailedParse xs) = "FailedParse " ++ xs

processCommandAddHelper : (xs: Vect n String) -> (x : String) -> Action
processCommandAddHelper {n} xs x = Added (n+1) (xs ++ [x])

getIndex : {n : Nat} -> (xs : Vect n a) -> (m : Nat) -> Maybe a
getIndex {n} xs m = natToFin m n >>= \fin => return $ index fin xs

processCommandGetHelper : (acc : Vect n String) -> (m : Nat) -> Either CommandError Action
processCommandGetHelper acc m = case getIndex acc m of
                Just result => Right $ Got result acc
                Nothing     => Left IdDoesNotExist

intToNat : (i : Integer) -> Maybe Nat
intToNat i = case i `compare` 0 of
              LT => Nothing
              _  => Just $ cast i

processCommand : (acc : Vect n String) -> (c : Command) -> Either CommandError Action
processCommand acc (Get m)  = processCommandGetHelper acc m
processCommand _   Quit     = Right ToQuit
processCommand acc (Add xs) = Right $ processCommandAddHelper acc xs

processInputHelper : (xs : String) -> Either ParseError Command
processInputHelper xs = case (parsePositive xs >>= \i => intToNat i) of
                         Nothing => Left $ GetArgNotNat xs
                         Just n  => Right $ Get n

-- Given a String, return either an error or command to execute
processInput : (cmd : String) -> Either ParseError Command
processInput cmd = case unpack cmd of
                        'g'::'e'::'t'::' '::xs => processInputHelper $ pack xs
                        'a'::'d'::'d'::' '::xs => Right $ Add $ pack xs
                        'q'::'u'::'i'::'t'::[] => Right Quit
                        _                      => Left $ FailedParse cmd

datastoreHelper : (acc : Vect n String) -> IO ()
datastoreHelper acc = do _         <- printLn "Command: "
                         line      <- getLine
                         processed <- return $ processInput line
                         case processed of
                            Right cmd       => case processCommand acc cmd of
                                                Right (Got str newAcc) => printLn str *> datastoreHelper newAcc
                                                Right (Added n newAcc) => printLn n   *> datastoreHelper newAcc
                                                Right ToQuit           => printLn "good bye!"
                                                Left commandError      => printLn commandError *> datastoreHelper acc
                            Left parseError => printLn parseError *> datastoreHelper acc

datastore : IO ()
datastore = datastoreHelper []
