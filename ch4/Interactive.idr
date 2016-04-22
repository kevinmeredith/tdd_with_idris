import Data.Vect
import Data.String

datastoreHelper : (acc : Vect n Nat) -> IO String
datastoreHelper acc = do _    <- print "Command: "
                         line <- getLine
                         return line

datastore : IO String
datastore = datastoreHelper []

data Command = Get Nat | Add String | Quit
data Action = Got String | Added Nat (Vect n String) | ToQuit
data CommandError = IdDoesNotExist | EmptyAddInput
data ParseError = GetArgNotNat String | FailedParse String
data MainError = PWrap ParseError | CWrap CommandError

addHelper : (xs: Vect n String) -> (x : String) -> Action
addHelper {n} xs x = Added (n+1) (xs ++ [x])

getIndexHelper : {n : Nat} -> (xs : Vect n a) -> (m : Nat) -> Maybe a
getIndexHelper {n} xs m = natToFin m n >>= \fin => return $ index fin xs

getIndex : (xs : Vect n String) -> (m : Nat) -> Either CommandError Action
getIndex xs m = case getIndexHelper xs m of
                Just result => Right $ Got result
                Nothing     => Left IdDoesNotExist

processCommand : (acc : Vect n String) -> (c : Command) -> Either CommandError Action
processCommand acc (Get m) = getIndex acc m
processCommand _ Quit      = Right ToQuit
processCommand _ (Add n)   = ?hole

-- TODO : String => Maybe Nat
-- https://groups.google.com/forum/#!topic/idris-lang/rYfdJ5jSUpY
processStrInput : (acc : Vect n Nat) -> (cmd : String) -> Either MainError Action
processStrInput acc cmd = case unpack cmd of
                          'g'::'e'::'t'::' '::xs => getCmdHelper acc xs
                          'a'::'d'::'d'::' '::xs => getAddHelper acc xs
                          invalid                => Left $ FailedParse invalid

getCmdHelper : (acc : Vect n String) -> (x : String) -> Either MainError Action
getCmdHelper x = case parsePositive x of
                  Nothing => Left $ PWrapper $ GetArgNotNat x
                  Just n  => processCommand acc (Get n)

getCmdHelper : (acc : Vect n String) -> (x : String) -> Action
getCmdHelper x = addHelper acc x
