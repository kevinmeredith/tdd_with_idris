import Data.Vect

datastoreHelper : (acc : Vect n Nat) -> IO String
datastoreHelper acc = do _    <- print "Command: "
                         line <- getLine
                         return line

datastore : IO String
datastore = datastoreHelper []

data Command = Get Nat | Add String | Quit

data Action = Got String | Added Nat | ToQuit

data CommandError = IdDoesNotExist | EmptyAddInput

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

-- processStrInput : (acc : Vect n Nat) -> (cmd : String) -> Either
-- processCommand acc cmd = ?hole
