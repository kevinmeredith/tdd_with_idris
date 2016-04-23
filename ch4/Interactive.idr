import Data.Vect
import Data.String

datastoreHelper : (acc : Vect n Nat) -> IO String
datastoreHelper acc = do _    <- print "Command: "
                         line <- getLine
                         return line

datastore : IO String
datastore = datastoreHelper []

data Command = Get Integer | Add String | Quit
data Action = Got String (Vect n String) | Added Nat (Vect n String) | ToQuit
data CommandError = IdDoesNotExist (Vect n String) | IdIsNotNat (Vect n String) | EmptyAddInput (Vect n String)
data ParseError = GetArgNotNat String (Vect n String) | FailedParse String (Vect n String)
data MainError = PWrap ParseError | CWrap CommandError

addHelper : (xs: Vect n String) -> (x : String) -> Action
addHelper {n} xs x = Added (n+1) (xs ++ [x])

getIndexHelper : {n : Nat} -> (xs : Vect n a) -> (m : Nat) -> Maybe a
getIndexHelper {n} xs m = natToFin m n >>= \fin => return $ index fin xs

getIndex : (acc : Vect n String) -> (m : Nat) -> Either MainError Action
getIndex acc m = case getIndexHelper acc m of
                Just result => Right $ Got result acc
                Nothing     => Left $ CWrap $ IdDoesNotExist acc

intToNat : (i : Integer) -> Maybe Nat
intToNat i = case i `compare` 0 of
              LT => Nothing
              _  => Just $ cast i

parseIntegerToNat : (i : Integer) -> (acc : Vect n String) -> Either MainError Action
parseIntegerToNat i acc = case intToNat i of
                            Just n  => getIndex acc n
                            Nothing => Left $ CWrap $ IdIsNotNat acc

processCommand : (acc : Vect n String) -> (c : Command) -> Either MainError Action
processCommand acc (Get m) = parseIntegerToNat m acc
processCommand _ Quit      = Right ToQuit
processCommand _ (Add n)   = ?hole

getCmdHelper : (acc : Vect n String) -> (x : String) -> Either MainError Action
getCmdHelper acc x = case parsePositive x of
                      Nothing => Left $ PWrap $ GetArgNotNat x acc
                      Just n  => processCommand acc (Get n)

-- TODO : String => Maybe Nat
-- https://groups.google.com/forum/#!topic/idris-lang/rYfdJ5jSUpY
processStrInput : (acc : Vect n String) -> (cmd : String) -> Either MainError Action
processStrInput acc cmd = case unpack cmd of
                          'g'::'e'::'t'::' '::xs => getCmdHelper acc $ pack xs
                          'a'::'d'::'d'::' '::xs => Right $ addHelper acc $ pack xs
                          _                      => Left $ PWrap $ FailedParse cmd acc

-- getCmdHelper : (acc : Vect n String) -> (x : String) -> Action
-- getCmdHelper x = addHelper acc x
