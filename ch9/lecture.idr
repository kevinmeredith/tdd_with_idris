import Data.Vect

-- removeElem : DecEq a => (value : a) -> (xs : Vect (S n) a) -> Vect n a
-- removeElem value (x :: xs) = case decEq value x of
--                               Yes prf   => xs
--                               No contra => x :: removeElem value xs

data MyElem : a -> Vect k a -> Type where
   MyHere  : MyElem x (x :: xs)
   MyThere : (later : MyElem x xs) -> MyElem x (y :: xs)

oneInVector : MyElem 1 [1,2,3]
oneInVector = MyHere

marryInVector : MyElem "Mary" ["Peter", "Paul", "Mary"]
marryInVector = MyThere (MyThere MyHere)

removeElem : (value : a) -> (xs : Vect (S n) a) -> (prf : Elem value xs) -> Vect n a
removeElem value (value :: ys) Here                  = ys
removeElem {n = Z} value (y :: []) (There later)     = absurd later
removeElem {n = (S k)} value (y :: ys) (There later) = y :: (removeElem value ys later)

removeElem_auto : (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
removeElem_auto value xs {prf} = removeElem value xs prf

notInTail : (notThere : Elem value xs -> Void) ->
            (notHere : (value = x) -> Void)    ->
            Elem value (x :: xs) -> Void
notInTail notThere notHere Here          = notHere Refl
notInTail notThere notHere (There later) = notThere later

isElem' : DecEq ty => (value : ty) -> (xs : Vect n ty) -> Dec (Elem value xs)
isElem' value []      = No absurd
isElem' value (x::xs) = case (decEq value x) of
                         Yes Refl   => Yes Here
                         No notHere => case isElem' value xs of
                                       Yes prf     => Yes (There prf)
                                       No notThere => No (notInTail notThere notHere)

stringOrInt : Bool -> Type
stringOrInt True = String
stringOrInt False = Int

f : (b : Bool) -> (stringOrInt b) -> String
f True  x = x
f False x = show x
