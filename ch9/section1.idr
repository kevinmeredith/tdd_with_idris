-- 1. Data.List includes a version of Elem for List that works similarly to Elem for Vect.
--    How would you define it?
data MyElem : a -> List a -> Type where
   MyHere  : MyElem x (x :: xs)
   MyThere : (later : MyElem x xs) -> MyElem x (y :: xs)

data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

last123 : Last [1,2,3] 3
last123 = LastCons( LastCons LastOne)

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast []      value = No ?a
isLast (x::[]) value = case decEq x value of
                         Yes prf => Yes LastOne
                         No x    => ?b
