-- 1. Data.List includes a version of Elem for List that works similarly to Elem for Vect.
--    How would you define it?
data MyElem : a -> List a -> Type where
   MyHere  : MyElem x (x :: xs)
   MyThere : (later : MyElem x xs) -> MyElem x (y :: xs)

-- 2. Exercise 2
data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

last123 : Last [1,2,3] 3
last123 = LastCons( LastCons LastOne)

Uninhabited (Last [] value) where
  uninhabited LastOne impossible
  uninhabited (LastCons _) impossible

notInLast : (Not (x = value)) ->  Last [x] value -> Void
notInLast prf LastOne impossible
notInLast prf (LastCons later) impossible

notInLast2 : (Not (y = value)) -> Last [x, y] value -> Void
notInLast2 prf LastOne impossible
notInLast2 prf (LastCons later) impossible


isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast []      value = No absurd
isLast (x::[]) value = case decEq x value of
                         Yes Refl => Yes LastOne
                         No prf   => No (notInLast prf)
isLast (y::ys) value = case decEq y value of
                            Yes Refl => case ys of
                                            [] => Yes LastOne
                                            as => isLast as value
                            No notHead => case isLast ys value of
                                            Yes there => Yes (LastCons there)
                                            No notTail => ?b
