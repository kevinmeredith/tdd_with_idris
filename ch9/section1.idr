-- 1. Data.List includes a version of Elem for List that works similarly to Elem for Vect.
--    How would you define it?
data MyElem : a -> List a -> Type where
   MyHere  : MyElem x (x :: xs)
   MyThere : (later : MyElem x xs) -> MyElem x (y :: xs)


----- separating 1 from 2 since I mix up #1 with the 2nd problem -----

-- 2. Exercise 2
data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

last123 : Last [1,2,3] 3
last123 = LastCons( LastCons LastOne)

Uninhabited (Last [] value) where
  uninhabited LastOne impossible
  uninhabited (LastCons _) impossible

-- credit: https://stackoverflow.com/questions/44865052/implementing-islast-with-idris
notLast : Not (x = value) -> Last [x] value -> Void
notLast prf LastOne      = absurd (prf Refl)
notLast prf (LastCons _) impossible

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast []      value = No absurd
isLast (x::[]) value = case decEq x value of
                         Yes Refl  => Yes LastOne
                         No contra => No (notLast contra)
isLast (x::y::ys) value = ?todo
