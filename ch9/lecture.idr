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

removeElem : (value : a) -> (xs : Vect (S n) a) -> (prf : MyElem value xs) -> Vect n a
removeElem value (value :: ys) MyHere         = ys
removeElem value (y :: ys)    (MyThere later) = removeElem value (y :: ys) (MyThere later)
