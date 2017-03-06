import Data.Vect

rev : Vect n a -> Vect n a
rev []                  = []
rev {n = S k} (x :: xs) = let result = rev xs ++ [x] in
                          rewrite plusCommutative 1 k in result

myReverse : Vect n a -> Vect n a
myReverse []        = []
myReverse (x :: xs) = reverseProof x xs (myReverse xs ++ [x])
  where
    reverseProof : (x : a) -> (xs : Vect len a) -> Vect (len + 1) a -> Vect (S len) a
    reverseProof {len} _ _ result = rewrite plusCommutative 1 len in result

-- http://stackoverflow.com/questions/42626914/writing-append-in-idris
append : (xs : Vect m a) -> (ys : Vect n a) -> Vect (m + n) a
append xs ys = appendProof xs ys (xs ++ ys)
  where
    appendProof : (xs : Vect m a) -> (ys : Vect n a) -> Vect (m + n) a -> Vect (plus n m) a
    appendProof {m} {n} xs ys result = rewrite plusCommutative m n in result
