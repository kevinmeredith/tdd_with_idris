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

append_nil : (ys : Vect n a) -> Vect (plus n 0) a
append_nil {n} ys = rewrite plusZeroRightNeutral n in ys

append_xs : Vect (S (m + k)) elem -> Vect (plus m (S k)) elem
append_xs {m} {k} xs = rewrite sym (plusSuccRightSucc m k) in xs

-- http://stackoverflow.com/questions/42626914/writing-append-in-idris
append : (xs : Vect m a) -> (ys : Vect n a) -> Vect (n + m) a
append []        ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)

-- Problem 1
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym (plusZeroRightNeutral m)
