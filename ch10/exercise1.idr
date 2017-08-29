
-- Exercise 1
data TakeN : List a -> Type where
  Fewer : TakeN xs
  Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

total
takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z     xs        = Exact []
takeN (S _) []        = Fewer
takeN (S n) (y :: ys) = case takeN n ys of
  Fewer    => Fewer
  Exact zs => Exact (y :: zs)

-- author: TDD / Idris as part of exercise
groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
   groupByN n xs | Fewer = [xs]
   groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest

 halvesHelper : (n : Nat) -> (xs: List a) -> (List a, List a)
 halvesHelper n xs with (takeN n xs)
   halvesHelper n xs             | Fewer        = ([], xs)
   halvesHelper n (n_xs ++ rest) | (Exact n_xs) = (n_xs, rest)

-- Exercise 2
halves : List a -> (List a, List a)
halves xs = halvesHelper ((length xs) `div` 2) xs

-- exploring instantiation of TakeN
x : TakeN [1,2,3]
x = Exact [1]

y : TakeN [1,2,3]
y = Fewer
