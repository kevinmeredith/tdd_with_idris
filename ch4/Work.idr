import Data.Vect
import Data.String

data Tree elem = Empty
                | Node (Tree elem) elem (Tree elem)

-- left decreasing, right increasing
insert : Ord a => a -> Tree a -> Tree a
insert x Empty               = Node Empty x Empty
insert x (Node left y right) = case compare x y of
                                GT => Node left y (insert x right)
                                LT => Node (insert x left) y right
                                EQ => Node left y right

listToTreeHelper : Ord a => List a -> Tree a -> Tree a
listToTreeHelper [] tree        = tree
listToTreeHelper (x :: xs) tree = listToTreeHelper xs (insert x tree)

listToTree : Ord a => List a -> Tree a
listToTree xs = listToTreeHelper xs Empty

treeToList : Tree a -> List a
treeToList Empty               = []
treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right

data Expr = Number Int
            | Add Expr Expr
            | Sub Expr Expr
            | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Number x)        = x
evaluate (Add left right)  = (evaluate left) + (evaluate right)
evaluate (Sub left right)  = (evaluate left) - (evaluate right)
evaluate (Mult left right) = (evaluate left) * (evaluate right)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing _         = Nothing
maxMaybe _ Nothing         = Nothing
maxMaybe (Just x) (Just y) = if (x > y) then Just x else Just y

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource  -> Type where
  Bicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  ElectricCar : (charge : Nat) -> Vehicle Electric
  Bus : (fuel : Nat) -> Vehicle Petrol
  Unicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol

wheels : (Vehicle _) -> Nat
wheels Bicycle         = 2
wheels (Car _)         = 4
wheels (Bus _)         = 4
wheels (ElectricCar _) = 4
wheels Unicycle        = 1
wheels (Motorcycle _ ) = 2

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 100

recharge : Vehicle Electric -> Vehicle Electric
recharge (ElectricCar _) = ElectricCar 100

data MyVect : Nat -> Type -> Type where
  Nil : MyVect Z a
  (::) : (x : Nat) -> MyVect k a -> MyVect (S k) a

append : Vect n a -> Vect m a -> Vect (n+m) a
append [] ys        = ys
append (x :: xs) ys = x :: append xs ys

-- zip' : Vect n a -> Vect n b -> Vect n (a, b)
-- zip' [] _                = []
-- zip' (x :: xs) (y :: ys) = (x, y) :: zip' xs ys

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} m xs = case integerToFin m n of
                      Nothing => Nothing
                      Just x  => Just $ index x xs

emptyVect : (n : Nat) -> a -> Vect n a
emptyVect n x = replicate n x

takeVect : (n : Nat) -> Vect (n + m) a -> Vect n a
takeVect Z     _         = []
takeVect (S k) (x :: xs) = x :: (takeVect k xs)

foo : (Fin n) -> Nat
foo FZ      = 0
foo (FS k)  = 1 + foo k

sumEntriesHelper : Num a => (Fin n) -> Vect n a -> Vect n a -> a
sumEntriesHelper fin xs ys = (index fin xs) + (index fin ys)

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                        Just fin => Just $ sumEntriesHelper fin xs ys
                        Nothing  => Nothing

getIndex : {n : Nat} -> (xs : Vect n a) -> (m : Nat) -> Maybe a
getIndex {n} xs m = natToFin m n >>= \fin => return $ index fin xs

g : String -> Maybe Int
g x = parsePositive x >>= \pos => return pos

-- data FooBarAge : Type -> Nat -> Type where
--   Age : {n = 100} -> (Fin n) => Nat -> FooBarAge (Fin n)

-- data FooBarAge : Type where
--   Age : (age : Nat) -> .(ok : LTE age 100) -> FooBarAge
