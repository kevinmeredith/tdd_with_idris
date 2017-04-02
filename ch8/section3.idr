import Data.Vect

twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible

valueNotSucc  : (x : Nat) -> x = S x -> Void
valueNotSucc _ Refl impossible

zeroNotSucc : (0 = S k) -> Void
zeroNotSucc Refl impossible

succNotZero : (S k = 0) -> Void
succNotZero Refl impossible

noRec : (contra : (j = k) -> Void) -> (S j = S k) -> Void
noRec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z     Z     = Yes Refl
checkEqNat Z    (S k)  = No zeroNotSucc
checkEqNat (S k) Z     = No succNotZero
checkEqNat (S j) (S k) = case (checkEqNat j k) of
                           Yes prf   => Yes (cong prf)
                           No contra => No (noRec contra)

exactLength' : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength'  {m} len input = case decEq m len of
                              Yes Refl  => Just input
                              No contra => Nothing

headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

-- data DoorAction : DoorState -> DoorState -> Type where
--   Open  : DoorAction DClosed DOpen
--   Close : DoorAction DOpen DClosed

data MyVect : (len : Nat) -> (elem : Type) -> Type where
   MyCons  : (x  : elem) -> (xs : MyVect len elem) -> MyVect (S len) elem
   Empty   : MyVect 0 elem

noDecEq : (contra : (x = y) -> Void) -> (MyCons x xs = MyCons y ys) -> Void
noDecEq contra Refl = contra Refl

implementation (DecEq a) => DecEq (MyVect n a) where
  decEq Empty         Empty         = Yes Refl
  decEq (MyCons x xs) (MyCons y ys) = case (decEq x y) of
                                        Yes prf   => decEq xs ys
                                        No contra => No (noDecEq contra)
  decEq _             _             = No ?k

--   decEq : DecEq t => (x1 : t) -> (x2 : t) -> Dec (x1 = x2)
