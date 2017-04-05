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

headEqual : DecEq a => (x : a) -> (y : a) -> Maybe (Dec (x = y))
headEqual x y = case decEq x y of
                  Yes Refl  => Just (Yes Refl)
                  No contra => Nothing

-- vectEqual : DecEq a => (xs : Vect n a) -> (ys : Vect n a) -> Maybe (Dec (xs = ys))
-- vectEqual []         []         = Just (Yes Refl)
-- vectEqual (x :: xxs) (y :: yys) = case headEqual x y of
--                                   Just (Yes prf) => vectEqual xxs yys
--                                   No contra      => Nothing
-- vectEqual (x :: xxs) []         = Nothing
-- vectEqual []         (y :: yys) = Nothing

data MyVect : (len : Nat) -> (elem : Type) -> Type where
   MyCons  : (x  : elem) -> (xs : MyVect len elem) -> MyVect (S len) elem
   Empty   : MyVect 0 elem

noDecEq : (contra : (x = y) -> Void) -> (MyCons x xs = MyCons y ys) -> Void
noDecEq contra Refl = contra Refl

noHead : (contra : (x = y) -> Void) -> (MyCons x xs = MyCons y ys) -> Void
noHead contra Refl = contra Refl

noTail : (contra : (xs = ys) -> Void) -> (MyCons x xs = MyCons x ys) -> Void
noTail contra Refl = contra Refl

yesTail : (prf : xs = ys) -> Dec (MyCons x xs = MyCons x ys)
yesTail Refl = Yes Refl

-- http://stackoverflow.com/questions/43207417/understanding-deceq
implementation (DecEq a) => DecEq (MyVect n a) where
  decEq Empty         Empty                         = Yes Refl
  decEq (MyCons x xs) (MyCons y ys)   with (decEq x y)
    decEq (MyCons x xs) (MyCons x ys) |     (Yes Refl)  = case decEq xs ys of
                                                        Yes prf   => yesTail prf
                                                        No contra => No (noTail contra)
    decEq (MyCons x xs) (MyCons y ys) |     (No contra) = No (noHead contra)

--   decEq : DecEq t => (x1 : t) -> (x2 : t) -> Dec (x1 = x2)
