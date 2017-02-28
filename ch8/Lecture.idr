import Data.Vect

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num								

sameS : (eq : EqNat k j) -> EqNat (S k) (S j)
sameS (Same n) = Same (S n)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z     Z     = Just $ Same Z
checkEqNat Z     (S k) = Nothing
checkEqNat (S k) Z     = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
						   Just eq => Just $ sameS eq
						   Nothing => Nothing

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case (checkEqNat m len) of 
							  Just (Same m) => Just input
							  Nothing       => Nothing

checkEqNat2 : (num : Nat) -> (num2 : Nat) -> Maybe (num = num2)					
checkEqNat2 Z Z     = Just Refl
checkEqNat2 Z (S _) =  Nothing

f : Maybe Int -> Maybe Int
f (Just 42) = Just 42

-- Exercise 1
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons prf1 = cong prf1

-- Exercise 2
same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists refl1 refl2 = ?hole

-- Exercise 3
data ThreeEq : a -> b -> c -> Type
  Same3 : x -> ThreeEq x x x

-- Exercise 4

--allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
--allSameS x y z (Same3 x) = cong (Same3 (S x))