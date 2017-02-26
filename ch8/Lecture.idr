import Data.Vect

--exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
--exactLength {m} len input = case (len == m) of 
--								True  => Just $ input
--								False => Nothing

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num								

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z     Z     = Just $ Same 0
checkEqNat Z     (S k) = Nothing
checkEqNat (S k) Z     = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
						   Just eq => Just $ Same num1
						   Nothing => Nothing