twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible

valueNotSucc  : (x : Nat) -> x = S x -> Void
valueNotSucc _ Refl impossible

zeroNotSucc : (0 = S k) -> Void
zeroNotSucc Refl impossible

succNotZero : (S k = 0) -> Void
succNotZero Refl impossible

noRec : (contra : (j = k) -> Void) -> (S j = S k) -> Void
noRec = ?hole

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z     Z     = Yes Refl
checkEqNat Z    (S k)  = No zeroNotSucc
checkEqNat (S k) Z     = No succNotZero
checkEqNat (S j) (S k) = case (checkEqNat j k) of
                           Yes prf   => Yes (cong prf)
                           No contra => No (noRec contra)
