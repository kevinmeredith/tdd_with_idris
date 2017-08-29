f : (action : a) -> (success: a -> Bool) -> (n : Nat) -> String
f action success (S n) = if (success action) then "good" else f action success n
f _       _      Z     = "bad"
