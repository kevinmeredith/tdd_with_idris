data Foo = A Int | B String
data Bar = C Int | D String

data Higher = FooWrapper Foo | BarWrapper Bar

-- data Higher : a -> Type where
--   FooImpl : x -> Higher x
--   BarImpl : x -> Higher x
--
-- f : Bool -> Either (Higher x) Int
-- f True  = Right 42
-- f False = Left $ FooImpl $ A 5


f : List a -> Bool
f [] = True
f _  = False
