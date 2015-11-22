module Hw1

import Data.Vect

one : (xs : Vect n elem) -> Vect n elem
one xs = xs

two : (xs : Vect n elem) -> Vect (n * 2) elem
two xs = ?unknownTwo

three : (xs : Vect (n + 1) elem) -> Vect n elem
three {n} xs = Data.Vect.take n xs

data Word = Foo | Bar

f : Word -> Bool
f Foo = True
