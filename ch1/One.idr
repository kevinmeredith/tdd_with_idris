module Hw1

import Data.Vect

one : (xs : Vect n elem) -> Vect n elem
one xs = xs

two : (xs : Vect n elem) -> Vect (n * 2) elem
two xs = ?unknownTwo

three : (xs : Vect (n + 1) elem) -> Vect n elem
three {n} xs = Data.Vect.take n xs

data DoorState = DOpen | DClosed

data DoorAction : DoorState -> DoorState -> Type where
  Open  : DoorAction DClosed DOpen
  Close : DoorAction DOpen DClosed

data Interaction : DoorState -> DoorState -> Type where
  Nil  : Interaction a b
  Cons : DoorAction a b -> Interaction b c -> Interaction a c
