-- 1. An n x m matrix can be represented by nested vectors of Double.
--    Define a type synonym:

import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

MatrixAny : (a : Type) -> Nat -> Nat -> Type
MatrixAny a n m = Vect n (Vect m a)

-- 2 x 3 == Vect n (Vect 3 a)
