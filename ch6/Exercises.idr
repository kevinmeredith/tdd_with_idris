-- 1. An n x m matrix can be represented by nested vectors of Double.
--    Define a type synonym:

import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

MatrixAny : (a : Type) -> Nat -> Nat -> Type
MatrixAny a n m = Vect n (Vect m a)

-- TupleVect 0 ty = ()
-- TupleVect 1 ty = (ty, ())
-- TupleVect 2 ty = (ty, (ty, ()))

TupleVect (n : Nat) -> (a : Type) -> Type
TupleVect Z      _ = ()
TupleVect (S n)  a =

TupleVect : a -> (n : Nat **
TupleVect Z a     = (a)
TupleVect (S n) a = (a, TupleVect n a)

-- 2 x 3 == Vect n (Vect 3 a)
