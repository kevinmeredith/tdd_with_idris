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

TupleVectType : Nat -> (a : Type) -> Type
TupleVectType Z     _ = ()
TupleVectType (S n) a = (a, TupleVectType n a)

TupleVect : (n : Nat) -> a -> TupleVectType n a
TupleVect Z _     = ()
TupleVect (S n) a = (a, TupleVect n a)
