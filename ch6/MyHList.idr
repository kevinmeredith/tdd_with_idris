import Data.Vect

-- HList - Heterogeneous list
-- Example: HList[Int, String] = [5, "foobar"]

-- first attempt
-- data MyHList = MyHListNil | MyHListCons a MyHList

-- 2nd attempt
-- data MyHList : Type -> Type where
--   HListCons : {b : Type} -> (a : Type) -> (MyHList b) -> MyHList _
--   HListNil : MyHList _

-- help from David C.
data HList : List Type -> Type where
   HL :
