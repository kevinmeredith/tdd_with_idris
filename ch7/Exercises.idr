Angle : Type
Angle = Double

Side : Type
Side = Double

Diameter : Type
Diameter = Double

data Shape = Triangle Angle Angle
   | Rectangle Side Side
   | Circle Diameter

implementation Eq Shape where
  (==) (Triangle a b)  (Triangle x y)  = (a == x && b == y) || (a == y && b == x)
  (==) (Rectangle a b) (Rectangle x y) = (a == x && b == y) || (a == y && b == x)
  (==) (Circle a)      (Circle b)      = a == b
  (==) _                _              = False

data Expr num = Val num
  | Add (Expr num) (Expr num)
  | Sub (Expr num) (Expr num)
  | Mul (Expr num) (Expr num)
  | Div (Expr num) (Expr num)
  | Abs (Expr num)

eval : (Neg num, Integral num) => Expr num -> num
eval (Val n)     = n
eval (Add e1 e2) = (eval e1) +     (eval e2)
eval (Sub e1 e2) = (eval e1) -     (eval e2)
eval (Mul e1 e2) = (eval e1) *     (eval e2)
eval (Div e1 e2) = (eval e1) `div` (eval e2)
eval (Abs e)     = abs $ eval e

implementation (Eq num, Neg num, Integral num) => Eq (Expr num) where
 (==) e1 e2 = (eval e1) == (eval e2)

implementation (Show num) => Show (Expr num) where
  show (Val n)     = show n
  show (Add e1 e2) = "( " ++ show e1 ++ " + " ++ show e2 ++ " )"
  show (Sub e1 e2) = "( " ++ show e1 ++ " - " ++ show e2 ++ " )"
  show (Mul e1 e2) = "( " ++ show e1 ++ " * " ++ show e2 ++ " )"
  show (Div e1 e2) = "( " ++ show e1 ++ " div " ++ show e2 ++ " )"
  show (Abs e)     = "( " ++ "abs(" ++ show e ++ ") " ++ " )"
