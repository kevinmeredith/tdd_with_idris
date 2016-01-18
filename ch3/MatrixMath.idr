module MatrixMath

add : (Num n) => (Vect len (Vect row n)) -> (Vect len (Vect row n)) -> (Vect len (Vect row n))
add [] []               = []
add (x :: xs) (y :: ys) =

add2Vects 
