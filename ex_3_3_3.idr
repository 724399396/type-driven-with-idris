import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         zipWith (::) x xsTrans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = let xsYsAdd = addMatrix xs ys
                                in zipWith (+) x y :: xsYsAdd

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [] _ = []
multMatrix (x :: xs) right = let tranposeRight = transposeMat right
                                 in (map (\r => sum $ zipWith (*) x r) tranposeRight) :: multMatrix xs right
