import Data.Vect

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
          where
            reverseProof : Vect (k + 1) elem -> Vect (S k) elem
            reverseProof {k} result = rewrite plusCommutative 1 k in result

