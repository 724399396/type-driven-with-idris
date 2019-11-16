import Data.Vect

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym $ plusZeroRightNeutral m
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in
                         rewrite plusSuccRightSucc m k in Refl

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
          where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
                reverse' {n} acc [] = rewrite plusZeroRightNeutral n in acc
                reverse' {n} {m = S len} acc (x :: ys) = let result = reverse' (x :: acc) ys in
                                                             rewrite plusAssociative n 1 len in
                                                             rewrite plusCommutative n 1 in
                                                             result


