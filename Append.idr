import Data.Vect

append : (elem: Type) -> (n: Nat) -> (m: Nat) -> Vect n elem -> Vect m elem -> Vect (n + m) elem
append elem Z m [] ys = ys
append elem (S k) m (x :: xs) ys = x :: append elem k m xs ys
