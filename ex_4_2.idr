import Data.Vect

vectTake : (n : Nat) -> Vect (n+m) a -> Vect n a
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: (vectTake k xs)


sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just idx) => Just $ (index idx xs) + (index idx ys)
