data Vect : Nat -> Type -> Type where
     Nil : Vect Z a
     (::) : (x : a) -> (xs: Vect k a) -> Vect (S k) a

elem : Eq ty => (value: ty) -> (xs : Vect n ty) -> Bool
elem value [] = False
elem value (x :: xs) = case value == x of
                            False => elem value xs
                            True => True

