every_other : Stream a -> Stream a
every_other (x :: y :: left) = x :: every_other left

data InfList : Type -> Type where
     (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, ys, zs

countFrom : Integer -> InfList Integer
countFrom x = x :: (countFrom (x + 1))

getPrefix : (count: Nat) -> InfList a -> List a
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs

Functor InfList where
  map func (value :: xs) = (func value) :: map func xs

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
        (seed' `shiftR` 2) :: randoms seed'

data Face = Heads | Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips Z xs = []
coinFlips (S k) (x :: xs) = (if x < 0 then Heads else Tails) :: coinFlips k xs

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = let next = (approx + (number / approx)) / 2
                                       in approx :: square_root_approx number next


square_root_bound : (max :Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound Z number bound (value :: xs) = value
square_root_bound (S k) number bound (value :: xs) = let diff = abs (value * value - number)
                                                         in if diff < bound then value else square_root_bound k number bound xs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001 (square_root_approx number number)
