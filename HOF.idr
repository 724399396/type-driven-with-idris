double : (Num ty) => ty -> ty

twice : (a -> a) -> a -> a
twice f x = f (f x)

Shape : Type
rotate : Shape -> Shape

quardruple : Num a => a -> a
quardruple = twice double

turn_around : Shape -> Shape
turn_around = twice rotate

add : Int -> Int -> Int
add x y = x + y
