data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

Eq Shape where
   (==) (Triangle x y) (Triangle x' y') = x == x' && y == y'
   (==) (Rectangle x y) (Rectangle x' y') = x == x' && y == y'
   (==) (Circle r) (Circle r') = r == r'
   (==) _ _ = False

Ord Shape where
    compare x y = compare (area x) (area y)
    where area : Shape -> Double
          area (Triangle x y) = 0.5 * x * y
          area (Rectangle x y) = x * y
          area (Circle x) = pi * x * x

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4,
Rectangle 2 7]
