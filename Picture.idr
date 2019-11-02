||| Represents shapes
data Shape = ||| A traingle, with ites base length and height
             Triangle Double Double
           | ||| A rectangle ,with its length and height
             Rectangle Double Double
           | ||| A circle ,with its radius
            Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

tringle : Picture
tringle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 45 5 circle)
              (Translate 15 25 tringle))

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic


data Biggest = NoTringle | Size Double

biggestTringle : Picture -> Biggest
biggestTringle (Primitive (Triangle x y)) = Size $ area (Triangle x y)
biggestTringle (Primitive (Rectangle x y)) = NoTringle
biggestTringle (Primitive (Circle x)) = NoTringle
biggestTringle (Combine pic pic1) = let picArea = biggestTringle pic
                                        pic1Area = biggestTringle pic1 in
                                          (case (picArea, picArea) of
                                                (NoTringle, NoTringle) => NoTringle
                                                (NoTringle, y) => y
                                                (x, NoTringle) => x
                                                (Size x, Size y) => if x > y then Size x else Size y)
biggestTringle (Rotate x pic) = biggestTringle pic
biggestTringle (Translate x y pic) = biggestTringle pic


