data Tree elem = Empty
| Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                           LT => Node (insert x left) val right
                                           EQ => orig
                                           GT => Node left val (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = (treeToList left) ++ [x] ++ (treeToList right)

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y


maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just x) = Just x
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just $ if x > y then x else y


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

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
         (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
         (Primitive (Circle 4))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic


biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive (Triangle x y)) = Just $ area (Triangle x y)
biggestTriangle (Primitive (Rectangle x y)) = Nothing
biggestTriangle (Primitive (Circle x)) = Nothing
biggestTriangle (Combine pic pic1) = let picArea = biggestTriangle pic
                                         pic1Area = biggestTriangle pic1 in
                                          (case (picArea, pic1Area) of
                                                (Nothing, Nothing) => Nothing
                                                (Nothing, y) => y
                                                (x, Nothing) => x
                                                (Just x, Just y) => if x > y then Just x else Just y)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

