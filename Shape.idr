module Shape

||| Represents shapes
export
data Shape = ||| A traingle, with ites base length and height
             Triangle Double Double
           | ||| A rectangle ,with its length and height
             Rectangle Double Double
           | ||| A circle ,with its radius
            Circle Double

private
rectangle_area : Double -> Double -> Double
rectangle_area width height = width * height

export
triangle : Double -> Double -> Shape
triangle = Triangle

export
rectangle : Double -> Double -> Shape
rectangle = Rectangle

export
circle : Double -> Shape
circle = Circle

area : Shape -> Double
area (Triangle base height) = 0.5 * rectangle_area base height
area (Rectangle length height) = rectangle_area length height
area (Circle radius) = pi * radius * radius

public export
data ShapeView : Shape -> Type where
     STraingle : ShapeView (triangle w h)
     SRectangle : ShapeView (rectangle w h)
     SCircle : ShapeView (circle c)

export
shapeView : (input : Shape) -> ShapeView input
shapeView (Triangle x y) = STraingle
shapeView (Rectangle x y) = SRectangle
shapeView (Circle x) = SCircle

