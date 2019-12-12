import DataStore
import Shape

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues input with (storeView input)
  getValues input | SNil = []
  getValues (addToStore (key, value) store) | (SAdd rec) = value :: (getValues store | rec)


area : Shape -> Double
area s with (shapeView s)
  area (triangle w h) | STraingle = 0.5 * w * h
  area (rectangle w h) | SRectangle = w * h
  area (circle c) | SCircle = pi * c * c

