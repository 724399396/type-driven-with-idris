data MyList : (elem : Type) -> Type where
     Nil : MyList elem
     (::) : (x: elem) -> (xs: MyList elem) -> MyList elem

data Elem : a -> List a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs value) -> Last (x :: xs) value

notInNil : Last [] value -> Void
notInNil LastOne impossible
notInNil (LastCons _) impossible

notEqual : (contra : (x = value) -> Void) -> Last [x] value -> Void
notEqual contra LastOne = contra Refl
notEqual _ (LastCons LastOne) impossible
notEqual _ (LastCons (LastCons _)) impossible

notInTail : (contra : Last (y :: xs) value -> Void) -> (Last (x :: (y :: xs)) value) -> Void
notInTail contra (LastCons prf) = (contra prf)

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notInNil
isLast (x :: []) value = case decEq x value of
                              (Yes Refl) => Yes LastOne
                              (No contra) => No (notEqual contra)
isLast (x :: (y :: xs)) value = case isLast (y :: xs) value of
                                     (Yes prf) => Yes (LastCons prf)
                                     (No contra) => No (notInTail contra)

