StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety four"
getStringOrInt True = 94


valtoString : (isInt : Bool) -> (case isInt of
                                      False => String
                                      True => Int) -> String
valtoString False x = trim x
valtoString True x = cast x
