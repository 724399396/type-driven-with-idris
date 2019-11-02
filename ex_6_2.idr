import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Double)

testMatrix : Matrix 2 3
testMatrix = [[0,0,0], [0,0,0]]

data Format = Number Format
            | Str Format
            | Lit String Format
            | Ch Format
            | Dou Format
            | End

PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType (Ch fmt) = (c : Char) -> PrintfType fmt
PrintfType (Dou fmt) = (d : Double) -> PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc: String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Ch fmt) acc = \ch => printfFmt fmt (acc ++ show ch)
printfFmt (Dou fmt) acc = \d => printfFmt fmt (acc ++ show d)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Ch (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dou (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

TupleVect : Nat -> Type -> Type
TupleVect Z elemType = ()
TupleVect (S k) elemType = (elemType, TupleVect k elemType)

test : TupleVect 4 Nat
test = (1,2,3,4,())
