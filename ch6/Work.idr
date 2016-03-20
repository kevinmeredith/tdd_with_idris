StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False y = trim y
valToString True y  = cast y

AdderType : (numArgs : Nat) -> Type
AdderType Z     = Int
AdderType (S k) = (next : Int) -> AdderType k

adder : (n : Nat) -> (acc : Int) -> AdderType n
adder Z acc     = acc
adder (S k) acc = \x => (adder k (x+acc))

GenericAdderType : (numArgs: Nat) -> (t : Type) -> Type
GenericAdderType Z t     = t
GenericAdderType (S k) t = (next : t) -> GenericAdderType k t

genericAdder : (Num a) => (n : Nat) -> (acc : a) -> GenericAdderType n a
genericAdder Z acc = acc
genericAdder (S k) acc = \x => genericAdder k (x + acc)

data Format = Number Format
            | Str Format
            | Lit String Format
            | End

PrintfType : Format -> Type
PrintfType (Number fmt)  = (i : Int)      -> PrintfType fmt
PrintfType (Str fmt)     = (str : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End           = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc  = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc     = \s => printfFmt fmt (acc ++ s)
printfFmt (Lit str fmt) acc = printfFmt fmt (acc ++ str)
printfFmt End acc           = acc

toFormat : (xs : List Char) -> Format
toFormat []                 = End
toFormat ('%' :: 'd' :: xs) = Number $ toFormat xs
toFormat ('%' :: 's' :: xs) = Str $ toFormat xs
toFormat (x :: xs)          = Lit (cast x) $ toFormat xs

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

printf2 : (xs : String) -> PrintfType Format
printf2 xs = printfFmt (toFormat $ unpack xs) ""
