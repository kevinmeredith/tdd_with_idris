data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] Empty                    = "empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Non-empty, initial portion = " ++ show xs

total
listLast : (xs : List a) -> ListLast xs
listLast []        = Empty
listLast (x :: xs) = case listLast xs of
                      Empty         => NonEmpty [] x
                      NonEmpty ys y => NonEmpty (x :: ys) y

describeListEnd : List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)

describeListEnd' : List Int -> String
describeListEnd' input with (listLast input)
   describeListEnd' []          | Empty = "Empty"
   describeListEnd' (xs ++ [x]) | (NonEmpty xs x) = "Non-empty, initial portion =" ++ show xs

myReverse : List a -> List a
myReverse input with (listLast input)
   myReverse []          | Empty = []
   myReverse (xs ++ [x]) | (NonEmpty xs x) = x :: myReverse xs

data SplitList : List a -> Type where
  SplitNil  : SplitList []
  SplitOne  : (x : a)      -> SplitList [x]
  SplitPair : (xs: List a) -> (ys : List a) -> SplitList (xs ++ ys)

total
splitList : (input : List a) -> SplitList input
splitList input = splitListHelp input input
 where
   splitListHelp : List a -> (input : List a) -> SplitList input
   splitListHelp _ []  = SplitNil
   splitListHelp _ [x] = SplitOne x
   splitListHelp (_ :: _ :: counter) (item :: items) =
     case splitListHelp counter items of
       SplitNil               => SplitOne _
       SplitOne {x}           => SplitPair [item] [x]
       SplitPair lefts rights => SplitPair (item :: lefts) rights
   splitListHelp _ items = SplitPair [] items

mergeSort : Ord a => List a -> List a
mergeSort input with (splitList input)
   mergeSort []                | SplitNil                 = []
   mergeSort [x]               | (SplitOne x)             = [x]
   mergeSort (lefts ++ rights) | (SplitPair lefts rights) =
    mergeSort lefts ++ mergeSort rights
