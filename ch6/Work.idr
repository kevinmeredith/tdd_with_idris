StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False y = trim y
valToString True y  = cast y
