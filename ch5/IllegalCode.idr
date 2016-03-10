readNumber : IO (Maybe Nat)
readNumber = do
    input <- getLine
    if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing
