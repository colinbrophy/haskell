

lastButOne [x,y] = Just x
lastButOne (x:xs) = lastButOne xs
lastButOne x = Nothing
