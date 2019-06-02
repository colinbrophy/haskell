

--myLength :: [x] -> Integer
myLength [] = 0
myLength (_:xs) = succ $ myLength xs 
