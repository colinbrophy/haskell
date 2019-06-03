
fun1 = product . map (subtract 2) . filter even

fun2 = sum . filter even . takeWhile (>1) . iterate f
    where f n | even n = n `div` 2
              | otherwise = 3 * n + 1

xor :: [Bool] -> Bool
xor = foldl (\x y -> not (x && y)) False

myfoldl f base l = foldr . flip f . base $ reverse l

cartProd :: Integer -> [(Integer, Integer)]
cartProd n = [(x,y) | x <- [1..n], y <- [1..n], x <= y]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ filter (`notElem` sieve) [1..n]
    where sieve = map (\(i, j) -> i + j + 2*i*j) $ cartProd n

