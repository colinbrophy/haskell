import Data.List
import Data.Char
{-
skips a = map (flip take a) [al,al-1..1]
    where al = length a
-}
skips :: [a] -> [[a]]
skips = init . tails

localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:_)) |
    y > x && y > z = y : lm |
    otherwise = lm
    where lm = localMaxima xs
localMaxima _ = [] 

--lm a = foldr (\(x:y:z:_) a -> if y > z && y > x then y:a else a) [] 
--             $ (tails a)

lm a = [y | (x:y:z:_) <- tails a, x < y && y > z]




rotate = reverse . transpose
histogram = unlines . rotate . formattedBars
formattedBars xs = zipWith (\x y -> x : '=' : y) ['0'..'9'] bars
    where bars = genBars $ frequencies xs 

genBars freqs = map (take $ maximum freqs) longBars
    where longBars = map (\x -> replicate x '*' ++ repeat ' ') freqs
count xs y = length $ filter (y==) xs
frequencies xs = map (count xs) [0..9]
