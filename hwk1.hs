import Data.Char (digitToInt)
-- Following this tutorial http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

toDigits :: (Integral a, Show a) => a -> [a]
toDigits x 
  | x > 0 = map digitToNum $ show x
  | otherwise = []
  where digitToNum = fromIntegral . digitToInt

toDigitsRev :: (Integral a, Show a) => a -> [a]
toDigitsRev = reverse . toDigits

doubleEveryOther :: Num a => [a] -> [a]
doubleEveryOther = reverse . doubleFromLeft . reverse
  where doubleFromLeft = zipWith (*) $ cycle [1,2]

sumDigits :: (Integral a, Show a) => [a] -> a
sumDigits = sum . concatMap toDigits

validate :: (Integral a, Show a) => a -> Bool
validate = divides 10 . sumDigits . doubleEveryOther . toDigits

divides :: Integral a => a -> a -> Bool
divides d n = n `mod` d == 0

type Peg = String
type Move = (Peg, Peg)

hannoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hannoi 0 _ _ _ = []
hannoi n a b c = hannoi (n - 1) a c b ++ ((a,b) : hannoi (n - 1) c b a)

han n = hannoi n "a" "b" "c"
