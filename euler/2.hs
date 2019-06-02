-- Haskell is so pretty :)
-- divides d x = x `mod` d == 0
fibs = [0, 1] ++ zipWith (+) fibs (tail fibs)
main = print . sum . filter even . takeWhile (< 4*10^6) $ fibs
