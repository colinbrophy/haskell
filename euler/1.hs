
divides x d = x `mod` d == 0

main = print $ sum [x | x <- [1..1000], (x `divides` 3 || x `divides` 5)]
