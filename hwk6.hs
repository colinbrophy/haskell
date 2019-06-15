{-# LANGUAGE FlexibleInstances #-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 = map fib [0..]

fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x y) = x : streamToList y

instance Show a => Show (Stream a) where
    show = show . take 50 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Stream x $ streamRepeat x
    
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x y) = Stream (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream a $ streamFromSeed f (f a)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

x :: Stream Integer
x = Stream 0 . Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = Stream n $ streamRepeat 0
    (+) (Stream a x) (Stream b y) = Stream (a + b) (x + y)
    (*) a@(Stream a0 aRest) b@(Stream b0 bRest) = Stream (a0 * b0) rest
       where rest = (fromInteger a0) * bRest + aRest * fromInteger b0
    negate (Stream x0 x) = Stream (-x0) (negate x)
