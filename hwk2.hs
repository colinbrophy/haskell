import Data.List (sortBy)

colLen :: (Enum b, Num b) => [a] -> b
colLen [] = 0
colLen (_:xs) = 1 + colLen xs 

meanList :: Fractional a => [a] -> a
meanList xs = sum xs / fromIntegral (length xs)

toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ reverse xs

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

sortLen :: [[a]] -> [[a]]
sortLen = sortBy cmpLen where
  cmpLen x y 
    | length x < length y = LT
    | length x > length y = GT
    | otherwise = EQ

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse sep (x:xs) = foldl (\acc y -> acc ++ sep : y) x xs

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

height :: Tree a -> Integer
height Empty = 0
height (Node _ a b) = succ $ max (height a) (height b)

data Direction = AngleLeft | AngleRight | Straight
            deriving (Show)
   
calcGrad (a1, a2) (b1, b2) = (a2 - b2) / (a1 - b1)
calcConst (x, y) grad = y - (grad * x)

calcDirection (a1, a2) (b1, b2) (c1, c2)
    | c1 < (m * c2 + c) = AngleLeft 
    | c1 > (m * c1 + c) = AngleRight
    | otherwise = Straight
    where 
        m = (b2 - a2)/(b1 - a1)
        c = a2 - (m * a1)
{-

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf Message Leaf
insert message@(LogMessage _ time _) (Node leftNode (LogMessage _ treeTime _) rightNode) |
    time <= treeTime = insert message leftNode |
    otherwise = insert message rightNode
-}
-- direction a b c = (x b - x a) (y)

-- if (y > mx + c) left
--   otherwi right

-- y a - m x a = y b - m x b
-- y1 = m.x1 + c
-- y2 = mx2 + c
-- y1 + y2 = ((y1 - y2)/ (x1 - x2))(x1 + x2) + 2c

-- y1 + y2 - m(x1 + x2) = 2c


-- m = (y1 - y2)/ (x1 - x2)
-- c = y1 
