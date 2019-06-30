module Employee where

import           Data.Tree

-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun  = Integer

-- An Employee consists of a name and a fun score.
data Employee = Emp { empName :: Name, empFun :: Fun }
  deriving (Show, Read, Eq)

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 2)
      [ Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]
      , Node (Emp "Fred" 3) []
      ]
    , Node (Emp "Sarah" 17)
      [ Node (Emp "Sam" 4) []
      ]
    ]

testCompany2 :: Tree Employee
testCompany2
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 3) -- (8, 8)
      [ Node (Emp "Joe" 5) -- (5, 6)
        [ Node (Emp "John" 1) [] -- (1, 0)
        , Node (Emp "Sue" 5) [] -- (5, 0)
        ]
      , Node (Emp "Fred" 3) [] -- (3, 0)
      ]
    , Node (Emp "Sarah" 17) -- (17, 4)
      [ Node (Emp "Sam" 4) [] -- (4, 0)
      ]
    ]

-- A type to store a list of guests and their total fun score.
data GuestList = GL [Employee] Fun
  deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) (empFun emp + fun)

instance Semigroup GuestList where
  (GL l1 f1) <> (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)
instance Monoid GuestList where
  mempty = GL [] 0


moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: b -> (a -> [b] -> b)-> Tree a -> b
treeFold e f t@(Node v forest) = f v $ map (treeFold e f) forest 

nextLevel boss gl = (glCons boss withoutSubBoss, withoutBoss)
                where 
                   -- No point inviting both boss and subboss
                  withoutSubBoss = newMax $ map snd gl  
                  withoutBoss = newMax $ map fst gl 

newMax [] = mempty
newMax x = maximum x

maxFun :: Tree Employee -> GuestList
maxFun emps = uncurry max $ treeFold empty nextLevel emps  
        where empty = (mempty, mempty)
