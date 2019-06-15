import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l Empty = l
(+++) Empty l = l
(+++) l1@(Append m1 _ _) l2@(Append m2 _ _) = Append (tag l1 <> tag l2) l1 l2
  where 
    tag (Single m _) = m
    tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) =>
           Int -> JointList b a -> Maybe aA
indexJ 0 (Single _ a) = Just a
indexJ i (Append _ a) = if getSize 
indexJ _ _ = Nothing
