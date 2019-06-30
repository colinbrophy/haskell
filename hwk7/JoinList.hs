{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module JoinList where

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l Empty = l
(+++) Empty l = l
(+++) l1@(Append m1 _ _) l2@(Append m2 _ _) = Append (tag l1 <> tag l2) l1 l2
    
tag :: JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m

intSize :: (Sized b) => 
          JoinList b a -> Int
intSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) =>
           Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ i (Append m a b) | i < aSize = indexJ i a
                        | otherwise = indexJ (i - aSize) b 
                        where 
                           aSize = intSize a
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ 0 j@(Single _ _) = j
dropJ i (Append m a b) | i > aSize = dropJ (i - aSize) b
                       | otherwise = dropJ i a +++ b
                       where
                          aSize = intSize a

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a 
takeJ 1 j@(Single _ _) = j
takeJ i (Append m a b) | i < aSize = takeJ i a 
                       | otherwise = a +++ takeJ (i - aSize) b
                        where 
                          aSize = intSize a

