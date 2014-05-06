{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m x y) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty a = a
(+++) a Empty = a
(+++) a b = Append (tag a <> tag b) a b

sizeJ :: (Sized b, Monoid b) => JoinList b a -> Int
sizeJ Empty = 0
sizeJ (Single s x) = getSize . size $ s
sizeJ (Append s a b) = getSize . size $ s

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i Empty = Nothing
indexJ i (Single s x)
  | i == 0    = Just x
  | otherwise = Nothing
indexJ i (Append s a b)
  | i < (sizeJ a) = indexJ i a
  | i < (sizeJ a) + (sizeJ b) = indexJ (i - (sizeJ a)) b
  | otherwise = Nothing 

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n Empty = Empty
dropJ n (Single s x)
  | n > 0    = Empty
  | otherwise = Single s x
dropJ n (Append s a b)
  | n < sizeJ a = (dropJ n a) +++ b
  | n == sizeJ a = b
  | otherwise = dropJ (n - (sizeJ a)) b

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n Empty = Empty
takeJ n (Single s x)
  | n > 0    = Single s x
  | otherwise = Empty
takeJ n (Append s a b)
  | n < sizeJ a = takeJ n a
  | n == sizeJ a = a
  | otherwise = a +++ takeJ (n - (sizeJ a)) b

valueJ :: JoinList (Score,Size) a -> Score
valueJ Empty = 0
valueJ (Single (score, _) _) = score
valueJ (Append (score, _) _ _) = score
 
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

scoreAndCountLine :: String -> JoinList (Score, Size) String
scoreAndCountLine s = Single (scoreString s, 1) s

balanceJ :: (Sized m, Monoid m) => JoinList m a -> JoinList m a
balanceJ l@(Append m a b) = (balanceJ . (takeJ split) $ l) +++ (balanceJ . (dropJ split) $ l)
  where split = (`div` 2) . getSize . size $ m
balanceJ x = x

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ a b) = (toString a) ++ (toString b)
  fromString   = balanceJ . (foldr (+++) Empty) . (map scoreAndCountLine) . lines
  line     = indexJ
  replaceLine n l b = (takeJ n b) +++ (scoreAndCountLine l) +++ (dropJ (n+1) b)
  numLines     = sizeJ
  value        = getScore . valueJ

main = runEditor editor $ balanceJ . (foldr (+++) Empty) . (map scoreAndCountLine) $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
