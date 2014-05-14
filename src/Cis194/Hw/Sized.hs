{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Cis194.Hw.Sized where

import Data.Monoid

class Sized a where
  size :: a -> Size
  
-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance Sized b => Sized (a,b) where
  size = size . snd

--
-- Size is a box around an Int
--
--   Size implements Monoid and Sized
--
newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

instance Monoid Size where
  mempty  = Size 0
  mappend = (+)

instance Sized Size where
  size = id
