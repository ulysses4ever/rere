{-# LANGUAGE Safe #-}
module RERE.Tuples where

import Data.Bifunctor (Bifunctor (..))

-------------------------------------------------------------------------------
-- 3-tuples
-------------------------------------------------------------------------------

data Triple a b c = T !a !b !c
  deriving (Eq, Ord)
instance Bifunctor (Triple a) where
    bimap f g (T a b c) = T a (f b) (g c)

fstOf3 :: Triple a b c -> a
fstOf3 (T a _ _) = a

sndOf3 :: Triple a b c -> b
sndOf3 (T _ b _) = b

trdOf3 :: Triple a b c -> c
trdOf3 (T _ _ c) = c
