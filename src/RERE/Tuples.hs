{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >=704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy #-}
#endif
module RERE.Tuples where

-------------------------------------------------------------------------------
-- 3-tuples
-------------------------------------------------------------------------------

data Triple a b c = T !a !b !c
  deriving (Eq, Ord)

bimap :: (b -> b') -> (c -> c') -> Triple a b c -> Triple a b' c'
bimap f g (T a b c) = T a (f b) (g c)

fstOf3 :: Triple a b c -> a
fstOf3 (T a _ _) = a

sndOf3 :: Triple a b c -> b
sndOf3 (T _ b _) = b

trdOf3 :: Triple a b c -> c
trdOf3 (T _ _ c) = c
