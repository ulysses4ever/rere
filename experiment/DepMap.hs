{-# LANGUAGE GADTs, ScopedTypeVariables, RankNTypes #-}
module RERE.DepMap where

import Data.Void (Void)
import Data.Proxy (Proxy (..))

import RERE.Var

import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Singleton for Var Peano
-------------------------------------------------------------------------------

data SN a where
    SZ :: SN Void
    SS :: SNI a => {-# UNPACK #-} !(Proxy a) -> SN (Var a)

class Ord a =>    SNI a       where sn :: SN a
instance          SNI Void    where sn = SZ
instance SNI a => SNI (Var a) where sn = SS Proxy

sn' :: SNI a => proxy a -> SN a
sn' _ = sn

withCompare :: SN a -> SN b -> ((a ~ b, SNI a) => r) -> (Ordering -> r) -> r
withCompare SZ     SZ     k _ = k
withCompare (SS a) (SS b) k l = withCompare (sn' a) (sn' b) k l
withCompare SZ     (SS _) _ k = k LT
withCompare (SS _) SZ     _ k = k GT

-------------------------------------------------------------------------------
-- Ord1
-------------------------------------------------------------------------------

-- old style Ord1, because I'm lazy
class Ord1 f where
    compare1 :: Ord a => f a -> f a -> Ordering

-------------------------------------------------------------------------------
-- Dependent map
-------------------------------------------------------------------------------

newtype DepMap f g = DP (Map.Map (Keyed f) (Keyed g))

data Keyed f where
    Key :: SN a -> f a -> Keyed f

instance Ord1 f => Eq (Keyed f) where
    x == y = compare x y == EQ

instance Ord1 f => Ord (Keyed f) where
    compare (Key u a) (Key v b) = withCompare u v (compare1 a b) id

dmInsert :: (SNI a, Ord1 f) => f a -> g a -> DepMap f g -> DepMap f g
dmInsert k v (DP m) = DP (Map.insert k' v' m)
  where
    k' = Key sn k
    v' = Key sn v

dmLookup :: (SNI a, Ord1 f) => f a -> DepMap f g -> Maybe (g a)
dmLookup k (DP mm) = case Map.lookup k' mm of
    Nothing          -> Nothing
    Just (Key m v' ) -> withCompare (sn' k) m (Just v') (const Nothing)
  where
    k' = Key sn k

dpEmpty :: DepMap f g
dpEmpty = DP Map.empty
