{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >=710
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy #-}
#endif
-- | Charactor classes.
module RERE.CharClasses (
    CharClasses,
    charClasses,
    classOfChar,
    ) where

import RERE.Type

import qualified Data.Set     as Set
import qualified RERE.CharSet as CS

-- | Character classes are represented by partition lower bounds.
type CharClasses = Set.Set Char

-- | Character classes.
--
-- We can partition 'Char' so characters in each part,
-- affect the given regular expression in the same way.
--
-- If we do some kind of memoising, we can map all characters
-- to 'classOfChar', making everything smaller.
--
charClasses :: RE a -> CharClasses
charClasses = charsetClasses . Set.toList . collect

-- | Map char to the representer of a class.
classOfChar :: CharClasses -> Char -> Char
#if MIN_VERSION_containers(0,5,0)
classOfChar cc c = case Set.lookupLE c cc of
    Just c' -> c'
    Nothing -> '\NUL'
#else
-- old containers: slow path
classOfChar _ c = c
#endif

collect :: RE a -> Set.Set CS.CharSet
collect = go where
    go :: RE a -> Set.Set CS.CharSet
    go Null        = Set.empty
    go Full        = Set.empty
    go Eps         = Set.empty
    go (Ch cs)     = Set.singleton cs
    go (App r s)   = Set.union (go r) (go s)
    go (Alt r s)   = Set.union (go r) (go s)
    go (Star r)    = go r
#ifdef RERE_INTERSECTION
    go (And r s)   = Set.union (go r) (go s)
#endif
    go (Var _)     = Set.empty
    go (Let _ r s) = Set.union (go r) (go s)
    go (Fix _ r)   = go r

charsetClasses :: [CS.CharSet] -> CharClasses
charsetClasses = go (Set.singleton '\NUL') where
    go acc []       = acc
    go acc (cs:css) = go
        (Set.union acc $ Set.fromList $ concatMap bounds $ CS.toIntervalList cs)
        css

    bounds (x, y) | y == maxBound = [x]
                  | otherwise     = [x, succ y]
