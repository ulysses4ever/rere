{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
#if __GLASGOW_HASKELL__ >=704
{-# LANGUAGE Safe         #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy  #-}
#endif
module RERE.CharSet (
    -- * Set of characters
    CharSet,
    -- * Construction
    empty,
    singleton,
    insert,
    union,
    -- * Query
    size,
    isEmpty,
    member,
    -- * Conversions
    fromList,
    toList,
    fromIntervalList,
    toIntervalList,
    ) where

import Data.Char   (chr, ord)
import Data.List   (foldl', sortBy)
import Data.String (IsString (..))

#if MIN_VERSION_containers(0,5,0)
import qualified Data.IntMap.Strict as IM
#else
import qualified Data.IntMap as IM
#endif

-- | A set of 'Char's.
--
-- We use range set, which works great with 'Char'.
newtype CharSet = CS { unCS :: IM.IntMap Int }
  deriving (Eq, Ord)

instance IsString CharSet where
    fromString = fromList

instance Show CharSet where
    showsPrec d cs
        | size cs < 20
        = showsPrec d (toList cs)
        | otherwise
        = showParen (d > 10)
        $ showString "CS "
        . showsPrec 11 (unCS cs)

-- | Empty character set.
empty :: CharSet
empty = CS IM.empty

-- | Check whether 'CharSet' is 'empty'. Not named 'null' to avoid name clashes.
isEmpty :: CharSet -> Bool
isEmpty (CS cs) = IM.null cs

-- | Size of 'CharSet'
--
-- >>> size $ fromIntervalList [('a','f'), ('0','9')]
-- 16
--
-- >>> length $ toList $ fromIntervalList [('a','f'), ('0','9')]
-- 16
--
size :: CharSet -> Int
size (CS m) = foldl' (\ !acc (lo, hi) -> acc + (hi - lo) + 1) 0 (IM.toList m)

-- | Singleton character set.
singleton :: Char -> CharSet
singleton c = CS (IM.singleton (ord c) (ord c))

-- | Test whether character is in the set.
member :: Char -> CharSet -> Bool
#if MIN_VERSION_containers(0,5,0)
member c (CS m) = case IM.lookupLE i m of
    Nothing      -> False
    Just (_, hi) -> i <= hi
  where
#else
member c (CS m) = go (IM.toList m)
  where
    go [] = False
    go ((x,y):zs) = (x <= i && i <= y) || go zs
#endif
    i = ord c

-- | Insert 'Char' into 'CharSet'.
insert :: Char -> CharSet -> CharSet
insert c (CS m) = normalise (IM.insert (ord c) (ord c) m)

-- | Union of two 'CharSet's.
union :: CharSet -> CharSet -> CharSet
union (CS xs) (CS ys) = normalise (IM.unionWith max xs ys)

-- | Make 'CharSet' from a list of characters, i.e. 'String'.
fromList :: String -> CharSet
fromList = normalise . foldl' (\ acc c -> IM.insert (ord c) (ord c) acc) IM.empty

-- | Convert 'CharSet' to a list of characters i.e. 'String'.
toList :: CharSet -> String
toList = concatMap (uncurry enumFromTo) . toIntervalList

-- | Convert to interval list
--
-- >>> toIntervalList $ union "01234" "56789"
-- [('0','9')]
--
toIntervalList :: CharSet -> [(Char, Char)]
toIntervalList (CS m) = [ (chr lo, chr hi) | (lo, hi) <- IM.toList m ]

-- | Convert from interval pairs.
--
-- >>> fromIntervalList []
-- ""
--
-- >>> fromIntervalList [('a','f'), ('0','9')]
-- "0123456789abcdef"
--
-- >>> fromIntervalList [('Z','A')]
-- ""
--
fromIntervalList :: [(Char,Char)] -> CharSet
fromIntervalList xs = normalise' $ sortBy (\a b -> compare (fst a) (fst b))
    [ (ord lo, ord hi)
    | (lo, hi) <- xs
    , lo <= hi
    ]

-------------------------------------------------------------------------------
-- Normalisation
-------------------------------------------------------------------------------

normalise :: IM.IntMap Int -> CharSet
normalise = normalise'. IM.toList

normalise' :: [(Int,Int)] -> CharSet
normalise' = CS . IM.fromList . go where
    go :: [(Int,Int)] -> [(Int,Int)]
    go []         = []
    go ((x,y):zs) = go' x y zs

    go' :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
    go' lo hi [] = [(lo, hi)]
    go' lo hi ws0@((u,v):ws)
        | u <= succ hi = go' lo (max v hi) ws
        | otherwise    = (lo,hi) : go ws0
