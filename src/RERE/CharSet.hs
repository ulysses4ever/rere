{-# LANGUAGE Trustworthy #-}
module RERE.CharSet where

import Data.Char            (ord, chr)
import Data.Coerce          (coerce)
import Data.RangeSet.IntMap (RIntSet)
import Data.String (IsString (..))

import qualified Data.RangeSet.IntMap as RS

-- | A set of 'Char's.
--
-- We use range set, which works great with 'Char'.
newtype CharSet = CS RIntSet
  deriving (Eq, Ord)

instance IsString CharSet where
    fromString = CS . RS.fromList . map ord

instance Show CharSet where
    showsPrec d (CS cs)
        | RS.size cs < 20
        = showsPrec d (map chr $ RS.toList cs)
        | otherwise
        = showParen (d > 10)
        $ showString "CS "
        . showsPrec 11 cs

-- | Empty character set.
empty :: CharSet
empty = CS RS.empty

-- | Singleton character set.
singleton :: Char -> CharSet
singleton = CS . RS.singleton . ord

-- | Test whether character is in the set.
member :: Char -> CharSet -> Bool
member c (CS rs) = RS.member (ord c) rs

-- | Union of two 'CharSet's.
union :: CharSet -> CharSet -> CharSet
union = coerce RS.union
