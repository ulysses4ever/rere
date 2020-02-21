{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Safe              #-}
module RERE.Var where

import Data.String (IsString (..))

-- | 'Var' is essentially 'Maybe'.
data Var a
    = B    -- ^ bound
    | F a  -- ^ free variable.
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Analogue of 'maybe' for 'Var'.
unvar :: r -> (a -> r) -> Var a -> r
unvar n _ B     = n
unvar _ j (F x) = j x

instance IsString a => IsString (Var a) where fromString = F . fromString

-- | Names carry information used in pretty-printing,
-- but otherwise they all 'compare' 'EQ'ual.
data Name = N String [Char]

instance Show Name where
    showsPrec d (N n sfx)
        | null sfx = showsPrec d n
        | otherwise
        = showParen (d > 10)
        $ showString "N "
        . showsPrec 11 n
        . showChar ' '
        . showsPrec 11 sfx

instance Eq Name where _ == _ = True
instance Ord Name where compare _ _ = EQ
instance IsString Name where fromString n = N n []

-- | Make a name for derivative binding (adds subindex).
derivativeName :: Char -> Name -> Name
derivativeName c (N n cs) =  N n (cs ++ [c])
