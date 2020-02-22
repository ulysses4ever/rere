{-# LANGUAGE Safe #-}
-- | Absurd i.e. not inhabited types.
module RERE.Absurd (Absurd (..), vacuous) where

import qualified Data.Void as Void

-- | A vendored 'Absurd' from https://hackage.haskell.org/package/boring
--
-- We only need 'Void' instance.
class Absurd a where
    absurd :: a -> b

instance Absurd Void.Void where
    absurd = Void.absurd

-- | @fmap absurd@.
vacuous :: (Functor f, Absurd a) => f a -> f b
vacuous = fmap absurd
