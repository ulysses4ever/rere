{-# LANGUAGE Safe #-}
module RERE.Absurd (Absurd (..), vacuous) where

import qualified Data.Void as Void

class Absurd a where
    absurd :: a -> b

instance Absurd Void.Void where
    absurd = Void.absurd

vacuous :: (Functor f, Absurd a) => f a -> f b
vacuous = fmap absurd
