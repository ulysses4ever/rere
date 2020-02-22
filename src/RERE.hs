{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >=704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy #-}
#endif
-- | Regular-expressions extended with fixpoints
-- for context-free powers.
--
-- Some examples are in "RERE.Examples" module.
--
module RERE (
    -- * Regular expressions with fixpoints
    RE (..),
    -- ** Smart constructors
    ch_, (\/), star_, let_, fix_, (>>>=),
    -- ** Operations
    nullable, derivative, compact,
    -- ** Matching
    match,

    -- * Variables
    Var (..), Name,

    -- * Context-free grammars
#ifdef RERE_CFG
    CFG, CFGBase,
    cfgToRE,
#endif

    -- * Faster matching
    matchR,
    -- ** Utilities
    -- | These are exported for tests, they aren't useful otherwise.
    RR, fromRE, nullableR, derivativeR,

    -- * Pretty printing (as LaTeX)
    putLatex,
    putLatexTrace,
#ifdef RERE_CFG
    putLatexCFG,
#endif
    ) where

import RERE.LaTeX
import RERE.Ref
import RERE.Type
import RERE.Var

#ifdef RERE_CFG
import RERE.CFG
#endif
