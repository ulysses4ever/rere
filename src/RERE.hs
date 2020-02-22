{-# LANGUAGE Safe #-}
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
    CFG, CFGBase,
    cfgToRE,

    -- * Faster matching
    matchR,
    -- ** Utilities
    -- | These are exported for tests, they aren't useful otherwise.
    RR, fromRE, nullableR, derivativeR,

    -- * Pretty printing (as LaTeX)
    putLatex,
    putLatexTrace,
    putLatexCFG,
    ) where

import RERE.CFG
import RERE.Type
import RERE.Var
import RERE.LaTeX
import RERE.Ref
