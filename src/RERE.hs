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
#ifdef RERE_INTERSECTION
    (/\),
#endif
    string_,
    -- ** Operations
    nullable, derivative, compact, size,
    -- ** Matching
    match,
    -- ** Generation
    generate,

    -- * Variables
    Var (..), unvar,
    Name,

    -- * Context-free grammars
#ifndef RERE_NO_CFG
    CFG, CFGBase,
    cfgToRE,
#endif

    -- * Faster matching
    -- ** Ref
    RR, matchR, matchDebugR,
    -- ** ST
    RST, matchST, matchDebugST,

    -- * Character classes
    CharClasses,
    charClasses,
    classOfChar,

    -- * Pretty printing (as LaTeX)
    putLatex,
    putLatexTrace,
#ifndef RERE_NO_CFG
    putLatexCFG,
#endif
    ) where

import RERE.CharClasses
import RERE.Gen
import RERE.LaTeX
import RERE.Ref
import RERE.ST
import RERE.Type
import RERE.Var

#ifndef RERE_NO_CFG
import RERE.CFG
#endif
