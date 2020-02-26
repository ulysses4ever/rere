{-# LANGUAGE CPP #-}
module DerpConv where

import Data.Void (Void, vacuous)
import qualified Text.Derp as D

import RERE
import qualified RERE.CharSet as CS

rere2derp :: RE Void -> D.Parser Char ()
rere2derp = go . vacuous where
    go :: RE (D.Parser Char ()) -> D.Parser Char ()

    go Null    = D.emp
    go Eps     = D.eps ()
    go (Ch cs) = derpVoid 
               $ foldr (D.<|>) D.emp
               $ map D.ter
               $ CS.toList cs
        
    go (App r s) = derpVoid $ go r D.<~> go s
    go (Alt r s) = go r D.<|> go s
    
    go (Star r) = star (go r)

    go (Var a) = a
    go (Let _ r s) =
        let r' = go r
        in go (fmap (unvar r' id) s)

    go (Fix _ r) =
        let r' = go (fmap (unvar r' id) r)
        in r'

#ifdef RERE_INTERSECTION
    go Full      = error "rere2derp: Full not supported"
    go (And _ _) = error "rere2derp: And not supported"
#endif

    derpVoid :: (Ord t, Ord b) => D.Parser t b -> D.Parser t ()
    derpVoid p = p D.==> const ()

    -- non-collecting star
    star :: (Ord t, Ord b) => D.Parser t b -> D.Parser t ()
    star p = r
      where
        r = D.eps () D.<|> p D.<~> r D.==> const ()
