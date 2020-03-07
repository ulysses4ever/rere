{-# LANGUAGE CPP #-}
module Main (main) where

import Criterion.Main (bench, defaultMain, whnf)
import RERE.Examples  (ex7parsec)

import qualified Text.Parsec        as P
import qualified Text.Parsec.String as P

import qualified Text.Derp as D
import qualified Data.Set as Set

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*))
#endif

#ifndef RERE_NO_CFG
import RERE          (match, matchR, matchST)
import RERE.Examples (ex7)
#endif

import DerpConv

input :: String
input = "1000*(2020+202)*(20+3)*((30+20)*10000)+123123123*12313"

input2 :: String
input2 = "1000*(20+a)*((30+20)*10000)"

parsec :: P.Parser () -> String -> Bool
parsec p s = case P.parse (p <* P.eof) "<input>" s of
    Right _ -> True
    Left _  -> False

derp :: D.Parser Char () -> String -> Bool
derp p s = not $ Set.null $ D.runParse p [ D.Token c [c] | c <- s]

main :: IO ()
main = do
    print $ parsec  ex7parsec input
#ifndef RERE_NO_CFG
    print $ match   ex7       input
    print $ matchR  ex7       input
    print $ matchST ex7       input
#endif
    print $ derp    ex7derp   input
#ifndef RERE_NO_CFG
    print $ derp    ex7derp'  input
#endif

    print $ parsec  ex7parsec input2
#ifndef RERE_NO_CFG
    print $ match   ex7       input2
    print $ matchR  ex7       input2
    print $ matchST ex7       input2
#endif
    print $ derp   ex7derp   input2
#ifndef RERE_NO_CFG
    print $ derp   ex7derp'  input2
#endif

    defaultMain
        [ bench "parsec" $ whnf (parsec ex7parsec) input
#ifndef RERE_NO_CFG
        , bench "rere"   $ whnf (match  ex7)       input
        , bench "ref"    $ whnf (matchR ex7)       input
        , bench "st"     $ whnf (matchST ex7)      input
#endif
        , bench "derp"   $ whnf (derp   ex7derp)   input
#ifndef RERE_NO_CFG
        , bench "derpC"  $ whnf (derp   ex7derp')  input
#endif

        , bench "parsec" $ whnf (parsec ex7parsec) input2
#ifndef RERE_NO_CFG
        , bench "rere"   $ whnf (match  ex7)       input2
        , bench "ref"    $ whnf (matchR ex7)       input2
        , bench "st"     $ whnf (matchST ex7)      input2
#endif
        , bench "derp"   $ whnf (derp   ex7derp)   input2
#ifndef RERE_NO_CFG
        , bench "derpC"  $ whnf (derp   ex7derp')  input2
#endif
        ]

-------------------------------------------------------------------------------
-- Derp
-------------------------------------------------------------------------------

#ifndef RERE_NO_CFG
ex7derp' :: D.Parser Char ()
ex7derp' = rere2derp ex7
#endif

ex7derp :: D.Parser Char ()
ex7derp = expr
  where
    digit  = foldr (D.<|>) D.emp $ map D.ter "0123456789"
    digits = derpVoid $ digit D.<~> derpStar digit

    term   = digits D.<|> derpVoid (D.ter '(' D.<~> expr D.<~> D.ter ')')
    mult   = derpVoid (term D.<~> D.ter '+' D.<~> mult) D.<|> term
    expr   = derpVoid (mult D.<~> D.ter '*' D.<~> expr) D.<|> mult

    derpStar :: (Ord t, Ord b) => D.Parser t b -> D.Parser t [b]
    derpStar p = let ps = D.eps [] D.<|> (p D.<~> ps D.==> uncurry (:)) in ps

    derpVoid :: (Ord t, Ord b) => D.Parser t b -> D.Parser t ()
    derpVoid p = p D.==> const ()
