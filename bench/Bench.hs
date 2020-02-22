{-# LANGUAGE CPP #-}
module Main (main) where

import RERE (match)
import RERE.Ref (matchR)
import RERE.Examples

import Criterion.Main (defaultMain, whnf, bench)

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

input :: String
input = "1000*(2020+202)*(20+3)*((30+20)*10000)+123123123*12313"

input2 :: String
input2 = "1000*(20+a)*((30+20)*10000)"

parsec :: P.Parser () -> String -> Bool
parsec p s = case P.parse (p <* P.eof) "<input>" s of
    Right _ -> True
    Left _  -> False

main :: IO ()
main = do
    print $ parsec ex7parsec input
#ifdef RERE_CFG
    print $ match  ex7       input
    print $ matchR ex7       input
#endif
    print $ parsec ex7parsec input2
#ifdef RERE_CFG
    print $ match  ex7       input2
    print $ matchR ex7       input2
#endif
    defaultMain
        [ bench "parsec" $ whnf (parsec ex7parsec) input
#ifdef RERE_CFG
        , bench "rere"   $ whnf (match  ex7)       input
        , bench "ref"    $ whnf (matchR ex7)       input
#endif
        , bench "parsec" $ whnf (parsec ex7parsec) input2
#ifdef RERE_CFG
        , bench "rere"   $ whnf (match  ex7)       input2
        , bench "ref"    $ whnf (matchR ex7)       input2
#endif
        ]
