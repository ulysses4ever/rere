{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main, jsonRE, jsonRE', jsonNames, jsonCFG, jsonDerp, derp) where

import Prelude hiding (exponent)

import Criterion.Main (bench, defaultMain, whnf)
import Data.Maybe     (isJust)
import System.Clock   (Clock (Monotonic), diffTimeSpec, getTime)

import qualified Data.Aeson      as A
import qualified Data.ByteString as BS
import qualified Data.Set        as Set
import qualified Text.Derp       as D

import RERE
import RERE.Examples.JSON

import DerpConv

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*))
#endif

derp :: D.Parser Char () -> String -> Bool
derp p s = not $ Set.null $ D.runParse p [ D.Token c [c] | c <- s]

aeson :: BS.ByteString -> Bool
aeson bs = isJust (A.decodeStrict bs :: Maybe A.Value)

main :: IO ()
main = do
    contents <- readFile    "jsverify.json"
    bs       <- BS.readFile "jsverify.json"

    do
        putStrLn "Ref run"
        start <- getTime Monotonic
        print $ matchR jsonRE contents
        end <- getTime Monotonic
        print $ diffTimeSpec end start

    do
        putStrLn "ST run"
        start <- getTime Monotonic
        print $ matchST jsonRE contents
        end <- getTime Monotonic
        print $ diffTimeSpec end start

--    do
--        putStrLn "derp run"
--        start <- getTime Monotonic
--        print $ derp jsonDerp $ take 0 contents
--        end <- getTime Monotonic
--        print $ diffTimeSpec end start

    putStrLn "Criterion"
    defaultMain
        [ bench "aeson"  $ whnf aeson bs
        , bench "rere"  $ whnf (matchST jsonRE) contents
        ]

-------------------------------------------------------------------------------
-- JSON definition
-------------------------------------------------------------------------------

jsonDerp :: D.Parser Char ()
jsonDerp = rere2derp jsonRE
