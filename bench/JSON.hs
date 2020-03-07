{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main, jsonRE, jsonRE', jsonNames, jsonCFG, jsonDerp, derp) where

import Prelude hiding (exponent)

import Criterion.Main (bench, defaultMain, whnf)
import Data.Either    (isRight)
import System.Clock   (Clock (Monotonic), diffTimeSpec, getTime)

import qualified Data.Aeson.Parser          as A
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString            as BS
import qualified Data.Set                   as Set
import qualified Text.Derp                  as D

#ifdef MIN_VERSION_saison
import qualified Saison.Decoding.Parser as S
import qualified Saison.Decoding.Tokens as S
#endif

import RERE
import RERE.Examples.JSON

import DerpConv

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*))
#endif

derp :: D.Parser Char () -> String -> Bool
derp p s = not $ Set.null $ D.runParse p [ D.Token c [c] | c <- s]

aeson :: BS.ByteString -> Bool
aeson bs = isRight (Atto.parseOnly (A.json <* Atto.endOfInput) bs)

#ifdef MIN_VERSION_saison
saison :: BS.ByteString -> Bool
saison = go end . S.tokens where
    end :: BS.ByteString -> Bool
    end = BS.null . S.skipSpace

    go :: (k -> Bool) -> S.Tokens k e -> Bool
    go kont (S.TkLit _ k)        = kont k
    go kont (S.TkText _ k)       = kont k
    go kont (S.TkNumber _ k)     = kont k
    go kont (S.TkArrayOpen arr)  = goArr kont arr
    go kont (S.TkRecordOpen obj) = goObj kont obj
    go _    (S.TkErr _)          = False

    goArr :: (k -> Bool) -> S.TkArray k e -> Bool
    goArr kont (S.TkItem k)     = go (goArr kont) k
    goArr kont (S.TkArrayEnd k) = kont k
    goArr _    (S.TkArrayErr _) = False

    goObj :: (k -> Bool) -> S.TkRecord k e -> Bool
    goObj kont (S.TkPair _ k)    = go (goObj kont) k
    goObj kont (S.TkRecordEnd k) = kont k
    goObj _    (S.TkRecordErr _) = False
#endif

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
#ifdef MIN_VERSION_saison
        , bench "saison" $ whnf saison bs
#endif
        -- , bench "rere"  $ whnf (matchR jsonRE) contents
        ]

-------------------------------------------------------------------------------
-- JSON definition
-------------------------------------------------------------------------------

jsonDerp :: D.Parser Char ()
jsonDerp = rere2derp jsonRE
