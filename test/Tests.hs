module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck ((===))

import Debug.Trace

import RERE
import RERE.Type (derivative1, derivative2)

main :: IO ()
main = defaultMain $ testGroup "RERE"
    [ testProperty "derivatives" $ \c re ->
        derivative1 c re === derivative2 c re
    , testProperty "RR nullable" $ \re ->
        nullable re === nullableR (fromRE re)
    , testProperty "RR nullable . derivative" $ \c re ->
        nullable (derivative c re) === nullableR (derivativeR c re)
    ]
