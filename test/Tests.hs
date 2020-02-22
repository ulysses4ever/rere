module Main (main) where

import Test.QuickCheck       ((===))
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Debug.Trace

import RERE
import RERE.Type (derivative1, derivative2)

import qualified Data.Set     as Set
import qualified RERE.CharSet as CS

main :: IO ()
main = defaultMain $ testGroup "RERE"
    [ testProperty "derivatives" $ \c re ->
        derivative1 c re === derivative2 c re
    , testProperty "RR nullable" $ \re ->
        nullable re === nullableR (fromRE re)
    , testProperty "RR nullable . derivative" $ \c re ->
        nullable (derivative c re) === nullableR (derivativeR c re)
    , testGroup "CharSet"
        [ testProperty "fromList . toList" $ \cs ->
            Set.fromList (CS.toList (CS.fromList (Set.toList cs))) === cs
        , testProperty "member" $ \c cs ->
            Set.member c cs === CS.member c (CS.fromList (Set.toList cs))
        , testProperty "union" $ \xs ys ->
            let xs' = CS.fromList (Set.toList xs)
                ys' = CS.fromList (Set.toList ys)
            in Set.toList (Set.union xs ys) === CS.toList (CS.union xs' ys')
        ]
    ]
