{-# LANGUAGE CPP #-}
module Main (main) where

import Test.QuickCheck       (label, (===), property)
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import RERE
import RERE.Type (derivative1, derivative2)

import qualified Data.Set     as Set
import qualified RERE.CharSet as CS

import Control.Monad.ST (runST)

topCon :: RE a -> String
topCon Null    = "Null"
topCon Full    = "Full"
topCon Eps     = "Eps"
topCon Ch {}   = "Ch"
topCon Alt {}  = "Alt"
topCon App {}  = "App"
topCon Star {} = "Star"
topCon Var {}  = "Var"
topCon Let {}  = "Let"
topCon Fix {}  = "Fix"
#ifdef RERE_INTERSECTION
topCon And {}  = "And"
#endif

main :: IO ()
main = defaultMain $ testGroup "RERE"
    [ testProperty "derivatives" $ \c re ->
        label (topCon re) $
        derivative1 c re === derivative2 c re
    , testProperty "RR nullable" $ \re ->
        label (topCon re) $
        match re ""  === matchR re ""
     , testProperty "RR nullable . derivative^2" $ \c d re ->
        label (topCon re) $
        label (topCon (derivative d (derivative c re))) $
        match re [c,d] === matchR re [c,d]
     , testProperty "RR nullable . derivative^3" $ \c d e re ->
        label (topCon re) $
        label (topCon (derivative c (derivative d (derivative c re)))) $
        match re [c,d,e] === matchR re [c,d,e]

    , testGroup "CharSet"
        [ testProperty "fromList . toList" $ \cs ->
            Set.fromList (CS.toList (CS.fromList (Set.toList cs))) === cs
        , testProperty "member" $ \c cs ->
            Set.member c cs === CS.member c (CS.fromList (Set.toList cs))

        , testProperty "union" $ \xs ys ->
            let xs' = CS.fromList (Set.toList xs)
                ys' = CS.fromList (Set.toList ys)
            in Set.toList (Set.union xs ys) === CS.toList (CS.union xs' ys')
        , testProperty "intersection" $ \xs ys ->
            let xs' = CS.fromList (Set.toList xs)
                ys' = CS.fromList (Set.toList ys)
            in Set.toList (Set.intersection xs ys) === CS.toList (CS.intersection xs' ys')
        , testProperty "difference" $ \xs ys ->
            let xs' = CS.fromList (Set.toList xs)
                ys' = CS.fromList (Set.toList ys)
            in Set.toList (Set.difference xs ys) === CS.toList (CS.difference xs' ys')

        , testProperty "union: left identity" $ \xs ->
            let xs' = CS.fromList xs
            in xs' === CS.union CS.empty xs'
        , testProperty "union: right identity" $ \xs ->
            let xs' = CS.fromList xs
            in xs' === CS.union xs' CS.empty
        , testProperty "union: commutativity" $ \xs ys ->
            let xs' = CS.fromList xs
                ys' = CS.fromList ys
            in CS.union xs' ys' === CS.union ys' xs'
        , testProperty "union: associativity" $ \xs ys zs ->
            let xs' = CS.fromList xs
                ys' = CS.fromList ys
                zs' = CS.fromList zs
            in CS.union xs' (CS.union ys' zs') === CS.union (CS.union xs' ys') zs'

       , testProperty "intersection: left identity" $ \xs ->
            let xs' = CS.fromList xs
            in xs' === CS.intersection CS.universe xs'
        , testProperty "intersection: right identity" $ \xs ->
            let xs' = CS.fromList xs
            in xs' === CS.intersection xs' CS.universe
        , testProperty "intersection: commutativity" $ \xs ys ->
            let xs' = CS.fromList xs
                ys' = CS.fromList ys
            in CS.intersection xs' ys' === CS.intersection ys' xs'
        , testProperty "intersection: associativity" $ \xs ys zs ->
            let xs' = CS.fromList xs
                ys' = CS.fromList ys
                zs' = CS.fromList zs
            in CS.intersection xs' (CS.intersection ys' zs') === CS.intersection (CS.intersection xs' ys') zs'

        , testProperty "complement intersection is empty" $ \xs ->
            let xs' = CS.fromList xs
            in CS.null $ CS.intersection xs' (CS.complement xs')

        , testProperty "complement union is universe" $ \xs ->
            let xs' = CS.fromList xs
            in CS.universe == CS.union xs' (CS.complement xs')
        ]
    ]
