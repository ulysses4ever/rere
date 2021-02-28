{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >=704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy #-}
#endif
-- | Using 'RE' to generate example 'String's.
module RERE.Gen (generate) where

import Control.Applicative (liftA2)
import Data.Char           (ord)
import Data.Void           (Void, vacuous)
import Test.QuickCheck     (Gen, arbitrary, choose, frequency, oneof)

import RERE.CharSet
import RERE.Type
import RERE.Var

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

-- $setup
-- >>> import Test.QuickCheck.Random (mkQCGen)
-- >>> import Test.QuickCheck.Gen (unGen)
-- >>> import RERE.Type
-- >>> let runGen seed = maybe "<<null>>" (\g' -> unGen g' (mkQCGen seed) 10)

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- | Generate strings.
--
-- >>> runGen 43 $ generate 10 10 $ star_ (ch_ 'a')
-- "aaaaaaaaaa"
--
-- >>> runGen 44 $ generate 10 10 $ star_ (ch_ 'a')
-- "aaa"
--
generate
    :: Int      -- ^ star upper size
    -> Int      -- ^ fix unroll
    -> RE Void
    -> Maybe (Gen String)
generate starSize fixSize = fmap (fmap ($ "")) . go . vacuous where
    go :: RE (Maybe (Gen ShowS)) -> Maybe (Gen ShowS)
    go Null = Nothing
    go Full = Just arbitrary
    go Eps  = Just (return id)
    go (Ch c) = case toIntervalList c of
        [] -> Nothing
        xs -> Just $ frequency
            [ (ord hi - ord lo + 1, showChar <$> choose (lo,hi))
            | (lo,hi) <- xs
            ]

    go (App x y) = do
        x' <- go x
        y' <- go y
        return (liftA2 (.) x' y')
    go (Alt x y) = alt (go x) (go y) where
        alt (Just x') (Just y') = Just (oneof [x', y'])
        alt x'        Nothing   = x'
        alt Nothing   y'        = y'
    go (Star x) = case go x of
        Nothing -> Just (return id)
        Just x' -> Just $ do
            n <- choose (0, starSize)
            if n <= 0
            then return id
            else foldr (\_ acc -> liftA2 (.) acc x') x' [2..n]

#ifdef RERE_INTERSECTION
    -- this is tricky.
    go (And _ _) = Nothing
#endif

    go (Var x) = x
    go (Let _ r s)  = go (fmap (unvar (go r) id) s)
    go (Fix _ r) = go' fixSize where
        go' :: Int -> Maybe (Gen ShowS)
        go' n | n <= 0    = Nothing
              | otherwise = go (fmap (unvar (go' (n - 1)) id) r)
