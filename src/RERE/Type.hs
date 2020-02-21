{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}
module RERE.Type (
    RE (..),
    (\/), star_, let_, fix_, (>>>=),
    nullable,
    derivative,
    match,
    compact,
    -- * Internals
    derivative1,
    derivative2,
    ) where

import Data.Bifunctor (bimap)
import Data.Functor   ((<&>))
import Data.String    (IsString (..))
import Data.Void      (Void)

import qualified Data.Set        as Set
import qualified Test.QuickCheck as QC

import RERE.Absurd
import RERE.Tuples
import RERE.Var

-------------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------------

data RE a
    = Null
    | Eps
    | Ch Char
    | App (RE a) (RE a)
    | Alt (RE a) (RE a)
    | Star (RE a)

    | Var a
    | Let Name (RE a) (RE (Var a))
    | Fix Name        (RE (Var a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance IsString a => IsString (RE a) where
    fromString = Var . fromString

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

arb :: Ord a => Int -> [QC.Gen a] -> QC.Gen (RE a)
arb n vars = QC.oneof $
    [ pure Null
    , pure Eps
    , Ch <$> QC.elements "abcdef"
    ] ++
    [ Var <$> g | g <- vars ] ++
    (if n > 1 then [app, alt, st, letG, fixG] else [])
  where
    app = do
        m <- QC.choose (0, n)
        x <- arb m vars
        y <- arb (n - m) vars
        return (x \/ y)
    alt = do
        m <- QC.choose (0, n)
        x <- arb m vars
        y <- arb (n - m) vars
        return (x <> y)

    st = do
        m <- QC.choose (0, n - 1)
        x <- arb m vars
        return (star_ x)

    letG = do
        m <- QC.choose (0, n)
        name <- arbName
        x <- arb m vars
        y <- arb m (pure B : map (fmap F) vars)
        return $ let_ name x y

    fixG = do
        m <- QC.choose (0, n)
        name <- arbName
        y <- arb m (pure B : map (fmap F) vars)
        return $ fix_ name y

instance (Absurd a, Ord a) => QC.Arbitrary (RE a) where
    arbitrary = QC.sized $ \n -> arb n []

arbName :: QC.Gen Name
arbName = QC.elements ["x","y","z"]

-------------------------------------------------------------------------------
-- Match
-------------------------------------------------------------------------------

-- | Match string by iteratively differentiating the regular expression.
--
-- This version is slow, consider using 'RERE.matchR'.
match :: RE Void -> String -> Bool
match !re []     = nullable re
match !re (c:cs) = match (derivative c re) cs

-------------------------------------------------------------------------------
-- nullability and derivative
-------------------------------------------------------------------------------

-- | Whether the regular expression accepts empty string,
-- or whether the formal language contains empty string.
--
-- >>> nullable Eps
-- True
--
-- >>> nullable (Ch 'c')
-- False
--
nullable :: RE a -> Bool
nullable = nullable' . fmap (const False)

nullable' :: RE Bool -> Bool
nullable' Null      = False
nullable' Eps       = True
nullable' (Ch _)    = False
nullable' (App r s) = nullable' r && nullable' s
nullable' (Alt r s) = nullable' r || nullable' s
nullable' (Star _)  = True

nullable' (Var a)      = a
nullable' (Let _ r s)  = nullable' (fmap (unvar (nullable' r) id) s)
nullable' (Fix _ r1)   = nullable' (fmap (unvar False id) r1)

-- | Derivative of regular exression to respect of character.
-- @'derivative' c r@ is \(D_c(r)\).
derivative :: Char -> RE Void -> RE Void
derivative = derivative1

derivative2 :: Char -> RE Void -> RE Void
derivative2 c = go' . vacuous where
    go' :: Ord b => RE (Triple Bool b b) -> RE b
    go' Null = Null
    go' Eps = Null
    go' (Ch x)
      | c == x    = Eps
      | otherwise = Null
    go' (App r s)
        | nullable' (fmap fstOf3 r) = go' s \/ (go' r <> fmap trdOf3 s)
        | otherwise                 =           go' r <> fmap trdOf3 s

    go' (Alt r s) = go' r \/ go' s
    go' r0@(Star r) = go' r <> fmap trdOf3 r0

    go' (Var x) = Var (sndOf3 x)

    go' (Let n r s)
        | Just s' <- unused s
        = let_ n
               (fmap trdOf3 r)
               (go' (fmap (bimap F F) s'))

        | otherwise
        = let_ n (fmap trdOf3 r)
        $ let_ n' (fmap F r')
        $ go'
        $ s <&> \case
            B   -> T (nullable' (fmap fstOf3 r)) B (F B)
            F x -> bimap (F . F) (F . F) x
      where
        r' = go' r
        n' = derivativeName c n

    go' r0@(Fix n r)
        = let_ n (fmap trdOf3 r0)
        $ fix_ n'
        $ go'
        $ r <&> \case
            B   -> T (nullable' (fmap fstOf3 r0)) B (F B)
            F x -> bimap (F . F) (F . F) x
      where
        n' = derivativeName c n

derivative1 :: Char -> RE Void -> RE Void
derivative1 c = go absurd where
    -- function to calculate nullability and derivative of a variable
    go :: (Ord a, Ord b) => (a -> Triple Bool b b) -> RE a -> RE b
    go _ Null = Null
    go _ Eps   = Null
    go _ (Ch x)
        | c == x    = Eps
        | otherwise = Null
    go f (App r s)
        | nullable' (fmap (fstOf3 . f) r) = go f s \/ (go f r <> fmap (trdOf3 . f) s)
        | otherwise                       =            go f r <> fmap (trdOf3 . f) s
    go f (Alt r s) = go f r \/ go f s
    go f r0@(Star r) = go f r <> fmap (trdOf3 . f) r0

    go f (Var a) = Var (sndOf3 (f a))
    go f (Let n r s)
        | Just s' <- unused s
          -- spare the binding
        = let_ n
               (fmap (trdOf3 . f) r)
               (go (bimap F F . f) s')

        | otherwise
        = let_ n (fmap (trdOf3 . f) r)
        $ let_ n' (fmap F r')
        $ go (\case
            B   -> T (nullable' (fmap (fstOf3 . f) r)) B (F B)
            F x -> bimap (F . F) (F . F) (f x))
        $ s
      where
        r' = go f r
        n' = derivativeName c n
    go f r0@(Fix n r)
        = let_ n (fmap (trdOf3 . f) r0)
        $ fix_ n'
        $ go (\case
            B   -> T (nullable' (fmap (fstOf3 . f) r0)) B (F B)
            F x -> bimap (F . F) (F . F) (f x))
        $ r
      where
        n' = derivativeName c n

-------------------------------------------------------------------------------
-- unused
-------------------------------------------------------------------------------

unused :: RE (Var a) -> Maybe (RE a)
unused = traverse (unvar Nothing Just)

-------------------------------------------------------------------------------
-- compact
-------------------------------------------------------------------------------

-- | Re-apply smart constructors on 'RE' structure,
-- thus potentially making it smaller.
compact :: Ord a => RE a -> RE a
compact r@Null      = r
compact r@Eps       = r
compact r@(Ch _)    = r
compact r@(Var _)   = r
compact (App r s)   = compact r <> compact s
compact (Alt r s)   = compact r \/ compact s
compact (Star r)    = star_ (compact r)
compact (Let n r s) = let_ n (compact r) (compact s)
compact (Fix n r)   = fix_ n r

-------------------------------------------------------------------------------
-- smart constructors
-------------------------------------------------------------------------------

-- | Variable substitution.
(>>>=) :: Ord b => RE a -> (a -> RE b) -> RE b
Null       >>>= _ = Null
Eps        >>>= _ = Eps
Ch c       >>>= _ = Ch c
App r s    >>>= k = (r >>>= k) <> (s >>>= k)
Alt r s    >>>= k = (r >>>= k) \/ (s >>>= k)
Star r     >>>= k = star_ (r >>>= k)
Var a      >>>= k = k a
Let n s r  >>>= k = Let n (s >>>= k) (r >>>= unvar (Var B) (fmap F . k))
Fix n r1   >>>= k = Fix n (r1 >>>= unvar (Var B) (fmap F . k))

infixl 4 >>>=

-- | Smart 'Star'.
star_ :: RE a -> RE a
star_ Null       = Eps
star_ Eps        = Eps
star_ r@(Star _) = r
star_ r          = Star r

-- | Smart 'Let'
let_ :: Ord a => Name -> RE a -> RE (Var a) -> RE a
let_ n (Let m x r) s
    = let_ m x
    $ let_ n r (fmap (unvar B (F . F)) s)
let_ _ r s
    | cheap r
    = s >>>= unvar r Var
-- let_ _ r s
--     | foldMap (unvar (Sum 1) (\_ -> Sum 0)) s <=  Sum (1 :: Int)
--     = s >>>= unvar r Var
let_ n r s = postlet_ n r (go B (fmap F r) s) where
    go :: Ord a => a -> RE a -> RE a -> RE a
    go v x y | x == y = Var v
    go _ _ Eps       = Eps
    go _ _ Null      = Null
    go _ _ (Ch c)    = Ch c
    go v x (App a b) = App (go v x a) (go v x b)
    go v x (Alt a b) = Alt (go v x a) (go v x b)
    go v x (Star a)  = Star (go v x a)

    go _ _ (Var v) = Var v
    go v x (Let m a b)
        | x == a    = go v x (b >>>= unvar (Var v) Var)
        | otherwise = let_ m (go v x a) (go (F v) (fmap F x) b)
    go v x (Fix m a) = fix_ m (go (F v) (fmap F x) a)

postlet_ :: Ord a => Name -> RE a -> RE (Var a) -> RE a
postlet_ _ r (Var B) = r
postlet_ _ _ s
    | Just s' <- unused s
    = s'
postlet_ n r s       = Let n r s

-- | Smart 'Fix'.
fix_ :: Ord a => Name -> RE (Var a) -> RE a
fix_ _ r
    | Just r' <- unused r
    = r'
    | (r >>>= unvar Null Var) == Null
     = Null
fix_ n (Let m r s)
    | Just r' <- unused r
    = Let m r' (fix_ n (fmap swapVar s))
  where
    swapVar (F (F a)) = F (F a)
    swapVar (F B)     = B
    swapVar B         = F B
fix_ n r = Fix n r

cheap :: RE a -> Bool
cheap Eps     = True
cheap Null   = True
cheap (Ch _)  = True
cheap (Var _) = True
cheap _       = False

instance Ord a => Semigroup (RE a) where
    Null     <> _         = Null
    _         <> Null     = Null
    Eps       <> r         = r
    r         <> Eps       = r
    Let n x r <> s         = let_ n x (r <> fmap F s)
    r         <> Let n x s = let_ n x (fmap F r <> s)
    r         <> s         = App r s

infixl 5 \/
-- | Smart 'Alt'.
(\/) :: Ord a => RE a -> RE a -> RE a
r       \/ s       | r == s = r
Null   \/ r       = r
r       \/ Null   = r
Eps     \/ r       | nullable r = r
r       \/ Eps     | nullable r = r
Let n x r \/ s       = let_ n x (r \/ fmap F s)
r       \/ Let n x s = let_ n x (fmap F r \/ s)
r       \/ s       = foldr alt' Null $ ordNub (unfoldAlt r . unfoldAlt s $ [])
  where
    alt' x Null = x
    alt' x y    = Alt x y

unfoldAlt :: RE a -> [RE a] -> [RE a]
unfoldAlt (Alt a b) = unfoldAlt a . unfoldAlt b
unfoldAlt r         = (r :)

ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty where
    go !_ []     = []
    go !s (x:xs)
        | Set.member x s = go s xs
        | otherwise      = x : go (Set.insert x s) xs
