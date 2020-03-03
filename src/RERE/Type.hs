{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
#if __GLASGOW_HASKELL__ >=704
{-# LANGUAGE Safe              #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy       #-}
#endif
-- | Regular-expression with fixed points.
module RERE.Type (
    -- * Regular expression type
    RE (..),
    -- * Smart constructors
    ch_, (\/), star_, let_, fix_, (>>>=),
#ifdef RERE_INTERSECTION
    (/\),
#endif
    string_,
    -- * Operations
    nullable,
    derivative,
    match,
    compact,
    -- * Internals
    derivative1,
    derivative2,
    ) where

import Data.String (IsString (..))
import Data.Void   (Void)

import qualified Data.Set        as Set
import qualified RERE.CharSet    as CS
import qualified Test.QuickCheck as QC

import RERE.Absurd
import RERE.Tuples
import RERE.Var

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>))
import Data.Foldable       (Foldable)
import Data.Traversable    (Traversable (..))
#endif

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-------------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------------

-- | Regular expression with fixed point.
data RE a
    = Null
    | Full
    | Eps
    | Ch CS.CharSet
    | App (RE a) (RE a)
    | Alt (RE a) (RE a)
    | Star (RE a)

#ifdef RERE_INTERSECTION
    | And (RE a) (RE a)
#endif

    | Var a
    | Let Name (RE a) (RE (Var a))
    | Fix Name        (RE (Var a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Ord a => IsString (RE a) where
    fromString = string_

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

arb :: Ord a => Int -> [QC.Gen a] -> QC.Gen (RE a)
arb n vars = QC.frequency $
    [ (1, pure Null)
    , (1, pure Full)
    , (1, pure Eps)
    , (5, Ch . CS.singleton <$> QC.elements "abcdef")
    ] ++
    [ (10, Var <$> g) | g <- vars ] ++
    (if n > 1
     then [ (20, app), (20, alt),  (10, st), (10, letG), (5, fixG)
#if RERE_INTERSECTION
          , (10, and_)
#endif
          ]
     else [])
  where
    alt = binary (\/)

#if RERE_INTERSECTION
    and_ = binary (/\)
#endif

    app = binary (<>)

    binary f = do
        m <- QC.choose (0, n)
        x <- arb m vars
        y <- arb (n - m) vars
        return (f x y)

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
    shrink    = shr
    
shr :: RE a -> [RE a]
shr Null   = []
shr Eps    = [Null]
shr Full   = [Eps]
shr (Ch _) = [Null, Eps]

shr (App r s) = r : s : map (uncurry App) (QC.liftShrink2 shr shr (r, s))
shr (Alt r s) = r : s : map (uncurry Alt) (QC.liftShrink2 shr shr (r, s))
shr (Star r)  = r : map Star (shr r)

#ifdef RERE_INTERSECTION
shr (And r s) = r : s :  map (uncurry And) (QC.liftShrink2 shr shr (r, s))
#endif

shr (Var _) = []
shr (Let n r s) = r : map (uncurry (Let n)) (QC.liftShrink2 shr shr (r, s))
shr (Fix n r) = map (Fix n) (shr r)

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
-- >>> nullable (ch_ 'c')
-- False
--
nullable :: RE a -> Bool
nullable = nullable' . fmap (const False)

nullable' :: RE Bool -> Bool
nullable' Null      = False
nullable' Full      = True
nullable' Eps       = True
nullable' (Ch _)    = False
nullable' (App r s) = nullable' r && nullable' s
nullable' (Alt r s) = nullable' r || nullable' s
nullable' (Star _)  = True

#ifdef RERE_INTERSECTION
nullable' (And r s) = nullable' r && nullable' s
#endif

nullable' (Var a)      = a
nullable' (Let _ r s)  = nullable' (fmap (unvar (nullable' r) id) s)
nullable' (Fix _ r1)   = nullable' (fmap (unvar False id) r1)

-- | Derivative of regular exression to respect of character.
-- @'derivative' c r@ is \(D_c(r)\).
derivative :: Char -> RE Void -> RE Void
derivative = derivative1

-- | 'derivative1' and 'derivative2' are slightly different
-- implementations internally. We are interested in comparing
-- whether either one is noticeably faster (no).
derivative2 :: Char -> RE Void -> RE Void
derivative2 c = go . vacuous where
    go :: Ord b => RE (Triple Bool b b) -> RE b
    go Null = Null
    go Full = Full
    go Eps = Null
    go (Ch x)
        | CS.member c x = Eps
        | otherwise     = Null
    go (App r s)
        | nullable' (fmap fstOf3 r) = go s \/ (go r <> fmap trdOf3 s)
        | otherwise                 =           go r <> fmap trdOf3 s

    go (Alt r s) = go r \/ go s
    go r0@(Star r) = go r <> fmap trdOf3 r0

#ifdef RERE_INTERSECTION
    go (And r s) = go r /\ go s
#endif

    go (Var x) = Var (sndOf3 x)

    go (Let n r s)
        | Just s' <- unused s
        = let_ n
               (fmap trdOf3 r)
               (go (fmap (bimap F F) s'))

        | otherwise
        = let_ n (fmap trdOf3 r)
        $ let_ n' (fmap F r')
        $ go
        $ s <&> \var -> case var of
            B   -> T (nullable' (fmap fstOf3 r)) B (F B)
            F x -> bimap (F . F) (F . F) x
      where
        r' = go r
        n' = derivativeName c n

    go r0@(Fix n r)
        = let_ n (fmap trdOf3 r0)
        $ fix_ n'
        $ go
        $ r <&> \var -> case var of
            B   -> T (nullable' (fmap fstOf3 r0)) B (F B)
            F x -> bimap (F . F) (F . F) x
      where
        n' = derivativeName c n

-- | 'derivative1' and 'derivative2' are slightly different
-- implementations internally. We are interested in comparing
-- whether either one is noticeably faster (no).
derivative1 :: Char -> RE Void -> RE Void
derivative1 c = go absurd where
    -- function to calculate nullability and derivative of a variable
    go :: (Ord a, Ord b) => (a -> Triple Bool b b) -> RE a -> RE b
    go _ Null = Null
    go _ Full = Full
    go _ Eps   = Null
    go _ (Ch x)
        | CS.member c x = Eps
        | otherwise     = Null
    go f (App r s)
        | nullable' (fmap (fstOf3 . f) r) = go f s \/ (go f r <> fmap (trdOf3 . f) s)
        | otherwise                       =            go f r <> fmap (trdOf3 . f) s
    go f (Alt r s) = go f r \/ go f s
    go f r0@(Star r) = go f r <> fmap (trdOf3 . f) r0

#ifdef RERE_INTERSECTION
    go f (And r s) = go f r /\ go f s
#endif

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
        $ go (\var ->  case var of
            B   -> T (nullable' (fmap (fstOf3 . f) r)) B (F B)
            F x -> bimap (F . F) (F . F) (f x))
        $ s
      where
        r' = go f r
        n' = derivativeName c n
    go f r0@(Fix n r)
        = let_ n (fmap (trdOf3 . f) r0)
        $ fix_ n'
        $ go (\var -> case var of
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
compact r@Full      = r
compact r@Eps       = r
compact r@(Ch _)    = r
compact r@(Var _)   = r
compact (App r s)   = compact r <> compact s
compact (Alt r s)   = compact r \/ compact s
compact (Star r)    = star_ (compact r)
compact (Let n r s) = let_ n (compact r) (compact s)
compact (Fix n r)   = fix_ n r
#ifdef RERE_INTERSECTION
compact (And r s)   = compact r /\ compact s
#endif

-------------------------------------------------------------------------------
-- smart constructors
-------------------------------------------------------------------------------

-- | Variable substitution.
(>>>=) :: Ord b => RE a -> (a -> RE b) -> RE b
Null       >>>= _ = Null
Full       >>>= _ = Full
Eps        >>>= _ = Eps
Ch c       >>>= _ = Ch c
App r s    >>>= k = (r >>>= k) <> (s >>>= k)
Alt r s    >>>= k = (r >>>= k) \/ (s >>>= k)
Star r     >>>= k = star_ (r >>>= k)
Var a      >>>= k = k a
Let n s r  >>>= k = let_ n (s >>>= k) (r >>>= unvar (Var B) (fmap F . k))
Fix n r1   >>>= k = fix_ n (r1 >>>= unvar (Var B) (fmap F . k))

#ifdef RERE_INTERSECTION
And r s    >>>= k = (r >>>= k) /\ (s >>>= k)
#endif

infixl 4 >>>=

-- | Smart 'Ch', as it takes 'Char' argument.
ch_ :: Char -> RE a
ch_ = Ch . CS.singleton

-- | Construct literal 'String' regex.
string_ :: Ord a => String -> RE a
string_ []  = Eps
string_ [c] = ch_ c
string_ xs  = foldr (\c r -> ch_ c <> r) Eps xs

-- | Smart 'Star'.
star_ :: RE a -> RE a
star_ Null       = Eps
star_ Eps        = Eps
star_ Full       = Full
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
    go _ _ Full      = Full
    go _ _ (Ch c)    = Ch c
    go v x (App a b) = App (go v x a) (go v x b)
    go v x (Alt a b) = Alt (go v x a) (go v x b)
    go v x (Star a)  = Star (go v x a)

#ifdef RERE_INTERSECTION
    go v x (And a b) = And (go v x a) (go v x b)
#endif

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
fix_ n r
    | Just r' <- traverse (unvar Nothing Just) r
    = r'
    | (r >>>= unvar Null Var) == Null
     = Null
    | Just r' <- floatOut r (unvar Nothing Just) (fix_ n)
    = r'
  where
-- fix_ n (Let m r s)
--     | Just r' <- traverse (unvar Nothing Just) r
--     = let_ m r' (fix_ n (fmap swapVar s))
fix_ n r = Fix n r

swapVar :: Var (Var a) -> Var (Var a)
swapVar (F (F a)) = F (F a)
swapVar (F B)     = B
swapVar B         = F B

floatOut
    :: (Ord a, Ord b)
    => RE (Var a)                        -- ^ expression
    -> (Var a -> Maybe b)                -- ^ float out var
    -> (RE (Var (Var a)) -> RE (Var b))  -- ^ binder
    -> Maybe (RE b)                      -- ^ maybe an expression with let floaten out
floatOut (Let m r s) un mk
    | Just r' <- traverse un r
    = Just
    $ Let m r' $ mk $ fmap swapVar s
    | otherwise
    = floatOut
        s
        (unvar Nothing un)
        (mk . Let m (fmap (fmap F) r) . fmap (fmap swapVar))
floatOut _ _ _ = Nothing

cheap :: RE a -> Bool
cheap Eps     = True
cheap Null   = True
cheap (Ch _)  = True
cheap (Var _) = True
cheap _       = False

instance Ord a => Semigroup (RE a) where
    Null      <> _         = Null
    _         <> Null      = Null
    Full      <> Full      = Full
    Eps       <> r         = r
    r         <> Eps       = r
    Let n x r <> s         = let_ n x (r <> fmap F s)
    r         <> Let n x s = let_ n x (fmap F r <> s)
    r         <> s         = App r s

infixl 5 \/
-- | Smart 'Alt'.
(\/) :: Ord a => RE a -> RE a -> RE a
r       \/ s       | r == s = r
Null    \/ r       = r
r       \/ Null    = r
Full    \/ _       = Full
_       \/ Full    = Full
Ch a    \/ Ch b    = Ch (CS.union a b)
Eps     \/ r       | nullable r = r
r       \/ Eps     | nullable r = r
Let n x r \/ s       = let_ n x (r \/ fmap F s)
r       \/ Let n x s = let_ n x (fmap F r \/ s)
r       \/ s       = foldr alt' Null $ ordNub (unfoldAlt r . unfoldAlt s $ [])
  where
    alt' x Null = x
    alt' x y    = Alt x y

#ifdef RERE_INTERSECTION
infixl 6 /\ -- silly CPP
-- | Smart 'Alt'.
(/\) :: Ord a => RE a -> RE a -> RE a
r       /\ s       | r == s = r
Null    /\ _       = Null
_       /\ Null    = Null
Full    /\ r       = r
r       /\ Full    = r
Ch a    /\ Ch b    = Ch (CS.intersection a b)
-- nullable is not precise here, so we cannot return Null when non nullable.
Eps     /\ r       | nullable r = Eps
r       /\ Eps     | nullable r = Eps
Let n x r /\ s       = let_ n x (r /\ fmap F s)
r       /\ Let n x s = let_ n x (fmap F r /\ s)
r       /\ s       = foldr and' Full $ ordNub (unfoldAnd r . unfoldAnd s $ [])
  where
    and' x Full = x
    and' x y    = And x y
#endif

-------------------------------------------------------------------------------
-- Tools
-------------------------------------------------------------------------------

unfoldAlt :: RE a -> [RE a] -> [RE a]
unfoldAlt (Alt a b) = unfoldAlt a . unfoldAlt b
unfoldAlt r         = (r :)

#ifdef RERE_INTERSECTION
unfoldAnd :: RE a -> [RE a] -> [RE a]
unfoldAnd (And a b) = unfoldAnd a . unfoldAnd b
unfoldAnd r         = (r :)
#endif

ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty where
    go !_ []     = []
    go !s (x:xs)
        | Set.member x s = go s xs
        | otherwise      = x : go (Set.insert x s) xs
