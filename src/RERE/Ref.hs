{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >=710
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy #-}
#endif
-- | Regular expression with explicit sharing.
--
-- 'RR' is an opaque type, to maintain the invariants.
module RERE.Ref (
    RR,
    matchR, matchDebugR,
    ) where

import Control.Monad.Fix         (mfix)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
       (State, StateT, evalState, evalStateT, get, modify, put, runState)
import Data.Void                 (Void, vacuous)

import qualified Data.Map as Map
import qualified Data.Set as Set

import           RERE.CharClasses
import qualified RERE.CharSet     as CS
import qualified RERE.Type        as R
import           RERE.Var

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$), (<$>), (<*>))
#endif

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif

import Control.Monad.ST
import Data.STRef

-------------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------------

-- | Knot-tied recursive regular expression.
data RR s
    = Eps
    | Ch CS.CharSet
    | App (RR s) (RR s)
    | Alt (RR s) (RR s)
#ifdef RERE_INTERSECTION
    | And (RR s) (RR s)
#endif
    | Star (RR s)
    | Ref !Int !(STRef s (Map.Map Char (RR s))) (RR s)

instance Show (RR s) where
    showsPrec = go Set.empty where
        go :: Set.Set Int -> Int -> RR s -> ShowS
        go _    _ Eps       = showString "Eps"
        go _    d (Ch c)    = showParen (d > 10) $ showString "Ch " . showsPrec 11 c
        go past d (App r s)
            = showParen (d > 10)
            $ showString "App"
            . showChar ' ' . go past 11 r
            . showChar ' ' . go past 11 s
        go past d (Alt r s)
            = showParen (d > 10)
            $ showString "Alt"
            . showChar ' ' . go past 11 r
            . showChar ' ' . go past 11 s
#ifdef RERE_INTERSECTION
        go past d (And r s)
            = showParen (d > 10)
            $ showString "And"
            . showChar ' ' . go past 11 r
            . showChar ' ' . go past 11 s
#endif
        go past d (Star r)
            = showParen (d > 10)
            $ showString "Star"
            . showChar ' ' . go past 11 r

        go past d (Ref i _ r)
            | Set.member i past = showParen (d > 10)
            $ showString "Ref " . showsPrec 11 i . showString " <<loop>>"
            | otherwise         = showParen (d > 10)
            $ showString "Ref " . showsPrec 11 i . showChar ' ' . go (Set.insert i past) 11 r

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

-- | Convert 'R.RE' to 'RR'.
fromRE :: R.RE Void -> M s (RR s)
fromRE re = go (vacuous re) where
    go R.Null   = return nullRR
    go R.Full   = return fullRR
    go R.Eps    = return Eps
    go (R.Ch c) = return (Ch c)

    go (R.App r s) = do
        r' <- go r
        s' <- go s
        return (app_ r' s')

    go (R.Alt r s) = do
        r' <- go r
        s' <- go s
        return (alt_ r' s')

#ifdef RERE_INTERSECTION
    go (R.And r s) = do
        r' <- go r
        s' <- go s
        return (and_ r' s')
#endif

    go (R.Star r) = do
        r' <- go r
        return (star_ r')

    go (R.Var r) = return r

    go (R.Let _ r s) = do
        r' <- go r
        -- it looks like we shouldn't memoize here
        -- both simple and json benchmark are noticeably faster.
        go (fmap (unvar r' id) s)

    go (R.Fix _ r) = mfix $ \res -> do
        i <- newId
        ref <- lift (newSTRef Map.empty)
        r' <- go (fmap (unvar res id) r)
        return (Ref i ref r')

_size :: RR s -> Int
_size rr = evalState (go rr) Set.empty where
    go Eps       = return 1
    go (Ch _)    = return 1
    go (App r s) = plus1 <$> go r <*> go s
    go (Alt r s) = plus1 <$> go r <*> go s
#ifdef RERE_INTERSECTION
    go (And r s) = plus1 <$> go r <*> go s
#endif
    go (Star r)  = succ <$> go r
    go (Ref i _ r) = do
        visited <- get
        if Set.member i visited
        then return 1
        else do
            put (Set.insert i visited)
            succ <$> go r

    plus1 x y = succ (x + y)

-------------------------------------------------------------------------------
-- Variable supply monad
-------------------------------------------------------------------------------

type M s = StateT Int (ST s)

newId :: M s Int
newId = do
    i <- get
    put $! i + 1
    return i

_returnI :: RR s -> M s (RR s)
_returnI r@Eps    = return r
_returnI r@Ch {}  = return r
_returnI r@Ref {} = return r
_returnI r = do
    i <- newId
    ref <- lift (newSTRef Map.empty)
    return (Ref i ref r)

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

nullRR :: RR s
nullRR = Ch CS.empty

fullRR :: RR s
fullRR = Star (Ch CS.universe)

isNull :: RR s -> Bool
isNull (Ch c) = CS.null c
isNull _      = False

isFull :: RR s -> Bool
isFull (Star (Ch x)) = x == CS.universe
isFull _             = False

app_ :: RR s -> RR s -> RR s
app_ r    _    | isNull r = r
app_ _    r    | isNull r = r
app_ Eps  r    = r
app_ r    Eps  = r
app_ r    s    = App r s

alt_ :: RR s -> RR s -> RR s
alt_ r      s      | isNull r = s
alt_ r      s      | isNull s = r
alt_ r      s      | isFull r || isFull s = fullRR
alt_ (Ch a) (Ch b) = Ch (CS.union a b)
alt_ r      s      = Alt r s

#ifdef RERE_INTERSECTION
and_ :: RR s -> RR s -> RR s
and_ r      s      | isFull r = s
and_ r      s      | isFull s = r
and_ r      s      | isNull r || isNull s = nullRR
and_ (Ch a) (Ch b) = Ch (CS.intersection a b)
and_ r      s      = And r s
#endif

star_ :: RR s -> RR s
star_ r          | isNull r
                 = Eps
star_ Eps        = Eps
star_ r@(Star _) = r
star_ r          = Star r

-------------------------------------------------------------------------------
-- Match
-------------------------------------------------------------------------------

-- | Convert 'R.RE' to 'RR' and then match.
--
-- Significantly faster than 'RERE.Type.match'.
--
matchR :: R.RE Void -> String -> Bool
matchR re str = runST (evalStateT (fromRE re >>= go0) 0)
  where
    go0 :: RR s -> M s Bool
    go0 rr = do
        let cc = charClasses re
        go cc str rr

    go :: CharClasses -> String -> RR s -> M s Bool
    go _  []     rr = return $ nullableR rr
    go cc (c:cs) rr = do
        let c' = classOfChar cc c
        rr' <- derivative c' rr
        go cc cs rr'

-- | Match and print final 'RR' + stats.
matchDebugR :: R.RE Void -> String -> IO ()
matchDebugR re str = runST (evalStateT (fromRE re >>= go0) 0)
  where
    go0 :: RR s -> M s (IO ())
    go0 rr = do
        let cc = charClasses re
        go cc str rr

    go :: CharClasses -> String -> RR s -> M s (IO ())
    go _  []     rr = return $ putStr $ unlines
            [ "size: " ++ show (_size rr)
            , "show: " ++ show rr
            , "null: " ++ show (nullableR rr)
            ]

    go cc (c:cs) rr = do
        let c' = classOfChar cc c
        rr' <- derivative c' rr
        go cc cs rr'

-------------------------------------------------------------------------------
-- Derivative
-------------------------------------------------------------------------------

derivative :: Char -> RR s -> M s (RR s)
derivative c = go where
    go :: RR s -> M s (RR s)
    go Eps                    = return nullRR
    go (Ch x) | CS.member c x = return Eps
              | otherwise     = return nullRR

    go (Alt r s) = do
        r' <- go r
        s' <- go s
        return (alt_ r' s')

#ifdef RERE_INTERSECTION
    go (And r s) = do
        r' <- go r
        s' <- go s
        return (and_ r' s')
#endif

    go (App r s)
        | nullableR r = do
            r' <- go r
            s' <- go s
            return $ alt_ s' (app_ r' s)
        | otherwise = do
            r' <- go r
            return $ app_ r' s

    go r0@(Star r) = do
        r' <- go r
        return (app_ r' r0)

    go (Ref _ ref r) = do
        m <- lift (readSTRef ref)
        case Map.lookup c m of
            Just r' -> return r'
            Nothing -> mfix $ \res -> do
                j <- newId
                ref' <- lift (newSTRef Map.empty)
                lift (writeSTRef ref (Map.insert c res m))
                r' <- go r
                return (Ref j ref' r')

-------------------------------------------------------------------------------
-- Nullable equations
-------------------------------------------------------------------------------

-- | Whether 'RR' is nullable.
--
-- @
-- 'R.nullable' re = 'nullableR' ('fromRE' re)
-- @
nullableR :: RR s -> Bool
nullableR r =
    let (bexpr, eqs) = equations r
    in lfp bexpr eqs

equations :: RR s -> (BoolExpr, Map.Map Int BoolExpr)
equations r =
    let (bexpr, next) = runState (collectEquation r) Map.empty
    in (bexpr, collectEquations next)

collectEquations :: Map.Map Int (RR s)-> Map.Map Int BoolExpr
collectEquations = go Map.empty where
    go acc queue = case Map.minViewWithKey queue of
        Nothing               -> acc
        Just ((i, r), queue')
            | Map.member i acc -> go acc queue'
            | otherwise        ->
                let (bexpr, next) = runState (collectEquation r) Map.empty
                in go (Map.insert i bexpr acc) (queue' <> next)

collectEquation :: RR s -> State (Map.Map Int (RR s)) BoolExpr
collectEquation Eps       = return BTrue
collectEquation (Ch _)    = return BFalse
collectEquation (App r s) = band <$> collectEquation r <*> collectEquation s
collectEquation (Alt r s) = bor <$> collectEquation r <*> collectEquation s
collectEquation (Star _)  = return BTrue
collectEquation (Ref i _ r) = do
    modify (Map.insert i r)
    return (BVar i)
#ifdef RERE_INTERSECTION
collectEquation (And r s) = band <$> collectEquation r <*> collectEquation s
#endif

lfp :: BoolExpr -> Map.Map Int BoolExpr -> Bool
lfp b exprs = go (False <$ exprs) where
    go curr
        | curr == next = evaluate b
        | otherwise    = go next
      where
        next = fmap evaluate exprs

        evaluate :: BoolExpr -> Bool
        evaluate BTrue      = True
        evaluate BFalse     = False
        evaluate (BOr x y)  = evaluate x || evaluate y
        evaluate (BAnd x y) = evaluate x && evaluate y
        evaluate (BVar i)   = Map.findWithDefault False i curr

-------------------------------------------------------------------------------
-- BoolExpr
-------------------------------------------------------------------------------

data BoolExpr
    = BVar Int
    | BTrue
    | BFalse
    | BOr BoolExpr BoolExpr
    | BAnd BoolExpr BoolExpr
  deriving (Show)

band :: BoolExpr -> BoolExpr -> BoolExpr
band BFalse _      = BFalse
band _      BFalse = BFalse
band BTrue  r      = r
band r      BTrue  = r
band r      s      = BAnd r s

bor :: BoolExpr -> BoolExpr -> BoolExpr
bor BFalse r      = r
bor r      BFalse = r
bor BTrue  _      = BTrue
bor _      BTrue  = BTrue
bor r      s      = BOr r s
