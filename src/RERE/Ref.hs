{-# LANGUAGE CPP          #-}
#if __GLASGOW_HASKELL__ >=704
{-# LANGUAGE Safe         #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy  #-}
#endif
-- | Regular expression with explicit sharing.
--
-- 'RR' is an opaque type, to maintain the invariants.
module RERE.Ref (
    RR,
    fromRE,
    nullableR,
    derivativeR,
    matchR,
    ) where


import Control.Monad.Fix         (mfix)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
       (State, StateT, evalState, evalStateT, get, modify, put, runState)
import Data.Void                 (Void, vacuous)

import qualified Data.Map as Map
import qualified Data.Set as Set

import           RERE.CharSet
import qualified RERE.Type    as R
import           RERE.Var

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$), (<$>), (<*>))
#endif

#if !MIN_VERSION_base(4,10,0)
import Data.Semigroup (Semigroup (..))
#endif

-------------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------------

-- | Knot-tied recursive regular expression.
data RR
    = Eps
    | Ch CharSet
    | App RR RR
    | Alt RR RR
    | Star RR
    | Ref !Int RR

instance Show RR where
    showsPrec = go Set.empty where
        go :: Set.Set Int -> Int -> RR -> ShowS
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
        go past d (Star r)
            = showParen (d > 10)
            $ showString "Star"
            . showChar ' ' . go past 11 r

        go past d (Ref i r)
            | Set.member i past = showParen (d > 10)
            $ showString "Ref " . showsPrec 11 i . showString " <<loop>>"
            | otherwise         = showParen (d > 10)
            $ showString "Ref " . showsPrec 11 i . showChar ' ' . go (Set.insert i past) 11 r

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

-- | Convert 'R.RE' to 'RR'.
fromRE :: R.RE Void -> RR
fromRE r = evalState (fromRE' r) 0

fromRE' :: R.RE Void -> M RR
fromRE' re = go (vacuous re) where
    go R.Null   = return nullRR
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

    go (R.Star r) = do
        r' <- go r
        star_ r'

    go (R.Var r) = return r

    go (R.Let _ r s) = do
        r' <- go r
        i <- newId
        go (fmap (unvar (Ref i r') id) s)

    go (R.Fix _ r) = mfix $ \res -> do
        i <- newId
        r' <- go (fmap (unvar res id) r)
        return (Ref i r')

-------------------------------------------------------------------------------
-- Variable supply monad
-------------------------------------------------------------------------------

type M = State Int

newId :: M Int
newId = do
    i <- get
    put $! i + 1
    return i

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

nullRR :: RR
nullRR = Ch empty

isNull :: RR -> Bool
isNull (Ch c) = isEmpty c
isNull _      = False

app_ :: RR -> RR -> RR
app_ r    _    | isNull r = r
app_ _    r    | isNull r = r
app_ Eps  r    = r
app_ r    Eps  = r
app_ r    s    = App r s

alt_ :: RR -> RR -> RR
alt_ r      s      | isNull r = s
alt_ r      s      | isNull s = r
alt_ (Ch a) (Ch b) = Ch (union a b)
alt_ r      s      = Alt r s

star_ :: RR -> M RR
star_ r          | isNull r
                 = return Eps
star_ Eps        = return Eps
star_ r@(Star _) = return r
star_ r          = return (Star r)

-------------------------------------------------------------------------------
-- Match
-------------------------------------------------------------------------------

-- | Convert 'R.RE' to 'RR' and then match.
--
-- Significantly faster than 'RERE.Type.match'.
--
matchR :: R.RE Void -> String -> Bool
matchR re str = evalState (fromRE' re >>= go str) 0 where
    go []     rr = return $ nullableR rr
    go (c:cs) rr = do
        rr' <- derivative c rr
        go cs rr'

-------------------------------------------------------------------------------
-- Derivative
-------------------------------------------------------------------------------

derivative :: Char -> RR -> M RR
derivative c rr = evalStateT (go rr) Map.empty where
    go :: RR -> StateT (Map.Map Int RR) M RR
    go Eps                 = return nullRR
    go (Ch x) | member c x = return Eps
              | otherwise  = return nullRR
    go (Alt r s) = do
        r' <- go r
        s' <- go s
        return (Alt r' s')

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

    go (Ref i r) = do
        m <- get
        case Map.lookup i m of
            Just r' -> return r'
            Nothing -> mfix $ \res -> do
                j <- lift newId
                put (Map.insert i res m)
                r' <- go r
                return (Ref j r')

-- | Convert 'R.RE' to 'RR' and differentiate once.
--
-- @
-- 'R.nullable' ('R.derivative' c re) = 'nullableR' ('derivativeR' c re)
-- @
derivativeR :: Char -> R.RE Void -> RR
derivativeR c re = evalState action 0 where
    action = do
        rr <- fromRE' re
        derivative c rr

-------------------------------------------------------------------------------
-- Nullable equations
-------------------------------------------------------------------------------

-- | Whether 'RR' is nullable.
--
-- @
-- 'R.nullable' re = 'nullableR' ('fromRE' re)
-- @
nullableR :: RR -> Bool
nullableR r =
    let (bexpr, eqs) = equations r
    in lfp bexpr eqs

equations :: RR -> (BoolExpr, Map.Map Int BoolExpr)
equations r =
    let (bexpr, next) = runState (collectEquation r) Map.empty
    in (bexpr, collectEquations next)

collectEquations :: Map.Map Int RR -> Map.Map Int BoolExpr
collectEquations = go Map.empty where
    go acc queue = case Map.minViewWithKey queue of
        Nothing               -> acc
        Just ((i, r), queue')
            | Map.member i acc -> go acc queue'
            | otherwise        ->
                let (bexpr, next) = runState (collectEquation r) Map.empty
                in go (Map.insert i bexpr acc) (queue' <> next)

collectEquation :: RR -> State (Map.Map Int RR) BoolExpr
collectEquation Eps       = return BTrue
collectEquation (Ch _)    = return  BFalse
collectEquation (App r s) = band <$> collectEquation r <*> collectEquation s
collectEquation (Alt r s) = bor <$> collectEquation r <*> collectEquation s
collectEquation (Star _)  = return BTrue
collectEquation (Ref i r) = do
    modify (Map.insert i r)
    return (BVar i)


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
