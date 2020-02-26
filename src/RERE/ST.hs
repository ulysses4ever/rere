{-# LANGUAGE CPP                 #-}
-- #define RERE_DEBUG

{-# LANGUAGE ScopedTypeVariables #-}

#ifdef RERE_DEBUG
#if __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy         #-}
#endif
#else
#if __GLASGOW_HASKELL__ >=710
{-# LANGUAGE Safe                #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy         #-}
#endif
#endif

-- | Regular expression with explicit sharing.
--
-- 'RST' is an opaque type, to maintain the invariants.
module RERE.ST (
    RST,
    matchST,
    matchDebugST,
    ) where

#ifdef RERE_DEBUG
import Debug.Trace
#endif

import Control.Monad.Fix         (mfix)
import Control.Monad.Trans.State (State, evalState, get, modify, put, runState)
import Data.Void                 (Void, vacuous)
import Data.Word                 (Word64)

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

-- Alt (Ch "b") (Alt (App (Alt (Star (Ch "c")) (Ch "c")) Full) (Ch "de"))
-- Alt Null (App Eps Full)

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

matchIter :: Int
matchIter = 20

nullableIter :: Int
nullableIter = 10

-------------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------------

-- | Knot-tied recursive regular expression.
data RST s = RST
    { _rrDef        :: Def s
    , _rrId         :: !Word64
    ,  rrDerivative :: !(Char -> ST s (RST s))
    ,  rrCompact    :: !(ST s (RST s))
    }

data Def s
    = Eps
    | Full
    | Ch CS.CharSet
    | App (RST s) (RST s)
    | Alt (RST s) (RST s)
#ifdef RERE_INTERSECTION
    | And (RST s) (RST s)
#endif
    | Star (RST s)

    | Del (RST s)

-------------------------------------------------------------------------------
-- Make
-------------------------------------------------------------------------------

data Ctx s = Ctx
    { ctxId  :: STRef s Word64
    , ctxNull :: RST s
    , ctxFull :: RST s
    , ctxEps  :: RST s
    }

newCtx :: ST s (Ctx s)
newCtx = do
    i <- newSTRef 3
    let n = RST (Ch CS.empty) 0 (\_ -> return n) (return n)
    let f = RST Full          1 (\_ -> return f) (return f)
        e = RST Eps           2 (\_ -> return n) (return e)

    return (Ctx i n f e)

makeRST :: Ctx s -> Def s -> ST s (RST s)
makeRST ctx def = do
    i <- readSTRef (ctxId ctx)
    writeSTRef (ctxId ctx) (i + 1)
    dref <- newSTRef Map.empty
    cref <- newSTRef Nothing

    let d ch = do
            m <- readSTRef dref
            case Map.lookup ch m of
                Just x -> return x
                Nothing -> mfix $ \deriv -> do
                    writeSTRef dref (Map.insert ch deriv m)
                    derivativeDef ctx ch def

    let c = do
          mcompacted <- readSTRef cref
          case mcompacted of
              Just compacted -> return compacted
              Nothing        -> mfix $ \compacted -> do
                  writeSTRef cref (Just compacted)
                  compactDef ctx def

    return (RST def i d c)

-------------------------------------------------------------------------------
-- Show
-------------------------------------------------------------------------------

instance Show (RST s) where
    showsPrec = go Set.empty where
        go :: Set.Set Word64 -> Int -> RST s -> ShowS
        go past d (RST def i _ _) =
            if Set.member i past
            then showString "<<loop " . shows i . showString ">>"
            else go' (Set.insert i past) d i def

        go' :: Set.Set Word64 -> Int -> Word64 -> Def s -> ShowS
        go' _    _ _ Eps       = showString "Eps"
        go' _    _ _ Full      = showString "Full"
        go' _    d _ (Ch c)    = showParen (d > 10) $ showString "Ch " . showsPrec 11 c
        go' past d i (App r s)
            = showParen (d > 10)
            $ showString "App"
            . showSub i
            . showChar ' ' . go past 11 r
            . showChar ' ' . go past 11 s
        go' past d i (Alt r s)
            = showParen (d > 10)
            $ showString "Alt"
            . showSub i
            . showChar ' ' . go past 11 r
            . showChar ' ' . go past 11 s
#ifdef RERE_INTERSECTION
        go' past d i (And r s)
            = showParen (d > 10)
            $ showString "And"
            . showSub i
            . showChar ' ' . go past 11 r
            . showChar ' ' . go past 11 s
#endif
        go' past d i (Star r)
            = showParen (d > 10)
            $ showString "Star"
            . showSub i
            . showChar ' ' . go past 11 r

        go' past d i (Del r)
            = showParen (d > 10)
            $ showString "Del"
            . showSub i
            . showChar ' ' . go past 11 r

        showSub i = showChar '_' . shows i

_size :: RST s -> Int
_size rr = evalState (go rr) Set.empty where
    go (RST def i _ _) = do
        visited <- get
        if Set.member i visited
        then return 1
        else do
            put (Set.insert i visited)
            succ <$> go' def

    go' Eps       = return 0
    go' Full      = return 0
    go' (Ch _)    = return 0
    go' (App r s) = plus1 <$> go r <*> go s
    go' (Alt r s) = plus1 <$> go r <*> go s
#ifdef RERE_INTERSECTION
    go' (And r s) = plus1 <$> go r <*> go s
#endif
    go' (Star r)  = succ <$> go r
    go' (Del r)   = succ <$> go r

    plus1 x y = succ (x + y)

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

-- | Convert 'R.RE' to 'RST'.
fromRE :: forall s. Ctx s -> R.RE Void -> ST s (RST s)
fromRE ctx re = go (vacuous re) where
    go :: R.RE (RST s) -> ST s (RST s)

    go R.Null   = return (ctxNull ctx)
    go R.Full   = return (ctxFull ctx)
    go R.Eps    = return (ctxEps ctx)
    go (R.Ch c)
        | CS.null c = return (ctxNull ctx)
        | otherwise = makeRST ctx (Ch c)

    go (R.App r s) = do
        r' <- go r
        s' <- go s
        makeRST ctx (App r' s')

    go (R.Alt r s) = do
        r' <- go r
        s' <- go s
        makeRST ctx (Alt r' s')

#ifdef RERE_INTERSECTION
    go (R.And r s) = do
        r' <- go r
        s' <- go s
        makeRST ctx (And r' s')
#endif

    go (R.Star r) = do
        r' <- go r
        makeRST ctx (Star r')

    go (R.Var r) = return r

    go (R.Let _ r s) = do
        r' <- go r
        go (fmap (unvar r' id) s)

    go (R.Fix _ r) = mfix $ \res -> do
        go (fmap (unvar res id) r)

-------------------------------------------------------------------------------
-- Match
-------------------------------------------------------------------------------

-- | Convert 'R.RE' to 'RST' and then match.
--
-- Significantly faster than 'RERE.Type.match'.
matchST :: R.RE Void -> String -> Bool
matchST re str = runST go0
  where
    go0 :: ST s Bool
    go0 = do
        ctx <- newCtx
        rr <- fromRE ctx re
        let cc = charClasses re
        go ctx cc str rr

    go :: Ctx s -> CharClasses -> String -> RST s -> ST s Bool
    go ctx _  []     rr = nullableR' ctx rr
    go ctx cc (c:cs) rr = do
        let c' = classOfChar cc c
        rr' <- derivativeR c' rr
        rr'' <- compactRN matchIter rr'
#ifdef RERE_DEBUG
        let size1 = _size rr'
            size2 = _size rr''
        traceM ("size: " ++ show size1 ++ " ~> " ++ show size2)
        if size1 < size2
        then traceM (show rr')
        else pure ()
#endif
        go ctx cc cs rr''

matchDebugST :: R.RE Void -> String -> IO ()
matchDebugST re str = runST go0 where
    go0 :: ST s (IO ())
    go0 = do
        ctx <- newCtx
        rr <- fromRE ctx re
        go ctx str rr

    go :: Ctx s -> String -> RST s -> ST s (IO ())
    go ctx [] rr = do
        n <- nullableR' ctx rr

        -- rn <- makeRST ctx (Del rr)
        -- (rn', trace) <- compactRTrace nullableIter rn

        return $ putStr $ unlines $
            [ "size: " ++ show (_size rr)
            , "show: " ++ show rr
            , "null: " ++ show n
            ]
            {-
            , "nul2: " ++ show (nullableR rr)
            , "dels: " ++ show rn'
            ] ++
            [ "    - " ++ show t
            | t <- trace
            ]
            -}

    go ctx (c:cs) rr = do
        rr' <- derivativeR c rr
        rr'' <- compactRN matchIter rr'
        go ctx cs rr''


-------------------------------------------------------------------------------
-- Compact
-------------------------------------------------------------------------------

compactR :: RST s -> ST s (RST s)
compactR = rrCompact

compactDef :: Ctx s -> Def s -> ST s (RST s)
compactDef ctx r0 = case r0 of
    Eps  -> return (ctxEps ctx)
    Full -> return (ctxFull ctx)
    Ch cs | CS.null cs -> return (ctxNull ctx)
          | otherwise  -> makeRST ctx r0

    Alt (RST (Ch x) _ _ _) (RST (Ch y) _ _ _) ->
        makeRST ctx (Ch (CS.union x y))
    Alt (RST (Ch x) _ _ _) s | CS.null x ->
        compactR s
    Alt r (RST (Ch x) _ _ _) | CS.null x ->
        compactR r
    Alt (RST Full _ _ _) _ ->
        return (ctxFull ctx)
    Alt _ (RST Full _ _ _) ->
        return (ctxFull ctx)
    Alt (RST Eps _ _ _) (RST Eps _ _ _) ->
        return (ctxEps ctx)
    Alt r s -> do
        r' <- compactR r
        s' <- compactR s
        makeRST ctx (Alt r' s')

    App (RST Eps _ _ _) s ->
        compactR s
    App r (RST Eps _ _ _) ->
        compactR r
    App (RST (Ch x) _ _ _) _ | CS.null x ->
        return (ctxNull ctx)
    App _ (RST (Ch x) _ _ _) | CS.null x ->
        return (ctxNull ctx)
    App r s -> do
        r' <- compactR r
        s' <- compactR s
        makeRST ctx (App r' s')

#ifdef RERE_INTERSECTION
    And r s -> do
        r' <- compactR r
        s' <- compactR s
        makeRST ctx (And r' s')
#endif

    Star (RST (Ch x) _ _ _) | CS.null x ->
        return (ctxEps ctx)
    Star (RST Eps _ _ _) ->
        return (ctxEps ctx)
    Star r@(RST Star {} _ _ _) ->
        compactR r
    Star r -> do
        r' <- compactR r
        makeRST ctx (Star r')

    Del (RST Full _ _ _ )      -> return (ctxEps ctx)
    Del (RST (Star _) _ _ _ )  -> return (ctxEps ctx)
    Del (RST Eps _ _ _ )       -> return (ctxEps ctx)
    Del (RST (Ch _) _ _ _)     -> return (ctxNull ctx)
    Del r@(RST (Del _) _ _ _ ) -> return r

    Del (RST (App r s) _ _ _) -> do
        r' <- makeRST ctx (Del r)
        s' <- makeRST ctx (Del s)
        makeRST ctx (App r' s')
    Del (RST (Alt r s) _ _ _) -> do
        r' <- makeRST ctx (Del r)
        s' <- makeRST ctx (Del s)
        makeRST ctx (Alt r' s')
#ifdef RERE_INTERSECTION
    Del (RST (And r s) _ _ _) -> do
        r' <- makeRST ctx (Del r)
        s' <- makeRST ctx (Del s)
        makeRST ctx (And r' s')
#endif

compactRN :: Int ->  RST s -> ST s (RST s)
compactRN n rr | n <= 0 = return rr
               | otherwise = compactR rr >>= compactRN (n - 1)

_compactRTrace :: Int -> RST s -> ST s (RST s, [RST s])
_compactRTrace n rr
    | n <= 0 = return (rr, [])
    | otherwise = do
        rr' <- compactR rr
        (rr'', tr) <- _compactRTrace (n - 1) rr'
        return (rr'', rr : tr)

-------------------------------------------------------------------------------
-- Derivative
-------------------------------------------------------------------------------

derivativeR :: Char -> RST s -> ST s (RST s)
derivativeR = flip rrDerivative

derivativeDef :: Ctx s -> Char -> Def s -> ST s (RST s)
derivativeDef ctx _ Eps =
    return (ctxNull ctx)
derivativeDef ctx _ Full =
    return (ctxFull ctx)
derivativeDef ctx _ (Del _) = do
    return (ctxNull ctx)
derivativeDef ctx c (Ch x)
    | CS.member c x = return (ctxEps ctx)
    | otherwise     = return (ctxNull ctx)
derivativeDef ctx c (Alt r s) = do
    r' <- derivativeR c r
    s' <- derivativeR c s
    makeRST ctx (Alt r' s')
#ifdef RERE_INTERSECTION
derivativeDef ctx c (And r s) = do
    r' <- derivativeR c r
    s' <- derivativeR c s
    makeRST ctx (And r' s')
#endif
derivativeDef ctx c (Star r) = do
    r' <- derivativeR c r
    starR <- makeRST ctx (Star r)
    makeRST ctx (App r' starR)
derivativeDef ctx c (App r s) = do
    r' <- derivativeR c r
    s' <- derivativeR c s

    dr <- makeRST ctx (Del r)

    lft <- makeRST ctx (App dr s')
    rgt <- makeRST ctx (App r' s)

    makeRST ctx (Alt lft rgt)

-------------------------------------------------------------------------------
-- Nullable equations
-------------------------------------------------------------------------------

-- | Whether 'RST' is nullable.
--
-- @
-- 'R.nullable' re = 'nullableR' ('fromRE' re)
-- @
nullableR :: RST s -> Bool
nullableR r =
    let (bexpr, eqs) = equations r
    in lfp bexpr eqs

nullableR' :: Ctx s -> RST s -> ST s Bool
nullableR' ctx rr = makeRST ctx (Del rr) >>= go nullableIter where
    go _ (RST Eps     _ _ _) = return True
    go _ (RST (Ch _)  _ _ _) = return False

    go n rr' | n <= 0 = return (nullableR rr')
             | otherwise = compactR rr' >>= go (n - 1)

equations :: RST s -> (BoolExpr, Map.Map Word64 BoolExpr)
equations r =
    let (bexpr, next) = runState (collectEquation r) Map.empty
    in (bexpr, collectEquations next)

collectEquations :: Map.Map Word64 (Def s)-> Map.Map Word64 BoolExpr
collectEquations = go Map.empty where
    go acc queue = case Map.minViewWithKey queue of
        Nothing               -> acc
        Just ((i, r), queue')
            | Map.member i acc -> go acc queue'
            | otherwise        ->
                let (bexpr, next) = runState (collectEquation' r) Map.empty
                in go (Map.insert i bexpr acc) (queue' <> next)

collectEquation :: RST s -> State (Map.Map Word64 (Def s)) BoolExpr
collectEquation (RST def i _ _) = do
    modify (Map.insert i def)
    return (BVar i)

collectEquation' :: Def s -> State (Map.Map Word64 (Def s)) BoolExpr
collectEquation' Eps       = return BTrue
collectEquation' Full      = return BTrue
collectEquation' (Ch _)    = return BFalse
collectEquation' (Del r)   = collectEquation r
collectEquation' (App r s) = band <$> collectEquation r <*> collectEquation s
collectEquation' (Alt r s) = bor <$> collectEquation r <*> collectEquation s
collectEquation' (Star _)  = return BTrue
#ifdef RERE_INTERSECTION
collectEquation' (And r s) = band <$> collectEquation r <*> collectEquation s
#endif

lfp :: BoolExpr -> Map.Map Word64 BoolExpr -> Bool
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
    = BVar Word64
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
