{-# LANGUAGE LambdaCase #-}
module RERE.ST where

import Data.Void (Void, vacuous)
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))

import RERE
import RERE.Var
import RERE.DepMap
import RERE.Tuples

import qualified Data.Map.Strict as M

import Control.Monad.Trans.State

import Debug.Trace

data RE' b =  RE' !Char (RE (Triple Bool b b))
  deriving (Eq, Ord)

instance Ord1 RE' where
    compare1 = compare

data S = S
    { sNullable   :: !(M.Map (RE Bool) Bool)
    , sDerivative :: !(DepMap RE' RE)
    }

emptyS :: S
emptyS = S M.empty dpEmpty

type ST = State S

nullableS :: RE Void -> ST Bool
nullableS = nullableS' . vacuous

nullableS' :: RE Bool -> ST Bool
nullableS' Null     = return False
nullableS' Eps      = return True
nullableS' (Ch _)   = return False
nullableS' (Star _) = return True
nullableS' (Var a)  = return a
nullableS' r0@(Let _ r s) = recordNullable r0 $ do
    r' <- nullableS' r
    nullableS' (fmap (unvar r' id) s)
nullableS' r0@(Fix _ r1) = recordNullable r0 $ nullableS' (fmap (unvar False id) r1)
nullableS' r0@(App r s)  = recordNullable r0 $ nullableS' r .&& nullableS' s
nullableS' r0@(Alt r s)  = recordNullable r0 $ nullableS' r .|| nullableS' s

derivativeS :: Char -> RE Void -> ST (RE Void)
derivativeS c = go' . vacuous where
    go' :: SNI b => RE (Triple Bool b b) -> ST (RE b)
    go' Null = return Null
    go' Eps = return Null
    go' (Ch x)
      | c == x    = return Eps
      | otherwise = return Null
    go' (Var x) = return $ Var (sndOf3 x)

    go' r0@(App r s) = recordDerivative c r0 $ do
        n <- nullableS' (fmap fstOf3 r)
        if n
        then do
            r' <- go' r
            s' <- go' s
            return $ s' \/ r' <> fmap trdOf3 s
        else do
            r' <- go' r
            return $ r' <> fmap trdOf3 s

 
    go' r0@(Alt r s) = recordDerivative c r0 $ do
        r' <- go' r
        s' <- go' s
        return $ r' \/ s'

    go' r0@(Star r) = recordDerivative c r0 $ do
        r' <- go' r
        return $ r' <> fmap trdOf3 r0

    go' r0@(Let n r s) = recordDerivative c r0 $ case unused s of
        Just s' -> do
            s'' <- go' (fmap (bimap F F) s')
            return $ let_ n
                (fmap trdOf3 r)
                s''

        Nothing -> do
            nu <- nullableS' (fmap fstOf3 r)
            r' <- go' r
            s' <- go' $ s <&> \case
                B   -> T nu B (F B)
                F x -> bimap (F . F) (F . F) x
            return
                $ let_ n (fmap trdOf3 r)
                $ let_ n' (fmap F r') s'
      where
        n' = derivativeName c n

    go' r0@(Fix n r) = recordDerivative c r0 $ do
        nu <- nullableS' (fmap fstOf3 r0)
        r' <- go' $ r <&> \case
            B   -> T nu B (F B)
            F x -> bimap (F . F) (F . F) x
        return
            $ let_ n (fmap trdOf3 r0)
            $ fix_ n' r'
      where
        n' = derivativeName c n

-------------------------------------------------------------------------------
-- Match
-------------------------------------------------------------------------------

matchS :: RE Void -> String -> Bool
matchS r str = evalState (go r str) emptyS  where
    go r []     = nullableS r
    go r (c:cs) = do
        r' <- derivativeS c r
        go r' cs

-------------------------------------------------------------------------------
-- Record
-------------------------------------------------------------------------------

recordNullable :: RE Bool -> ST Bool -> ST Bool
recordNullable r result = do
    s <- get
    case M.lookup r (sNullable s) of
        Just b -> return b
        Nothing -> do
            b <- result
            modify' $ \s' -> s' { sNullable = M.insert r b (sNullable s') }
            return b

recordDerivative :: SNI b => Char -> RE (Triple Bool b b) -> ST (RE b)  -> ST (RE b)
recordDerivative c r result = do
    let r' = RE' c r

    s <- get
    case dmLookup r' (sDerivative s) of
        Just b -> return b
        Nothing -> do
            b <- result
            modify' $ \s' -> s' { sDerivative = dmInsert r' b (sDerivative s') }
            return b

-------------------------------------------------------------------------------
-- monad extras
-------------------------------------------------------------------------------

(.&&) :: Monad m => m Bool -> m Bool -> m Bool
x .&& y = do
    x'  <- x
    if x'
    then y
    else return False

(.||) :: Monad m => m Bool -> m Bool -> m Bool
x .|| y = do
    x' <- x
    if x'
    then return True
    else y
