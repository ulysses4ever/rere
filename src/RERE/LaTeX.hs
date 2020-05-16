{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifndef RERE_NO_CFG
{-# LANGUAGE Trustworthy       #-}
#elif __GLASGOW_HASKELL__ >=704
{-# LANGUAGE Safe                #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy         #-}
#endif
-- | Pretty-print structures as LaTeX code.
--
-- Note: doesn't work with MathJax.
--
-- Requires rere.sty distributed with this package, or definition of
-- the macros that are provided by rere.sty in some other way.
--
module RERE.LaTeX (
    putLatex,
    putLatexTrace,
#ifndef RERE_NO_CFG
    putLatexCFG,
#endif
    ) where

import Control.Monad.Trans.State (State, evalState, get, put)
import Data.Char                 (ord)
import Data.Foldable             (for_)
import Data.List                 (intersperse)
import Data.Set                  (Set)
import Data.String               (IsString (..))
import Data.Void                 (Void)

import qualified Data.Set     as Set
import qualified RERE.CharSet as CS

import RERE.Absurd
import RERE.Type
import RERE.Var

#ifndef RERE_NO_CFG
import RERE.CFG

import           Data.Vec.Lazy (Vec (..))
import qualified Data.Vec.Lazy as V
#endif

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid (..))
#endif

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- | Pretty-print 'RE' as LaTeX code.
putLatex :: RE Void -> IO ()
putLatex = putStrLn . latexify

-------------------------------------------------------------------------------
-- Latex utilities
-------------------------------------------------------------------------------

data Prec
    = BotPrec
    | AltPrec
#ifdef RERE_INTERSECTION
    | AndPrec
#endif
    | AppPrec
    | StarPrec
  deriving (Eq, Ord, Enum, Show)

-- | The Booleans indicate whether the piece needs spacing if
-- combined with another spacing-sensitive piece before and/or
-- after.
--
data Piece = Piece !Bool !Bool ShowS

instance IsString Piece where
    fromString = piece . showString

piece :: ShowS -> Piece
piece = Piece False False

-- | Modify a piece, preserving the underlying piece's spacing
-- behaviour.
--
preserve :: (Piece -> Piece) -> Piece -> Piece
preserve f p@(Piece a b _) =
  Piece a b (unPiece (f p))

unPiece :: Piece -> ShowS
unPiece (Piece _ _ ss) = ss

instance Semigroup Piece where
    Piece a b x <> Piece c d y = Piece a d (x . sep . y) where
        sep | b, c      = showString rerespace
            | otherwise = id

instance Monoid Piece where
    mempty  = Piece False False id
    mappend = (<>)

latexify :: RE Void -> String
latexify re0 = unPiece (evalState (latexify' (vacuous re0)) Set.empty) ""

latexCS :: (IsString a, Monoid a) => String -> Maybe a -> [a] -> a
latexCS csname Nothing [] = mconcat
 -- add extra space after plain csname to ensure there is no letter directly following
    [ "\\", fromString csname, " " ]
latexCS csname optarg args = mconcat $
    [ "\\", fromString csname, optwrap optarg ] ++ map wrap args
  where
    optwrap Nothing    = mempty
    optwrap (Just arg) = mconcat ["[", arg, "]"]

    wrap arg = "{" `mappend` arg `mappend` "}"

preservingLatexCS :: String -> Maybe Piece -> Piece -> Piece
preservingLatexCS csname optarg arg =
    preserve (latexCS csname optarg . return) arg

latexBegin :: String -> Piece
latexBegin envname =
    latexCS "begin" Nothing [fromString envname]

latexEnd :: String -> Piece
latexEnd envname =
    latexCS "end" Nothing [fromString envname]

rerespace :: (IsString a, Monoid a) => a
rerespace = latexCS "rerespace" Nothing []

rerelitset :: (IsString a, Monoid a) => a -> a
rerelitset x = latexCS "rerelitset" Nothing [x]

rerelitsetcomplement :: (IsString a, Monoid a) => a -> a
rerelitsetcomplement x = latexCS "rerelitsetcomplement" Nothing [x]

rerealt :: Piece -> Piece -> Piece
rerealt x y = latexCS "rerealt" Nothing [x, y]

rereintersect :: Piece -> Piece -> Piece
rereintersect x y = latexCS "rereintersect" Nothing [x, y]

rerestar :: Piece -> Piece
rerestar x = preservingLatexCS "rerestar" Nothing x

beginrerealignedlet :: Piece
beginrerealignedlet = latexBegin "rerealignedlet"

endrerealignedlet :: Piece
endrerealignedlet = latexEnd "rerealignedlet"

rereletreceqn :: Piece -> Piece -> Piece
rereletreceqn x y = latexCS "rereletreceqn" Nothing [x, y]

rereleteqn :: Piece -> Piece -> Piece
rereleteqn x y = latexCS "rereleteqn" Nothing [x, y]

rereletrecin :: Piece -> Piece -> Piece -> Piece
rereletrecin x y z = latexCS "rereletrecin" Nothing [x, y, z]

rereletin :: Piece -> Piece -> Piece -> Piece
rereletin x y z = latexCS "rereletin" Nothing [x, y, z]

rerefix :: Piece -> Piece -> Piece
rerefix x y = latexCS "rerefix" Nothing [x, y]

rereletbody :: Piece -> Piece
rereletbody x = latexCS "rereletbody" Nothing [x]

rerelit :: (IsString a, Monoid a) => a -> a
rerelit x = latexCS "rerelit" Nothing [x]

rerelitrange :: (IsString a, Monoid a) => a -> a -> a
rerelitrange x y = latexCS "rerelitrange" Nothing [x, y]

rerestr :: (IsString a, Monoid a) => a -> a
rerestr x = latexCS "rerestr" Nothing [x]

rerevar :: (IsString a, Monoid a) => a -> a
rerevar x = latexCS "rerevar" Nothing [x]

rerevarsub :: (IsString a, Monoid a) => a -> a -> a
rerevarsub x y = latexCS "rerevarsub" Nothing [x, y]

rerevarsubsub :: (IsString a, Monoid a) => a -> a -> a -> a
rerevarsubsub x y z = latexCS "rerevarsubsub" Nothing [x, y, z]

rerenull :: (IsString a, Monoid a) => a
rerenull = latexCS "rerenull" Nothing []

rerefull :: (IsString a, Monoid a) => a
rerefull = latexCS "rerefull" Nothing []

rereeps :: (IsString a, Monoid a) => a
rereeps = latexCS "rereeps" Nothing []

beginreretrace :: Piece
beginreretrace = latexBegin "reretrace"

endreretrace :: Piece
endreretrace = latexEnd "reretrace"

reretraceline :: Maybe Piece -> String -> String -> Piece
reretraceline o x y = latexCS "reretraceline" o [fromString x, fromString y]

beginrerecfg :: Piece
beginrerecfg = latexBegin "rerecfg"

endrerecfg :: Piece
endrerecfg = latexEnd "rerecfg"

rerecfgproduction :: Piece -> Piece -> Piece
rerecfgproduction x y = latexCS "rerecfgproduction" Nothing [x, y]

rerecharstar :: String
rerecharstar = latexCS "rerecharstar" Nothing []
rerecharplus :: String
rerecharplus = latexCS "rerecharplus" Nothing []
rerecharminus :: String
rerecharminus = latexCS "rerecharminus" Nothing []
rerecharpopen :: String
rerecharpopen = latexCS "rerecharpopen" Nothing []
rerecharpclose :: String
rerecharpclose = latexCS "rerecharpclose" Nothing []
rerecharbopen :: String
rerecharbopen = latexCS "rerecharbopen" Nothing []
rerecharbclose :: String
rerecharbclose = latexCS "rerecharbclose" Nothing []
rerecharcopen :: String
rerecharcopen = latexCS "rerecharcopen" Nothing []
rerecharcclose :: String
rerecharcclose = latexCS "rerecharcclose" Nothing []
rerecharbackslash :: String
rerecharbackslash = latexCS "rerecharbackslash" Nothing []
rerecharhash :: String
rerecharhash = latexCS "rerecharhash" Nothing []
rerechartilde :: String
rerechartilde = latexCS "rerechartilde" Nothing []
rerecharspace :: String
rerecharspace = latexCS "rerecharspace" Nothing []
rerecharampersand :: String
rerecharampersand = latexCS "rerecharampersand" Nothing []
rerecharpercent :: String
rerecharpercent = latexCS "rerecharpercent" Nothing []
rerecharunderscore :: String
rerecharunderscore = latexCS "rerecharunderscore" Nothing []
rerecharhat :: String
rerecharhat = latexCS "rerecharhat" Nothing []
rerechardollar :: String
rerechardollar = latexCS "rerechardollar" Nothing []

rerecharcode :: String -> String
rerecharcode x = latexCS "rerecharcode" Nothing [x]

nullPiece :: Piece
nullPiece = rerenull

fullPiece :: Piece
fullPiece = rerefull

epsPiece :: Piece
epsPiece = rereeps

latexify' :: RE Piece -> State (Set NI) Piece
latexify' = go BotPrec where
    go :: Prec -> RE Piece -> State (Set NI) Piece
    go _ Null    = return nullPiece
    go _ Full    = return fullPiece
    go _ Eps     = return epsPiece
    go _ (Ch cs) = return $ case CS.toIntervalList cs of
        []                   -> nullPiece
        [(lo,hi)] | lo == hi -> latexCharPiece lo
        xs | sz < sz'        -> rerelitset (mconcat (intersperse ", " $ map latexCharRange xs))
           | otherwise       -> rerelitsetcomplement (mconcat (intersperse ", " $ map latexCharRange $ CS.toIntervalList ccs))
      where
        ccs = CS.complement cs
        sz  = CS.size cs
        sz' = CS.size ccs

    go d (App r s) = parens (d > AppPrec) $ do
        r'  <- go AppPrec r
        s'  <- go AppPrec s
        return $ r' <> s' -- not via a control sequence to preserve the spacing hack

    go d (Alt r s) = parens (d > AltPrec) $ do
        r'  <- go AltPrec r
        s'  <- go AltPrec s
        return $ rerealt r' s'

#ifdef RERE_INTERSECTION
    go d (And r s) = parens (d > AndPrec) $ do
        r'  <- go AndPrec r
        s'  <- go AndPrec s
        return $ rereintersect r' s'
#endif

    go d (Star r) = parens (d > StarPrec) $ do
        r' <- go StarPrec r
        return $ rerestar r'

    go _ (Var x) = return x

    go d (Let n (Fix _ r) s@Let {}) = parens (d > BotPrec) $ do
        i <- newUnique n
        let v  = showVar n i
        let r' = fmap (unvar v id) r
        let s' = fmap (unvar v id) s

        r2 <- go BotPrec r'

        let acc = beginrerealignedlet <> rereletreceqn v r2

        goLet acc s'

    go d (Let n r s@Let {}) = parens (d > BotPrec) $ do
        i <- newUnique n
        let v  = showVar n i
        let s' = fmap (unvar v id) s

        r2 <- go BotPrec r

        let acc = beginrerealignedlet <> rereleteqn v r2

        goLet acc s'

    go d (Let n (Fix _ r) s) = parens (d > BotPrec) $ do
        i <- newUnique n
        let v  = showVar n i
        let r' = fmap (unvar v id) r
        let s' = fmap (unvar v id) s

        r2 <- go BotPrec r'
        s2 <- go BotPrec s'

        return $ rereletrecin v r2 s2

    go d (Let n r s) = parens (d > BotPrec) $ do
        i <- newUnique n
        let v  = showVar n i
        let s' = fmap (unvar v id) s

        r2 <- go BotPrec r
        s2 <- go BotPrec s'

        return $ rereletin v r2 s2

    go d (Fix n r) = parens (d > BotPrec) $ do
        i <- newUnique n
        let v  = showVar n i
        let r' = fmap (unvar v id) r

        r'' <- go BotPrec r'
        return $ rerefix v r''


    goLet :: Piece -> RE Piece -> State (Set NI) Piece
    goLet acc0 (Let n (Fix _ r) s) = do
        i <- newUnique n
        let v  = showVar n i
        let r' = fmap (unvar v id) r
        let s' = fmap (unvar v id) s

        r2 <- go BotPrec r'

        let acc = acc0 <> rereletreceqn v r2

        goLet acc s'

    goLet acc0 (Let n r s) = do
        i <- newUnique n
        let v  = showVar n i
        let s' = fmap (unvar v id) s

        r2 <- go BotPrec r

        let acc = acc0 <> rereleteqn v r2

        goLet acc s'

    goLet acc s = do
        s' <- go BotPrec s
        return $ acc <> rereletbody s' <> endrerealignedlet

    parens :: Bool -> State (Set NI) Piece -> State (Set NI) Piece
    parens True  = fmap $ \(Piece _ _ x) -> piece $ showChar '(' . x . showChar ')'
    parens False = id

latexChar :: Char -> String
latexChar c = rerelit (latexChar' c)

latexChar' :: Char -> String
latexChar' '*'  = rerecharstar
latexChar' '+'  = rerecharplus
latexChar' '-'  = rerecharminus
latexChar' '('  = rerecharpopen
latexChar' ')'  = rerecharpclose
latexChar' '['  = rerecharbopen
latexChar' ']'  = rerecharbclose
latexChar' '{'  = rerecharcopen
latexChar' '}'  = rerecharcclose
latexChar' '\\' = rerecharbackslash
latexChar' '#'  = rerecharhash
latexChar' '~'  = rerechartilde
latexChar' ' '  = rerecharspace
latexChar' '&'  = rerecharampersand
latexChar' '%'  = rerecharpercent
latexChar' '_'  = rerecharunderscore
latexChar' '^'  = rerecharhat
latexChar' '$'  = rerechardollar
latexChar' c
    | c <= '\x20' || c >= '\127' = rerecharcode (show (ord c))
    | otherwise                  = [c]

latexCharPiece :: Char -> Piece
latexCharPiece c = fromString (latexChar c)

latexCharRange :: (Char, Char) -> Piece
latexCharRange (lo, hi)
    | lo == hi  = latexCharPiece lo
    | otherwise = rerelitrange (latexCharPiece lo) (latexCharPiece hi)

data NI = NI String [Char] Int deriving (Eq, Ord)

newUnique :: Name -> State (Set NI) Int
newUnique (N n cs) = get >>= go 0 where
    go i s | Set.member (NI n cs i) s = go (i + 1) s
           | otherwise = do
        put (Set.insert (NI n cs i) s)
        return i

latexString :: String -> String
latexString cs = rerestr (concatMap latexChar' cs)

showVar :: Name -> Int -> Piece
showVar (N n cs) i
    = Piece True True $ showString var
  where
    cs' = latexString cs
    i'  = showI i

    var :: String
    var | null cs && null i'             = rerevar n
        | not (null cs) && not (null i') = rerevarsubsub n cs' i'
        | otherwise                      = rerevarsub n (cs' <> i')

    showI :: Int -> String
    showI 0 = ""
    showI j = show j

-------------------------------------------------------------------------------
-- Trace
-------------------------------------------------------------------------------

-- | Run 'match' variant, collect intermediate steps, and
-- pretty-print that trace.
--
putLatexTrace :: RE Void -> String -> IO ()
putLatexTrace re str = displayTrace (traced re str)

traced :: RE Void -> String -> (Bool, RE Void, [(String, RE Void)])
traced = go id where
    go acc re []         = (nullable re, re, acc [])
    go acc re str@(c:cs) = go (acc . ((str, re) :)) (derivative c re) cs

putPieceLn :: Piece -> IO ()
putPieceLn = putStrLn . ($ "") . unPiece

displayTrace :: (Bool, RE Void, [(String, RE Void)]) -> IO ()
displayTrace (matched, final, steps) = do
    putPieceLn beginreretrace
    for_ steps $ \(str, re) ->
        putPieceLn $ reretraceline (Just (sub (nullable re))) (latexString str) (latexify re)
    putPieceLn $ reretraceline (Just (sub matched)) rereeps (latexify final)
    putPieceLn endreretrace

    print matched
    print final

  where
    -- sub True  = "_\\varepsilon"
    -- sub False = "_\\kappa"
    sub _ = ""

-------------------------------------------------------------------------------
-- CFG
-------------------------------------------------------------------------------

#ifndef RERE_NO_CFG
-- | Pretty-print 'CFG' given the names.
putLatexCFG :: Vec n Name -> CFG n Void -> IO ()
putLatexCFG names cfg = mapM_ putPieceLn (latexifyCfg names cfg)

latexifyCfg :: forall n. Vec n Name -> CFG n Void -> [Piece]
latexifyCfg names cfg =
    [beginrerecfg] ++ go names cfg ++ [endrerecfg]
  where
    initS :: State (Set NI) ()
    initS = for_ names newUnique

    go :: Vec m Name -> Vec m (CFGBase n Void) -> [Piece]
    go VNil       VNil       = []
    go (n ::: ns) (e ::: es) = eq' : go ns es where
        e' = fmap (either (\i -> showVar (names V.! i) 0) absurd) e
        n' = showVar n 0

        eq = do
            initS
            e'' <- latexify' e'
            return $ rerecfgproduction n' e''

        eq' :: Piece
        eq' = evalState eq Set.empty
#if __GLASGOW_HASKELL__  <711
    go _ _ = error "silly GHC"
#endif
#endif
