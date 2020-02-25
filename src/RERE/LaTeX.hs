{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifdef RERE_CFG
{-# LANGUAGE Trustworthy       #-}
#elif __GLASGOW_HASKELL__ >=704
{-# LANGUAGE Safe              #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy       #-}
#endif
-- | Pretty-print structures as LaTeX code.
module RERE.LaTeX (
    putLatex,
    putLatexTrace,
#ifdef RERE_CFG
    putLatexCFG,
#endif
    ) where

import Control.Monad.Trans.State (State, evalState, get, put)
import Data.Foldable             (for_)
import Data.List                 (intersperse)
import Data.Set                  (Set)
import Data.String               (IsString (..))
import Data.Void                 (Void)

import qualified Data.Set as Set

import RERE.Absurd
import RERE.CharSet
import RERE.Type
import RERE.Var

#ifdef RERE_CFG
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

data Piece = Piece !Bool !Bool ShowS

instance IsString Piece where
    fromString = piece . showString

piece :: ShowS -> Piece
piece = Piece False False

unPiece :: Piece -> ShowS
unPiece (Piece _ _ ss) = ss

instance Semigroup Piece where
    Piece a b x <> Piece c d y = Piece a d (x . sep . y) where
        sep | b, c      = showString "\\,"
            | otherwise = id

instance Monoid Piece where
    mempty  = Piece False False id
    mappend = (<>)

latexify :: RE Void -> String
latexify re0 = unPiece (evalState (latexify' (vacuous re0)) Set.empty) ""

nullPiece :: Piece
nullPiece = "{\\color{red!80!black}\\emptyset}"

fullPiece :: Piece
fullPiece = "{\\color{red!80!black}\\Sigma}"

literalColor :: Piece -> Piece
literalColor c = "\\color{green!50!black}" <> c

latexify' :: RE Piece -> State (Set NI) Piece
latexify' = go BotPrec where
    go :: Prec -> RE Piece -> State (Set NI) Piece
    go _ Null    = return nullPiece
    go _ Full    = return fullPiece
    go _ Eps     = return $ piece $ showString "{\\color{red!80!black}\\varepsilon}"
    go _ (Ch cs) = case toIntervalList cs of
        []                   -> return nullPiece
        [(lo,hi)] | lo == hi -> return $ "\\mathtt{" <> latexCharPiece lo <> "}"
        xs -> return $ "\\{" <> mconcat (intersperse ", " $ map latexCharRange xs) <> "\\}"

    go d (App r s) = parens (d > AppPrec) $ do
        r'  <- go AppPrec r
        s'  <- go AppPrec s
        return (r' <> s')

    go d (Alt r s) = parens (d > AltPrec) $ do
        r'  <- go AltPrec r
        s'  <- go AltPrec s
        return $ r' <> "\\cup" <>  s'

#ifdef RERE_INTERSECTION
    go d (And r s) = parens (d > AndPrec) $ do
        r'  <- go AndPrec r
        s'  <- go AndPrec s
        return $ r' <> "\\cap" <>  s'
#endif

    go d (Star r) = parens (d > StarPrec) $ do
        r' <- go StarPrec r
        return (r' <> "^\\star")

    go _ (Var x) = return x

    go d (Let n (Fix _ r) s@Let {}) = parens (d > BotPrec) $ do
        i <- newUnique n
        let v  = showVar n i
        let r' = fmap (unvar v id) r
        let s' = fmap (unvar v id) s

        r2 <- go BotPrec r'

        let acc = "\\begin{aligned}[t] \\mathbf{let}& \\,"
                <> v <> "=_R" <> r2

        goLet acc s'

    go d (Let n r s@Let {}) = parens (d > BotPrec) $ do
        i <- newUnique n
        let v  = showVar n i
        let s' = fmap (unvar v id) s

        r2 <- go BotPrec r

        let acc = "\\begin{aligned}[t] \\mathbf{let}& \\,"
                <> v <> "=" <> r2

        goLet acc s'

    go d (Let n (Fix _ r) s) = parens (d > BotPrec) $ do
        i <- newUnique n
        let v  = showVar n i
        let r' = fmap (unvar v id) r
        let s' = fmap (unvar v id) s

        r2 <- go BotPrec r'
        s2 <- go BotPrec s'

        return $ "\\mathbf{let}\\,"
               <> v <> "=_R" <> r2
               <> "\\,\\mathbf{in}\\,"
               <> s2

    go d (Let n r s) = parens (d > BotPrec) $ do
        i <- newUnique n
        let v  = showVar n i
        let s' = fmap (unvar v id) s

        r2 <- go BotPrec r
        s2 <- go BotPrec s'

        return $ "\\mathbf{let}\\,"
               <> v <> "=" <> r2
               <> "\\,\\mathbf{in}\\,"
               <> s2

    go d (Fix n r) = parens (d > BotPrec) $ do
        i <- newUnique n
        let v  = showVar n i
        let r' = fmap (unvar v id) r

        r'' <- go BotPrec r'
        return $ piece $ showString "\\mathbf{fix}\\," . unPiece v . showChar '=' . unPiece r''


    goLet :: Piece -> RE Piece -> State (Set NI) Piece
    goLet acc0 (Let n (Fix _ r) s) = do
        i <- newUnique n
        let v  = showVar n i
        let r' = fmap (unvar v id) r
        let s' = fmap (unvar v id) s

        r2 <- go BotPrec r'

        let acc = acc0 <> "; \\\\ &"
                <> v <> "=" <> r2

        goLet acc s'

    goLet acc0 (Let n r s) = do
        i <- newUnique n
        let v  = showVar n i
        let s' = fmap (unvar v id) s

        r2 <- go BotPrec r

        let acc = acc0 <> "; \\\\ &"
                <> v <> "=" <> r2

        goLet acc s'

    goLet acc s = do
        s' <- go BotPrec s
        return $ acc <> "\\\\ \\mathbf{in} & \\," <> s' <> "\\end{aligned}"

    parens :: Bool -> State (Set NI) Piece -> State (Set NI) Piece
    parens True  = fmap $ \(Piece _ _ x) -> piece $ showChar '(' . x . showChar ')'
    parens False = id

latexChar :: Char -> String
latexChar '*' = "\\text{*}"
latexChar '+' = "\\text{+}"
latexChar '(' = "\\text{(}"
latexChar ')' = "\\text{)}"
latexChar '[' = "\\text{[}"
latexChar ']' = "\\text{]}"
latexChar c   = [c]

latexCharPiece :: Char -> Piece
latexCharPiece c = "{" <> literalColor (fromString (latexChar c)) <> "}"

latexCharRange :: (Char, Char) -> Piece
latexCharRange (lo, hi)
    | lo == hi  = latexCharPiece lo
    | otherwise = latexCharPiece lo <> " \\cdots " <> latexCharPiece hi

data NI = NI String [Char] Int deriving (Eq, Ord)

newUnique :: Name -> State (Set NI) Int
newUnique (N n cs) = get >>= go 0 where
    go i s | Set.member (NI n cs i) s = go (i + 1) s
           | otherwise = do
        put (Set.insert (NI n cs i) s)
        return i

showVar :: Name -> Int -> Piece
showVar (N n cs) i
    = Piece True True
    $ showString $ "{\\color{blue}\\mathit{" ++ n ++ "}" ++ sub ++ "}"
  where
    cs' = showCS cs
    i'  = showI i

    sub | null cs && null i'             = ""
        | not (null cs) && not (null i') = "_{" ++ cs' ++ ";" ++ i' ++ "}"
        | otherwise                      = "_{" ++ cs' ++ i' ++ "}"

    showCS :: [Char] -> String
    showCS ds = "\\mathtt{\\color{red!50!blue}" ++ concatMap latexChar ds ++ "}"

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

displayTrace :: (Bool, RE Void, [(String, RE Void)]) -> IO ()
displayTrace (matched, final, steps) = do
    putStrLn "\\begin{aligned}"
    for_ steps $ \(str, re) ->
        putStrLn $ "& \\mathtt{\\color{red!50!blue}" ++ concatMap latexChar str ++ "} &&\\vdash" ++ sub (nullable re) ++ " " ++ latexify re ++ " \\\\"
    putStrLn $ "&{\\color{red!80!black}\\varepsilon} &&\\vdash" ++ sub matched ++ " " ++ latexify final ++ " \\\\"
    putStrLn "\\end{aligned}"

    print matched
    print final

  where
    -- sub True  = "_\\varepsilon"
    -- sub False = "_\\kappa"
    sub _ = ""

-------------------------------------------------------------------------------
-- CFG
-------------------------------------------------------------------------------

#ifdef RERE_CFG
-- | Pretty-print 'CFG' given the names.
putLatexCFG :: Vec n Name -> CFG n Void -> IO ()
putLatexCFG names cfg = putStrLn (latexifyCfg names cfg)

latexifyCfg :: forall n. Vec n Name -> CFG n Void -> String
latexifyCfg names cfg =
    unlines $  ["\\begin{aligned}"] ++ go names cfg ++ ["\\end{aligned}"]
  where
    initS :: State (Set NI) ()
    initS = for_ names newUnique

    go :: Vec m Name -> Vec m (CFGBase n Void) -> [String]
    go VNil       VNil       = []
    go (n ::: ns) (e ::: es) = eq' : go ns es where
        e' = fmap (either (\i -> showVar (names V.! i) 0) absurd) e
        n' = showVar n 0

        eq = do
            initS
            e'' <- latexify' e'
            return $ n' <> " &= " <> e'' <> " \\\\"

        eq' :: String
        eq' = unPiece (evalState eq Set.empty) ""
#endif
