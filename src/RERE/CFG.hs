{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
-- | Context free grammars, where
-- each production is a regular-expression.
module RERE.CFG (
    -- * Context-free grammars
    CFG,
    CFGBase,
    -- * Conversion to recursive regular expressions
    cfgToRE,
    ) where

import Data.Fin      (Fin (..))
import Data.Nat      (Nat (..))
import Data.Vec.Lazy (Vec (..))

import qualified Data.Type.Nat as N
import qualified Data.Vec.Lazy as V

import RERE.Type
import RERE.Var

#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (Traversable (..))
#endif

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Fin      (Fin (..))
-- >>> import Data.Vec.Lazy (Vec (..))
-- >>> import RERE

-- | Context-free grammar represented as @n@ equations
-- of 'RE' ('CFGBase') with @n@ variables.
--
type CFG n a = Vec n (CFGBase n a)

-- | Single equation in context-free-grammar equation.
type CFGBase n a = RE (Either (Fin n) a)

-- | Convert 'CFG' (with names for productions) into 'RE'.
-- Note: the start symbol have to be last equation.
--
-- >>> let a = Eps \/ ch_ 'a' <> Var (Left FZ)
-- >>> let b = Eps \/ ch_ 'b' <> Var (Left (FS FZ))
-- >>> let cfg = b ::: a ::: VNil
--
-- \[
-- \begin{aligned}
-- {\mathit{b}} &= {\varepsilon}\cup\mathtt{b}{\mathit{a}} \\
-- {\mathit{a}} &= {\varepsilon}\cup\mathtt{a}{\mathit{b}} \\
-- \end{aligned}
-- \]
--
-- >>> cfgToRE ("b" ::: "a" ::: VNil) cfg
-- Fix "a" (Let "b" (Alt Eps (App (Ch "b") (Var B))) (Alt Eps (App (Ch "a") (Var B))))
--
-- which represents \(\mathbf{fix}\,{\mathit{a}}=\mathbf{let}\,{\mathit{b}}={\varepsilon}\cup\mathtt{b}{\mathit{a}}\,\mathbf{in}\,{\varepsilon}\cup\mathtt{a}{\mathit{b}}\)
-- recursive regular expression.
--
cfgToRE :: (N.SNatI n, Ord a) => Vec ('S n) Name -> CFG ('S n) a -> RE a
cfgToRE = getCfgToRE (N.induction1 start step) where
    start = CfgToRE baseCase

    step :: Ord a => CfgToRE m a -> CfgToRE ('S m) a
    step (CfgToRE rec) = CfgToRE $ \names cfg ->
        rec (V.tail names) (consCase names cfg)

newtype CfgToRE n a = CfgToRE { getCfgToRE :: Vec ('S n) Name -> CFG ('S n) a -> RE a }

baseCase :: Ord a => Vec N.Nat1 Name -> CFG N.Nat1 a -> RE a
baseCase (name ::: VNil) (cfg ::: VNil) =
    fix' name (fmap (either (\FZ -> B) F) cfg)
#if __GLASGOW_HASKELL__  <711
baseCase _ _ = error "silly GHC"
#endif

consCase
    :: forall a n. Ord a
    => Vec ('S n) Name
    -> CFG ('S n) a
    -> CFG n a
consCase (name ::: _names) (f ::: gs) =
    V.map (\g -> let' name f' (fmap sub g)) gs
  where
    f' = fix' name (fmap sub' f)

    sub :: Either (Fin ('S n)) a -> Var (Either (Fin n) a)
    sub (Right a)     = F (Right a)
    sub (Left (FS n)) = F (Left n)
    sub (Left FZ)     = B

    sub' :: Either (Fin ('S n)) a -> Var (Either (Fin n) a)
    sub' (Right a)     = F (Right a)
    sub' (Left (FS n)) = F (Left n)
    sub' (Left FZ)     = B

-------------------------------------------------------------------------------
-- Dummier fix and let
-------------------------------------------------------------------------------

-- This functions only rearrange fix and let,
-- and don't perform other simplifications.

fix' :: Eq a => Name -> RE (Var a) -> RE a
-- fix' n (Let m r s)
--     | Just r' <- traverse (unvar Nothing Just) r
--     = Let m r' (fix' n (fmap swapVar s))
fix' n r
    | Just r' <- floatOut r (unvar Nothing Just) (fix' n)
    = r'
    | Just r' <- traverse (unvar Nothing Just) r
    = r'
fix' n r = Fix n r

floatOut
    :: (Eq a, Eq b)
    => RE (Var a)                        -- ^ expression
    -> (Var a -> Maybe b)                -- ^ float out var
    -> (RE (Var (Var a)) -> RE (Var b))  -- ^ binder
    -> Maybe (RE b)                      -- ^ maybe an expression with let floaten out
floatOut (Let m r s) un mk
    | Just r' <- traverse un r
    = Just
    $ let' m r' $ mk $ fmap swapVar s
    | otherwise
    = floatOut
        s
        (unvar Nothing un)
        (mk . let' m (fmap (fmap F) r) . fmap (fmap swapVar))
floatOut _ _ _ = Nothing

let' :: Eq a => Name -> RE a -> RE (Var a) -> RE a
let' n (Let m x r) s
    = let' m x
    $ let' n r (fmap (unvar B (F . F)) s)
let' n r s = postlet' n r (go B (fmap F r) s) where
    -- This simple CSE only looks for lets. i.e
    --
    --     let x = a; y = a in ...body x y...
    --     -- >
    --     let x = a in ...body x x...
    --
    -- 'consCase' introduces same lets, so this fires a lot.
    --
    -- Note: not using let' or fix' in the bodies
    -- makes this faster.
    go :: Eq b => b -> RE b -> RE b -> RE b
    go v x (Let m a b)
        | x == a    = go v x (fmap (unvar v id) b)
        | otherwise = Let m (go v x a) (go (F v) (fmap F x) b)
    go v x (Fix m a) = Fix m (go (F v) (fmap F x) a)

    go _ _ r' = r'

postlet' :: Name -> RE a -> RE (Var a) -> RE a
postlet' _ r (Var B)                       = r
postlet' _ _ s       | Just s' <- unused s = s'
postlet' n r s                             = Let n r s

unused :: RE (Var a) -> Maybe (RE a)
unused = traverse (unvar Nothing Just)
