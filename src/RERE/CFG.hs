{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
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

-- $setup
-- >>> :set -XOverloadedStrings

-- | Context-free grammar represented as @n@ equations 
-- of 'RE' ('CFGBase') with @n@ variables.
--
type CFG n a = Vec n (CFGBase n a)

-- | Single equation in context-free-grammar equation.
type CFGBase n a = RE (Either (Fin n) a)

-- | Convert 'CFG' (with names for productions) into 'RE'.
-- Note: the start symbol have to be last equation.
--
-- >>> let a = Eps \/ Ch 'a' <> Var (Left FZ)
-- >>> let b = Eps \/ Ch 'b' <> Var (Left (FS FZ))
-- >>> let cfg = b ::: a ::: VNil
--
-- >>> cfgToRE ("b" ::: "a" ::: VNil) cfg
-- Fix "a" (Let "b" (Alt Eps (App (Ch 'b') (Var B))) (Alt Eps (App (Ch 'a') (Var B))))
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
    fix_ name (fmap (either (\FZ -> B) F) cfg)

consCase
    :: forall a n. Ord a
    => Vec ('S n) Name
    -> CFG ('S n) a
    -> CFG n a
consCase (name ::: _names) (f ::: gs) =
    V.map (\g -> let_ name f' (fmap sub g)) gs
  where
    f' = fix_ name (fmap sub' f)

    sub :: Either (Fin ('S n)) a -> Var (Either (Fin n) a)
    sub (Right a)     = F (Right a)
    sub (Left (FS n)) = F (Left n)
    sub (Left FZ)     = B

    sub' :: Either (Fin ('S n)) a -> Var (Either (Fin n) a)
    sub' (Right a)     = F (Right a)
    sub' (Left (FS n)) = F (Left n)
    sub' (Left FZ)     = B
