{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}
module RERE.Examples where

import Control.Applicative (some, (<|>))
import Control.Monad       (void)
import Data.Vec.Lazy       (Vec (..))
import Data.Void           (Void)

import qualified Data.Fin      as F
import qualified Data.Type.Nat as N

import qualified Text.Parsec        as P
import qualified Text.Parsec.String as P

import RERE

-------------------------------------------------------------------------------
-- * Syntax
-------------------------------------------------------------------------------

-- | Demonstrates how various constructors are pretty printed.
syntaxExamples :: IO ()
syntaxExamples = do
    putLatex Null
    putLatex Eps

    putLatex $ Ch 'a'
    putLatex $ Ch 'b'

    putLatex $ Let "r" Eps $ Let "r" Eps $ App (Var B) (Var (F B))
    putLatex $ Let "r" Eps $ Let "r" Eps $ Alt (Var B) (Var (F B))
    putLatex $ Let "r" Eps $ Star (Var B)

-------------------------------------------------------------------------------
-- * Example 1
-------------------------------------------------------------------------------

-- |
--
-- >>> match ex1 "abab"
-- True
--
-- >>> match ex1 "ababa"
-- False
--
-- >>> ex1
-- Star (App (Ch 'a') (Ch 'b'))
--
-- >>> fromRE ex1
-- Star (App (Ch 'a') (Ch 'b'))
--
-- >>> matchR ex1 "abab"
-- True
--
-- >>> matchR ex1 "ababa"
-- False
--
ex1 :: RE Void
ex1 = star_ (Ch 'a' <> Ch 'b')

ex1run1 :: IO ()
ex1run1 = putLatexTrace ex1 "abab"

-------------------------------------------------------------------------------
-- * Example 2
-------------------------------------------------------------------------------

-- |
--
-- >>> match ex2 "aaa"
-- True
--
-- Note: how "sharing" is preserved.
--
-- >>> fromRE ex2
-- App (Ref 0 (Star (Ch 'a'))) (Ref 0 (Star (Ch 'a')))
--
-- >>> matchR ex2 "aaa"
-- True
--
ex2 :: RE Void
ex2 = Let "r" (star_ (Ch 'a')) (Var B <> Var B)

ex2run1 :: IO ()
ex2run1 = putLatexTrace ex2 "aaa"

-------------------------------------------------------------------------------
-- * Example 3
-------------------------------------------------------------------------------

-- |
--
-- >>> match ex3 "abab"
-- True
--
-- >>> match ex3 "ababa"
-- False
--
-- >>> fromRE ex3
-- Ref 0 (Alt Eps (App (Ch 'a') (App (Ch 'b') (Ref 0 ...))))
--
-- >>> matchR ex3 "abab"
-- True
--
-- >>> matchR ex3 "ababa"
-- False
--
ex3 :: RE Void
ex3 = Fix "x" (Eps \/ Ch 'a' <> Ch 'b' <> Var B)

ex3run1 :: IO ()
ex3run1 = putLatexTrace ex3 "abab"

-------------------------------------------------------------------------------
-- * Example 4
-------------------------------------------------------------------------------

-- |
--
-- >>> match ex4 "aaaabbbb"
-- True
--
-- >>> matchR ex4 "aaaabbbb"
-- True
--
ex4 :: RE Void
ex4 = Fix "x" (Eps \/ Ch 'a' <> Var B <> Ch 'b')

ex4run1 :: IO ()
ex4run1 = putLatexTrace ex4 "aaaabbbb"

-------------------------------------------------------------------------------
-- * Example 5
-------------------------------------------------------------------------------

-- |
--
-- >>> match ex5 "abab"
-- True
--
-- >>> match ex5 "ababa"
-- False
--
-- >>> matchR ex5 "abab"
-- True
--
-- >>> matchR ex5 "ababa"
-- False
--
ex5 :: RE Void
ex5 = Fix "x" (Eps \/ Var B <> Ch 'a' <> Ch 'b')

ex5run1 :: IO ()
ex5run1 = putLatexTrace ex5 "abab"

-------------------------------------------------------------------------------
-- * Example 6
-------------------------------------------------------------------------------

--
-- Using fix-point operator:
--
-- @
-- fix expr = "(" expr ")" | "1" | "2" | expr "+" expr | expr "*" expr
-- @
--
-- which in BNF is almost he same
--
-- @
-- expr ::= "(" expr ")" | "1" | "2" | expr "+" expr | expr "*" expr
-- @
--
ex6 :: RE Void
ex6 = Let "d" (Ch '0' \/ Ch '1' \/ Ch '2' \/ Ch '3')
    $ Let "n" (Var B <> star_ (Var B))
    $ Fix "e"
    $  Ch '(' <> Var B <> Ch ')'
    \/ Var (F B)
    \/ Var B <> Ch '+' <> Var B
    \/ Var B <> Ch '*' <> Var B

--
-- (displayTraced . traced) is "match" function, which
-- also prints the trace of execution.
--
-- True
ex6run1 :: IO ()
ex6run1 = putLatexTrace ex6 "1*(20+3)"

-------------------------------------------------------------------------------
-- * Example7
-------------------------------------------------------------------------------

exCfg :: Ord a => CFG N.Nat5 a
exCfg =
    digit ::: digits ::: term ::: mult ::: expr ::: VNil
  where
    expr = Alt (multV <> Ch '*' <> exprV) multV
    mult = Alt (termV <> Ch '+' <> multV) termV
    term = Alt digitsV (Ch '(' <> exprV <> Ch ')')

    digit = Ch '0' \/ Ch '1' \/ Ch '2' \/ Ch '3'
    digits = digitV <> star_ digitV

    digitV, digitsV, exprV, multV, termV :: CFGBase N.Nat5 a
    exprV   = Var $ Left F.fin4
    multV   = Var $ Left F.fin3
    termV   = Var $ Left F.fin2
    digitsV = Var $ Left F.fin1
    digitV  = Var $ Left F.fin0

exCfgN :: Vec N.Nat5 Name
exCfgN = "digit" ::: "digits" ::: "term" ::: "mult" ::: "expr" ::: VNil

ex7 :: Ord a => RE a
ex7 = cfgToRE exCfgN exCfg

-- ex7 :: RE Void
-- ex7 = Let "digit" (Ch '0' \/ Ch '1' \/ Ch '2' \/ Ch '3')
--     $ Let "digits" (Var B <> star (Var B))
--     $ Fix "expr"
--     $ Let "term" (Var (F B) \/ Ch '(' <> Var B <> Ch ')')
--     $ Let "mult" (Fix "mult" $ Var (F B) <> Ch '+' <> Var B \/ Var (F B))
--     $ Var B <> Ch '*' <> Var (F B) \/ Var B

ex7run1 :: IO ()
ex7run1 = ex7run "1*(20+3)"

ex7run :: String -> IO ()
ex7run str = putLatexTrace ex7 str

ex7parsec :: P.Parser ()
ex7parsec = expr where
    expr   = void $ P.try (mult >> P.char '*' >> expr) <|> mult
    mult   = void $ P.try (term >> P.char '+' >> mult) <|> term
    term   = P.try digits <|> void (P.char '(' *> expr *> P.char ')')
    digits = void $ some digit
    digit  = P.char '0'
         <|> P.char '1'
         <|> P.char '2'
         <|> P.char '3'

ex7parsecRun :: IO ()
ex7parsecRun = P.parseTest ex7parsec "1*(20+3)"
