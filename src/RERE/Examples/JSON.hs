{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
#if __GLASGOW_HASKELL__ < 709
{-# OPTIONS_GHC -fcontext-stack=50 #-}
#endif
-- | JSON grammar example.
module RERE.Examples.JSON where

import Prelude hiding (exponent)

import Data.Vec.Lazy (Vec (..))
import Data.Void     (Void)

import qualified Data.Type.Nat as N
import qualified Data.Vec.Lazy as V

import           RERE
import qualified RERE.CharSet as CS

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif

-- | Size of JSON grammar, 22.
type Size = N.Mult2 (N.Plus N.Nat5 N.Nat6)

-- | JSON recursive regular expression constructor from 'jsonCFG'.
--
-- @
-- 'jsonRE'' = 'compact' ('cfgToRE' 'jsonNames' 'jsonCFG')
-- @
--
-- The conversion doesn't optimize the resulting regular expression,
-- but is relatively fast.
--
-- >>> size (cfgToRE jsonNames jsonCFG)
-- 232
--
-- 'jsonRE' is pre-calculated variant.
--
-- >>> size jsonRE
-- 205
--
jsonRE' :: RE Void
jsonRE' = compact (cfgToRE jsonNames jsonCFG)

-- | Names of 'jsonCFG' productions.
jsonNames :: Vec Size Name
jsonNames = V.reverse $
    "json" ::: "value" ::: "object" ::: "members" ::: "member" ::: "array" ::: "elements" ::: "element" ::: "string" ::: "characters" ::: "character" ::: "escape" ::: "hex" ::: "number" ::: "integer" ::: "digits" ::: "digit" ::: "onenine" ::: "fraction" ::: "exponent" ::: "sign" ::: "ws" ::: VNil

-- | JSON grammar
jsonCFG :: forall a. Ord a => CFG Size a
jsonCFG = V.reverse $
    json ::: value ::: object ::: members ::: member ::: array ::: elements ::: element ::: string ::: characters ::: character ::: escape ::: hex ::: number ::: integer ::: digits ::: digit ::: onenine ::: fraction ::: exponent ::: sign ::: ws ::: VNil
  where

    _jsonV, valueV, objectV, membersV, memberV, arrayV, elementsV, elementV, stringV, charactersV, characterV, escapeV, hexV, numberV, integerV, digitsV, digitV, onenineV, fractionV, exponentV, signV, wsV :: CFGBase Size a
    _jsonV      = Var $ Left 21
    valueV      = Var $ Left 20
    objectV     = Var $ Left 19
    membersV    = Var $ Left 18
    memberV     = Var $ Left 17
    arrayV      = Var $ Left 16
    elementsV   = Var $ Left 15
    elementV    = Var $ Left 14
    stringV     = Var $ Left 13
    charactersV = Var $ Left 12
    characterV  = Var $ Left 11
    escapeV     = Var $ Left 10
    hexV        = Var $ Left 9
    numberV     = Var $ Left 8
    integerV    = Var $ Left 7
    digitsV     = Var $ Left 6
    digitV      = Var $ Left 5
    onenineV    = Var $ Left 4
    fractionV   = Var $ Left 3
    exponentV   = Var $ Left 2
    signV       = Var $ Left 1
    wsV         = Var $ Left 0

    -- json
    --     element
    --
    json = elementV

    -- value
    --     object
    --     array
    --     string
    --     number
    --     "true"
    --     "false"
    --     "null"
    --
    value = unions [ objectV, arrayV, stringV, numberV, "true", "false", "null" ]

    -- object
    --     '{' ws '}'
    --     '{' members '}'
    --
    object = "{" <> wsV <> "}" \/ "{" <> membersV <> "}"

    -- members
    --     member
    --     member ',' members
    --
    members = memberV \/ memberV <> "," <> membersV

    -- member
    --     ws string ws ':' element
    --
    member = wsV <> stringV <> wsV <> ":" <> elementV

    -- array
    --     '[' ws ']'
    --     '[' elements ']'
    --
    array = "[" <> wsV <> "]" \/ "[" <> elementsV <> "]"

    -- elements
    --     element
    --     element ',' elements
    --
    elements = elementV \/ elementV <> "," <> elementsV

    -- element
    --     ws value ws
    --
    element = wsV <> valueV <> wsV

    -- string
    --     '"' characters '"'
    --
    string = "\"" <> charactersV <> "\""

    -- characters
    --     ""
    --     character characters
    --
    characters = "" \/ characterV <> charactersV

    -- character
    --     '0020' . '10FFFF' - '"' - '\'
    --     '\' escape
    --
    character = Ch chars \/ "\\" <> escapeV

    chars = CS.fromList [ c | c <- ['\x20' .. maxBound ], c /= '"', c /= '\\' ]

    -- escape
    --     '"'
    --     '\'
    --     '/'
    --     'b'
    --     'f'
    --     'n'
    --     'r'
    --     't'
    --     'u' hex hex hex hex
    --
    escape =
        Ch (CS.fromList "\"\\/bfnrt")
        \/ "u" <> hexV <> hexV <> hexV <> hexV

    -- hex
    --     digit
    --     'A' . 'F'
    --     'a' . 'f'
    hex = digitV <> Ch (CS.fromList "ABCDEFabcdef")

    -- number
    --     integer fraction exponent
    --
    number = integerV <> fractionV <> exponentV

    -- integer
    --     digit
    --     onenine digits
    --     '-' digit
    --     '-' onenine digits
    --
    integer = digitV \/ onenineV <> digitsV \/ "-" <> digitV \/ "-" <> onenineV <> digitsV

    -- digits
    --     digit
    --     digit digits
    --
    digits = digitV \/ digitV <> digitsV

    -- digit
    --     '0'
    --     onenine
    --
    digit = "0" \/ onenineV

    -- onenine
    --     '1' . '9'
    --
    onenine = Ch (CS.fromList "123456789")

    -- fraction
    --     ""
    -- '.' digits
    --
    fraction = "" \/ "." <> digitsV

    -- exponent
    --     ""
    --     'E' sign digits
    --     'e' sign digits
    --
    exponent = "" \/ "E" <> signV <> digitsV \/ "e" <> signV <> digitsV

    -- sign
    --     ""
    --     '+'
    --     '-'
    --
    sign = "" \/ "+" \/ "-"

    -- ws
    --     ""
    --     '0020' ws
    --     '000A' ws
    --     '000D' ws
    --     '0009' ws
    --
    ws = unions ["", "\x20" <> wsV, "\x0A" <> wsV, "\x0D" <> wsV, "\x09" <> wsV ]

    -- unions
    unions = foldr (\/) Null

-------------------------------------------------------------------------------
-- Cheat
-------------------------------------------------------------------------------

-- | Pre-calculated JSON grammar as regular expression.
--
-- >>> size jsonRE
-- 205
--
-- See 'jsonRE'' for one constructed from 'jsonCFG'.
--
jsonRE :: RE Void
jsonRE =
    Let "ws" (Fix "ws" (Alt Eps (Alt (App (Ch " ") (Var B)) (Alt (App (Ch "\n") (Var B)) (Alt (App (Ch "\r") (Var B)) (App (Ch "\t") (Var B))))))) (Let "hex" (App (Ch "0123456789") (Ch "ABCDEFabcdef")) (Let "escape" (Alt (Ch "\"/\\bfnrt") (App (Ch "u") (App (Var B) (App (Var B) (App (Var B) (Var B)))))) (Let "character" (Alt (Ch (CS.fromIntervalList [('\32','\33'),('\35','\91'),('\93','\1114111')])) (App (Ch "\\") (Var B))) (Let "characters" (Fix "characters" (Alt Eps (App (Var (F B)) (Var B)))) (Let "string" (App (Ch "\"") (App (Var B) (Ch "\""))) (Let "digits" (Fix "digits" (Alt (Ch "0123456789") (App (Ch "0123456789") (Var B)))) (Let "integer" (Alt (Ch "0123456789") (Alt (App (Ch "123456789") (Var B)) (Alt (App (Ch "-") (Ch "0123456789")) (App (Ch "-") (App (Ch "123456789") (Var B)))))) (Let "fraction" (Alt Eps (App (Ch ".") (Var (F B)))) (Let "sign" (Alt Eps (Ch "+-")) (Let "exponent" (Alt Eps (Alt (App (Ch "E") (App (Var B) (Var (F (F (F B)))))) (App (Ch "e") (App (Var B) (Var (F (F (F B)))))))) (Let "number" (App (Var (F (F (F B)))) (App (Var (F (F B))) (Var B))) (Let "value" (Fix "value" (Let "element" (App (Var (F (F (F (F (F (F (F (F (F (F (F (F B))))))))))))) (App (Var B) (Var (F (F (F (F (F (F (F (F (F (F (F (F B))))))))))))))) (Let "member" (App (Var (F (F (F (F (F (F (F (F (F (F (F (F (F B)))))))))))))) (App (Var (F (F (F (F (F (F (F (F B))))))))) (App (Var (F (F (F (F (F (F (F (F (F (F (F (F (F B)))))))))))))) (App (Ch ":") (Var B))))) (Let "members" (Fix "members" (Alt (Var (F B)) (App (Var (F B)) (App (Ch ",") (Var B))))) (Let "object" (Alt (App (Ch "{") (App (Var (F (F (F (F (F (F (F (F (F (F (F (F (F (F (F B)))))))))))))))) (Ch "}"))) (App (Ch "{") (App (Var B) (Ch "}")))) (Let "elements" (Fix "elements" (Alt (Var (F (F (F (F B))))) (App (Var (F (F (F (F B))))) (App (Ch ",") (Var B))))) (Let "array" (Alt (App (Ch "[") (App (Var (F (F (F (F (F (F (F (F (F (F (F (F (F (F (F (F (F B)))))))))))))))))) (Ch "]"))) (App (Ch "[") (App (Var B) (Ch "]")))) (Alt (Var (F (F B))) (Alt (Var B) (Alt (Var (F (F (F (F (F (F (F (F (F (F (F (F (F B)))))))))))))) (Alt (Var (F (F (F (F (F (F (F B)))))))) (Alt (App (Ch "t") (App (Ch "r") (App (Ch "u") (Ch "e")))) (Alt (App (Ch "f") (App (Ch "a") (App (Ch "l") (App (Ch "s") (Ch "e"))))) (App (Ch "n") (App (Ch "u") (App (Ch "l") (Ch "l"))))))))))))))))) (App (Var (F (F (F (F (F (F (F (F (F (F (F (F B))))))))))))) (App (Var B) (Var (F (F (F (F (F (F (F (F (F (F (F (F B)))))))))))))))))))))))))))

#ifdef RERE_SLOW_DOCTEST
-- | This are slow tests, take around 90 seconds on my machine
--
-- >>> size jsonRE'
-- 205
--
-- >>> jsonRE == jsonRE'
-- True
--
_doctest1 :: ()
_doctest1 =  ()
#endif
