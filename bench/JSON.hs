{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main, jsonRE, jsonRE', jsonNames, jsonCFG, jsonDerp, derp) where

import Prelude hiding (exponent)

import Criterion.Main (bench, defaultMain, whnf)
import Data.Either    (isRight)
import Data.Fin       (Fin (..))
import Data.Vec.Lazy  (Vec (..))
import Data.Void      (Void)
import System.Clock   (Clock (Monotonic), diffTimeSpec, getTime)

import qualified Data.Aeson.Parser          as A
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString            as BS
import qualified Data.Set                   as Set
import qualified Data.Type.Nat              as N
import qualified Data.Vec.Lazy              as V
import qualified Text.Derp                  as D

#ifdef MIN_VERSION_saison
import qualified Saison.Decoding.Parser as S
import qualified Saison.Decoding.Tokens as S
#endif

import           RERE
import qualified RERE.CharSet as CS

import DerpConv

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif

derp :: D.Parser Char () -> String -> Bool
derp p s = not $ Set.null $ D.runParse p [ D.Token c [c] | c <- s]

aeson :: BS.ByteString -> Bool
aeson bs = isRight (Atto.parseOnly (A.json <* Atto.endOfInput) bs)

#ifdef MIN_VERSION_saison
saison :: BS.ByteString -> Bool
saison = go end . S.tokens where
    end :: BS.ByteString -> Bool
    end = BS.null . S.skipSpace

    go :: (k -> Bool) -> S.Tokens k e -> Bool
    go kont (S.TkLit _ k)        = kont k
    go kont (S.TkText _ k)       = kont k
    go kont (S.TkNumber _ k)     = kont k
    go kont (S.TkArrayOpen arr)  = goArr kont arr
    go kont (S.TkRecordOpen obj) = goObj kont obj
    go _    (S.TkErr _)          = False

    goArr :: (k -> Bool) -> S.TkArray k e -> Bool
    goArr kont (S.TkItem k)     = go (goArr kont) k
    goArr kont (S.TkArrayEnd k) = kont k
    goArr _    (S.TkArrayErr _) = False

    goObj :: (k -> Bool) -> S.TkRecord k e -> Bool
    goObj kont (S.TkPair _ k)    = go (goObj kont) k
    goObj kont (S.TkRecordEnd k) = kont k
    goObj _    (S.TkRecordErr _) = False
#endif

main :: IO ()
main = do
    contents <- readFile    "jsverify.json"
    bs       <- BS.readFile "jsverify.json"

    id $ do
        putStrLn "Ref run"
        start <- getTime Monotonic
        print $ matchR jsonRE' contents
        end <- getTime Monotonic
        print $ diffTimeSpec end start

    id $ do
        putStrLn "Ref2 run"
        start <- getTime Monotonic
        print $ matchR jsonRE'' contents
        end <- getTime Monotonic
        print $ diffTimeSpec end start

    id $ do
        putStrLn "ST run"
        start <- getTime Monotonic
        print $ matchST jsonRE' contents
        end <- getTime Monotonic
        print $ diffTimeSpec end start

    id $ do
        putStrLn "ST run"
        start <- getTime Monotonic
        print $ matchST jsonRE'' contents
        end <- getTime Monotonic
        print $ diffTimeSpec end start

--    id $ do
--        putStrLn "derp run"
--        start <- getTime Monotonic
--        print $ derp jsonDerp $ take 0 contents
--        end <- getTime Monotonic
--        print $ diffTimeSpec end start

    putStrLn "Criterion"
    defaultMain
        [ bench "aeson"  $ whnf aeson bs
#ifdef MIN_VERSION_saison
        , bench "saison" $ whnf saison bs
#endif
        -- , bench "rere"  $ whnf (matchR jsonRE') contents
        ]

-------------------------------------------------------------------------------
-- JSON definition
-------------------------------------------------------------------------------

-- | 22
type Size = N.Mult2 (N.Plus N.Nat5 N.Nat6)

jsonRE :: RE Void
jsonRE = cfgToRE jsonNames jsonCFG

jsonDerp :: D.Parser Char ()
jsonDerp = rere2derp jsonRE

jsonNames :: Vec Size Name
jsonNames = V.reverse $
    "json" ::: "value" ::: "object" ::: "members" ::: "member" ::: "array" ::: "elements" ::: "element" ::: "string" ::: "characters" ::: "character" ::: "escape" ::: "hex" ::: "number" ::: "integer" ::: "digits" ::: "digit" ::: "onenine" ::: "fraction" ::: "exponent" ::: "sign" ::: "ws" ::: VNil

jsonCFG :: forall a. Ord a => CFG Size a
jsonCFG = V.reverse $
    json ::: value ::: object ::: members ::: member ::: array ::: elements ::: element ::: string ::: characters ::: character ::: escape ::: hex ::: number ::: integer ::: digits ::: digit ::: onenine ::: fraction ::: exponent ::: sign ::: ws ::: VNil
  where

    _jsonV, valueV, objectV, membersV, memberV, arrayV, elementsV, elementV, stringV, charactersV, characterV, escapeV, hexV, numberV, integerV, digitsV, digitV, onenineV, fractionV, exponentV, signV, wsV :: CFGBase Size a
    _jsonV ::: valueV ::: objectV ::: membersV ::: memberV ::: arrayV ::: elementsV ::: elementV ::: stringV ::: charactersV ::: characterV ::: escapeV ::: hexV ::: numberV ::: integerV ::: digitsV ::: digitV ::: onenineV ::: fractionV ::: exponentV ::: signV ::: wsV ::: VNil
        = V.reverse $ V.map (Var . Left) (V.universe :: V.Vec Size (Fin Size))

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

jsonRE' :: RE Void
jsonRE' =
    Let "ws" (Fix "ws" (Alt Eps (Alt (App (Ch " ") (Var B)) (Alt (App (Ch "\n") (Var B)) (Alt (App (Ch "\r") (Var B)) (App (Ch "\t") (Var B))))))) (Let "value" (Fix "value" (Let "element" (App (Var (F B)) (App (Var B) (Var (F B)))) (Let "hex" (App (Ch "0123456789") (Ch "ABCDEFabcdef")) (Let "escape" (Alt (Ch "\"/\\bfnrt") (App (Ch "u") (App (Var B) (App (Var B) (App (Var B) (Var B)))))) (Let "character" (Alt (Ch (CS.fromIntervalList [('\32','\33'),('\35','\91'),('\93','\1114111')])) (App (Ch "\\") (Var B))) (Let "characters" (Fix "characters" (Alt Eps (App (Var (F B)) (Var B)))) (Let "string" (App (Ch "\"") (App (Var B) (Ch "\""))) (Let "member" (App (Var (F (F (F (F (F (F (F B)))))))) (App (Var B) (App (Var (F (F (F (F (F (F (F B)))))))) (App (Ch ":") (Var (F (F (F (F (F B)))))))))) (Let "members" (Fix "members" (Alt (Var (F B)) (App (Var (F B)) (App (Ch ",") (Var B))))) (Let "object" (Alt (App (Ch "{") (App (Var (F (F (F (F (F (F (F (F (F B)))))))))) (Ch "}"))) (App (Ch "{") (App (Var B) (Ch "}")))) (Let "elements" (Fix "elements" (Alt (Var (F (F (F (F (F (F (F (F (F B)))))))))) (App (Var (F (F (F (F (F (F (F (F (F B)))))))))) (App (Ch ",") (Var B))))) (Let "array" (Alt (App (Ch "[") (App (Var (F (F (F (F (F (F (F (F (F (F (F B)))))))))))) (Ch "]"))) (App (Ch "[") (App (Var B) (Ch "]")))) (Let "digits" (Fix "digits" (Alt (Ch "0123456789") (App (Ch "0123456789") (Var B)))) (Let "integer" (Alt (Ch "0123456789") (Alt (App (Ch "123456789") (Var B)) (Alt (App (Ch "-") (Ch "0123456789")) (App (Ch "-") (App (Ch "123456789") (Var B)))))) (Let "fraction" (Alt Eps (App (Ch ".") (Var (F B)))) (Let "sign" (Alt Eps (Ch "+-")) (Let "exponent" (Alt Eps (Alt (App (Ch "E") (App (Var B) (Var (F (F (F B)))))) (App (Ch "e") (App (Var B) (Var (F (F (F B)))))))) (Let "number" (App (Var (F (F (F B)))) (App (Var (F (F B))) (Var B))) (Alt (Var (F (F (F (F (F (F (F (F B))))))))) (Alt (Var (F (F (F (F (F (F B))))))) (Alt (Var (F (F (F (F (F (F (F (F (F (F (F B)))))))))))) (Alt (Var B) (Alt (App (Ch "t") (App (Ch "r") (App (Ch "u") (Ch "e")))) (Alt (App (Ch "f") (App (Ch "a") (App (Ch "l") (App (Ch "s") (Ch "e"))))) (App (Ch "n") (App (Ch "u") (App (Ch "l") (Ch "l")))))))))))))))))))))))))))) (App (Var (F B)) (App (Var B) (Var (F B)))))

jsonRE'' :: RE Void
jsonRE'' =
    Let "ws" (Fix "ws" (Alt Eps (Alt (App (Ch " ") (Var B)) (Alt (App (Ch "\n") (Var B)) (Alt (App (Ch "\r") (Var B)) (App (Ch "\t") (Var B))))))) (Let "hex" (App (Ch "0123456789") (Ch "ABCDEFabcdef")) (Let "escape" (Alt (Ch "\"/\\bfnrt") (App (Ch "u") (App (Var B) (App (Var B) (App (Var B) (Var B)))))) (Let "character" (Alt (Ch (CS.fromIntervalList [('\32','\33'),('\35','\91'),('\93','\1114111')])) (App (Ch "\\") (Var B))) (Let "characters" (Fix "characters" (Alt Eps (App (Var (F B)) (Var B)))) (Let "string" (App (Ch "\"") (App (Var B) (Ch "\""))) (Let "digits" (Fix "digits" (Alt (Ch "0123456789") (App (Ch "0123456789") (Var B)))) (Let "integer" (Alt (Ch "0123456789") (Alt (App (Ch "123456789") (Var B)) (Alt (App (Ch "-") (Ch "0123456789")) (App (Ch "-") (App (Ch "123456789") (Var B)))))) (Let "fraction" (Alt Eps (App (Ch ".") (Var (F B)))) (Let "sign" (Alt Eps (Ch "+-")) (Let "exponent" (Alt Eps (Alt (App (Ch "E") (App (Var B) (Var (F (F (F B)))))) (App (Ch "e") (App (Var B) (Var (F (F (F B)))))))) (Let "number" (App (Var (F (F (F B)))) (App (Var (F (F B))) (Var B))) (Let "value" (Fix "value" (Let "element" (App (Var (F (F (F (F (F (F (F (F (F (F (F (F B))))))))))))) (App (Var B) (Var (F (F (F (F (F (F (F (F (F (F (F (F B))))))))))))))) (Let "member" (App (Var (F (F (F (F (F (F (F (F (F (F (F (F (F B)))))))))))))) (App (Var (F (F (F (F (F (F (F (F B))))))))) (App (Var (F (F (F (F (F (F (F (F (F (F (F (F (F B)))))))))))))) (App (Ch ":") (Var B))))) (Let "members" (Fix "members" (Alt (Var (F B)) (App (Var (F B)) (App (Ch ",") (Var B))))) (Let "object" (Alt (App (Ch "{") (App (Var (F (F (F (F (F (F (F (F (F (F (F (F (F (F (F B)))))))))))))))) (Ch "}"))) (App (Ch "{") (App (Var B) (Ch "}")))) (Let "elements" (Fix "elements" (Alt (Var (F (F (F (F B))))) (App (Var (F (F (F (F B))))) (App (Ch ",") (Var B))))) (Let "array" (Alt (App (Ch "[") (App (Var (F (F (F (F (F (F (F (F (F (F (F (F (F (F (F (F (F B)))))))))))))))))) (Ch "]"))) (App (Ch "[") (App (Var B) (Ch "]")))) (Alt (Var (F (F B))) (Alt (Var B) (Alt (Var (F (F (F (F (F (F (F (F (F (F (F (F (F B)))))))))))))) (Alt (Var (F (F (F (F (F (F (F B)))))))) (Alt (App (Ch "t") (App (Ch "r") (App (Ch "u") (Ch "e")))) (Alt (App (Ch "f") (App (Ch "a") (App (Ch "l") (App (Ch "s") (Ch "e"))))) (App (Ch "n") (App (Ch "u") (App (Ch "l") (Ch "l"))))))))))))))))) (App (Var (F (F (F (F (F (F (F (F (F (F (F (F B))))))))))))) (App (Var B) (Var (F (F (F (F (F (F (F (F (F (F (F (F B)))))))))))))))))))))))))))
