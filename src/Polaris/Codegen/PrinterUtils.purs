module Polaris.Codegen.PrinterUtils
       ( printRefName
       , printRefNameConstructor
       , lines
       , isCommonType
       , printCST
       ) where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.Extra (camelCase, pascalCase)
import Dodo as Dodo
import Foreign.Object (Object)
import Foreign.Object as Object
import Language.PS.SmartCST (Module)
import Language.PS.SmartCST as SmartCST

printRefName :: String -> String
printRefName rawName = fromMaybe name (Object.lookup rawName replacements)
  where
    name = String.replaceAll (Pattern ".") (Replacement "_")
           <<< pascalCase
           <<< String.replaceAll (Pattern " & ") (Replacement "__")
           $ rawName

printRefNameConstructor :: String -> String
printRefNameConstructor = camelCase <<< printRefName

lines :: Array String -> String
lines = Array.intercalate "\n"

replacements :: Object String
replacements = Object.fromHomogeneous
  { "Array": "PArray"
  }

isCommonType :: String -> Boolean
isCommonType "Action" = true
isCommonType _ = false

printCST :: Module -> String
printCST =
  Dodo.print Dodo.plainText Dodo.twoSpaces
  <<< SmartCST.printModule
