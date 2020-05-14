module Polaris.Codegen.PrinterUtils
       ( printRefName
       , printRefNameConstructor
       , lines
       ) where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.Extra (camelCase, pascalCase)
import Foreign.Object (Object)
import Foreign.Object as Object

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
