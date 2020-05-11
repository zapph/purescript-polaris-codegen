module Polaris.Codegen.PrinterUtils
       ( printRefName
       , lines
       ) where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.Extra (pascalCase)
import Foreign.Object (Object)
import Foreign.Object as Object

printRefName :: Array String -> String
printRefName ns = fromMaybe name (Object.lookup name replacements)
  where
    name =
      Array.intercalate "__"
      $ String.replaceAll (Pattern ".") (Replacement "_") <<< pascalCase <$> ns

lines :: Array String -> String
lines = Array.intercalate "\n"

replacements :: Object String
replacements = Object.fromHomogeneous
  { "Array": "PArray"
  }
