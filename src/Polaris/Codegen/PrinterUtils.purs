module Polaris.Codegen.PrinterUtils
       ( printRefName
       , lines
       ) where

import Prelude

import Data.Array as Array
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.Extra (pascalCase)

printRefName :: Array String -> String
printRefName ns =
  Array.intercalate "__"
  $ String.replaceAll (Pattern ".") (Replacement "_") <<< pascalCase <$> ns

lines :: Array String -> String
lines = Array.intercalate "\n"
