module Polaris.Codegen.LocalesModulePrinter
       ( printLocalesPSModule
       , printLocalesJSModule
       ) where

import Prelude

import Data.Array as Array
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Polaris.Codegen.PrinterUtils (lines)

printLocalesPSModule :: Array String -> String
printLocalesPSModule localeNames = lines
  [ "module Polaris.Locales"
  , "  ( " <> exportNamesPart
  , "  ) where"
  , ""
  , "import Polaris.Types (TranslationDictionary)"
  , ""
  , defParts
  , ""
  ]

  where
    names = psLocaleName <$> localeNames

    exportNamesPart = Array.intercalate "\n  , " names
    defParts = Array.intercalate "\n" $ printForeignDef <$> names

    printForeignDef :: String -> String
    printForeignDef psLocaleName' = lines
      [ "foreign import  " <> psLocaleName' <> " :: TranslationDictionary"
      ]

printLocalesJSModule :: Array String -> String
printLocalesJSModule localeNames =
  (Array.intercalate "\n" jsDefs)
  <> "\n"
  where
    jsDefs = printJSDef <$> localeNames

    printJSDef n =
      "exports." <> psLocaleName n <> " = require(\"@shopify/polaris/locales/" <> n <> ".json\");"

psLocaleName :: String -> String
psLocaleName = String.replace (Pattern "-") (Replacement "_")
