module Polaris.Codegen.LocalesModulePrinter
       ( localesModuleBuilder
       ) where

import Prelude

import CST.Simple (ModuleBuilder, typCons)
import CST.Simple.ModuleBuilder (addForeignJsValue)
import Data.Foldable (for_)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String

localesModuleBuilder :: Array String -> ModuleBuilder Unit
localesModuleBuilder localeNames = for_ localeNames \localeName ->
  addForeignJsValue
  { export: true
  , name: psLocaleName localeName
  , type_: typCons "Polaris.Components.AppProvider.TranslationDictionary"
  , jsExpr: "require(\"@shopify/polaris/locales/" <> localeName <> ".json\")"
  }

psLocaleName :: String -> String
psLocaleName = String.replace (Pattern "-") (Replacement "_")
