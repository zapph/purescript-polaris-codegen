module Polaris.Codegen.LocalesModulePrinter
       ( localesModuleBuilder
       ) where

import Prelude

import CST.Simple (ModuleBuilder)
import CST.Simple.ModuleBuilder (addForeignJsValue)
import Data.Foldable (for_)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Polaris.Codegen.Defs (typ_TranslationDictionary)

localesModuleBuilder :: Array String -> ModuleBuilder Unit
localesModuleBuilder localeNames = for_ localeNames \localeName ->
  addForeignJsValue
  { export: true
  , name: psLocaleName localeName
  , type_: typ_TranslationDictionary
  , jsExpr: "require(\"@shopify/polaris/locales/" <> localeName <> ".json\")"
  }

psLocaleName :: String -> String
psLocaleName = String.replace (Pattern "-") (Replacement "_")
