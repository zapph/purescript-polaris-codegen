module Polaris.Codegen.LocalesModulePrinter
       ( printLocalesModule
       ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (singleton, (:))
import Data.Array.NonEmpty as NEArray
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Language.PS.CST (Export(..))
import Language.PS.SmartCST (Declaration(..), Foreign(..), Ident(..), Module(..), ModuleName(..), ProperName(..), SmartQualifiedName(..), Type(..), mkModuleName)
import Polaris.Codegen.PrinterUtils (printCST)
import Polaris.Codegen.Types (PSJSContent)

printLocalesModule :: Array String -> PSJSContent
printLocalesModule localeNames =
  { psContent: printLocalesPSModule localeNames
  , jsContent: printLocalesJSModule localeNames
  }

printLocalesPSModule :: Array String -> String
printLocalesPSModule localeNames = printCST $ Module
  { moduleName: mkModuleName $ NEArray.cons' "Polaris" [ "Locales" ]
  , exports: ExportValue <<< Ident <$> psLocaleNames
  , declarations: foreignImportDict <$> psLocaleNames
  }
  where
    psLocaleNames = psLocaleName <$> localeNames

    foreignImportDict name = DeclForeign
      { comments: Nothing
      , foreign_: ForeignValue
        { ident: Ident name
        , type_: (TypeConstructor $ SmartQualifiedName__Simple
                  (ModuleName $
                   (ProperName "Polaris")
                   : (ProperName "Components")
                   : singleton (ProperName "AppProvider"))
                  (ProperName "TranslationDictionary")
                 )
        }
      }

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
