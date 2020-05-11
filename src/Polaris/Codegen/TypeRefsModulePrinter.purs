module Polaris.Codegen.TypeRefsModulePrinter
       ( printTypeRefsModule
       ) where

import Prelude

import Data.Array as Array
import Polaris.Codegen.PrinterUtils (lines, printRefName)

printTypeRefsModule :: Array (Array String) -> String
printTypeRefsModule refNames = lines
  [ "module Polaris.Types"
  , "  ( " <> exportNamesPart
  , "  ) where"
  , ""
  , defParts
  , ""
  ]

  where
    names = printRefName <$> refNames

    exportNamesPart = Array.intercalate "\n  , " names
    defParts = Array.intercalate "\n" $ printForeignDataDef <$> names

printForeignDataDef :: String -> String
printForeignDataDef name = lines
  [ "foreign import data " <> name <> " :: Type"
  ]
