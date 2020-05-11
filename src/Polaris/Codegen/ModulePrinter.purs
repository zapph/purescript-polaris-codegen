module Polaris.Codegen.ModulePrinter
       ( printModule
       ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.String.Extra (camelCase)
import Polaris.Codegen.PrinterUtils (lines, printRefName)
import Polaris.Codegen.Types (Module, PropEntry, Typ(..), PSJSContent)

printModule :: Module -> PSJSContent
printModule m =
  { psContent: printPSModule m
  , jsContent: printJSModule m
  }

printPSModule :: Module -> String
printPSModule { name, props } = lines
  [ "module Polaris.Components." <> name
  , "  ( " <> elFnName'
  , "  , " <> rcFnName'
  , "  , " <> propsName
  , "  ) where"
  , ""
  , "import Prelude"
  , ""
  , "import Effect"
  , "import Effect.Uncurried"
  , "import Foreign"
  , "import Literals"
  , "import React.Basic.Hooks"
  , "import Polaris.Types"
  , "import Untagged.Coercible"
  , "import Untagged.Union"
  , ""
  , "type " <> propsName <> " ="
  , "  { " <> propsContent
  , "  }"
  , ""
  , elFnName' <> " :: forall r. Coercible r " <> propsName <> " => r -> JSX"
  , elFnName' <> " = element " <> rcFnName' <> " <<< coerce"
  , ""
  , "foreign import " <> rcFnName' <> " :: ReactComponent " <> propsName
  , ""
  ]
  where
    moduleName = name
    propsName = name <> "Props"
    elFnName' = elFnName name
    rcFnName' = rcFnName name

    propsContent = Array.intercalate "\n  , " $ printPropEntry <$> props

printJSModule :: Module -> String
printJSModule { name } =
  "exports."
  <> rcFnName name
  <> " = require(\"@shopify/polaris\")."
  <> name
  <> ";\n"


printPropEntry :: PropEntry -> String
printPropEntry { name, description, required, typ } =
  "\"" <> name <> "\" :: " <> t
  where
    t' = printTyp typ

    t = if required
        then t'
        else "UndefinedOr (" <> t' <> ")"

printTyp :: Typ -> String
printTyp TypUnit = "Unit"
printTyp TypString = "String"
printTyp TypBoolean = "Boolean"
printTyp TypNumber = "Number"
printTyp TypJSX = "JSX"
printTyp (TypStringLiteral s) = "StringLit \"" <> s <> "\""
printTyp (TypBooleanLiteral b) = "BooleanLit \"" <> show b <> "\""
printTyp (TypArray t) = "Array " <> printTypWrapped t
printTyp (TypUnion ts) =
  Array.intercalate " |+| " $ printTypWrapped <$> (NonEmptyArray.toArray ts)
printTyp (TypRecord rs) =
  "{"
  <> ( Array.intercalate " , "
       $ (\ {name, typ} -> "\"" <> name <> "\" :: (" <> printTyp typ <> ")") <$> rs
     )
  <> "}"
printTyp (TypRef ns) =
  printRefName ns

printTyp TypForeign = "Foreign"
printTyp (TypFn { params, out }) = case params of
  [] -> "Effect " <> outPart
  _ ->
    "EffectFn" <> (show $ Array.length params) <> " "
    <> ( Array.intercalate " " $ printTypWrapped <$> params) <> " "
    <> outPart
  where
    outPart = printTypWrapped out

printTypWrapped :: Typ -> String
printTypWrapped t = "(" <> printTyp t <> ")"

--

elFnName :: String -> String
elFnName = camelCase

rcFnName :: String -> String
rcFnName name = (elFnName name) <> "RC"
