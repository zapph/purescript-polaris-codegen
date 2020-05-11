module Polaris.Codegen.ModulePrinter
       ( printModule
       ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.Extra (camelCase)
import Polaris.Codegen.Types (Module, PropEntry, Typ(..))

printModule :: Module -> String
printModule { name, props } = lines
  [ "module Polaris.Components." <> name
  , "  ( " <> elFnName
  , "  , " <> rcFnName
  , "  )"
  , ""
  , "import Prelude"
  , ""
  , "import Foreign"
  , "import React.Basic.Hooks"
  , "import Polaris.Types"
  , "import Untagged.Coercible"
  , "import Untagged.Union"
  , ""
  , "type Props ="
  , "  { " <> propsContent
  , "  }"
  , ""
  , elFnName <> " :: forall r. Coercible r Props => r -> JSX"
  , elFnName <> " = element " <> rcFnName <> " <<< coerce"
  , ""
  , "foreign import " <> rcFnName <> " :: ReactComponent Props"
  ]
  where
    lname = camelCase name

    moduleName = name
    elFnName = lname
    rcFnName = lname <> "RC"

    propsContent = Array.intercalate "\n  , " $ printPropEntry <$> props

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
  Array.intercalate " , " $ (\ {name, typ} -> "\"" <> name <> "\" :: (" <> printTyp typ <> ")") <$> rs
printTyp (TypRef ns) =
  Array.intercalate "__" $ String.replaceAll (Pattern ".") (Replacement "_") <$> ns

printTyp TypForeign = "Foreign"
printTyp (TypFn { params, out }) = case params of
  [] -> "Effect " <> outPart
  _ ->
    "EffectFn" <> (show $ Array.length params) <> " "
    <> ( Array.intercalate " " $ printTypWrapped <$> params)
    <> outPart
  where
    outPart = printTypWrapped out

printTypWrapped :: Typ -> String
printTypWrapped t = "(" <> printTyp t <> ")"

lines :: Array String -> String
lines = Array.intercalate "\n" >>> (_ <> "\n")
