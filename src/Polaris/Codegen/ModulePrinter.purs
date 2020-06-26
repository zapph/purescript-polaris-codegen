module Polaris.Codegen.ModulePrinter
       ( printModule
       ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Char.Unicode (isLetter)
import Data.Foldable (foldMap, intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (charAt)
import Data.String.Extra (camelCase)
import Data.Tuple (Tuple(..))
import Polaris.Codegen.PrinterUtils (lines, printRefName, printRefNameConstructor)
import Polaris.Codegen.Types (ComponentSpec, Module, PSImport(..), PSImportEntry(..), PSJSContent, Prop, Typ(..), TypeDef)

printModule :: Module -> PSJSContent
printModule { name, psImports, typeDefs, specs } =
  { psContent: printPSContent { name, exports, imports, psCodes }
  , jsContent: printJSContent jsCodes
  }

  where
    codes = mkComponentCode <$> specs

    printTypeDef' = (\d -> Tuple d.exports d.code) <<< printTypeDef
    (Tuple typeDefExports typeDefsCode) = foldMap printTypeDef' typeDefs

    exports = typeDefExports <> foldMap _.exports codes
    imports = printImport <$> psImports

    psCodes = Array.cons typeDefsCode (_.psCode <$> codes)
    jsCodes = _.jsCode <$> codes

printPSContent ::
  { name :: String
  , imports :: Array String
  , exports :: Array String
  , psCodes :: Array String
  } ->
  String
printPSContent { name, exports, imports, psCodes } = lines
  [ "module Polaris.Components." <> name
  , "  ( " <> exportsPart
  , "  ) where"
  , ""
  , importsPart
  , ""
  , psCodesPart
  ]
  where
    exportsPart = Array.intercalate "\n  , " exports
    importsPart = lines imports
    psCodesPart = lines psCodes

printJSContent :: Array String -> String
printJSContent jsCodes =
  lines jsCodes <> "\n"

type ComponentCode =
  { exports :: Array String
  , psCode :: String
  , jsCode :: String
  }

mkComponentCode :: ComponentSpec -> ComponentCode
mkComponentCode { namePath, props } =
  { exports: [ propsName, elFnName, rcFnName ]
  , psCode: lines
    [ "type " <> propsName <> " ="
    , "  { " <> propsContent
    , "  }"
    , ""
    , elFnName <> " :: forall r. Coercible r " <> propsName <> " => r -> JSX"
    , elFnName <> " = element " <> rcFnName <> " <<< coerce"
    , ""
    , "foreign import " <> rcFnName <> " :: ReactComponent " <> propsName
    , ""
    ]
  , jsCode:
    "exports."
    <> rcFnName
    <> " = require(\"@shopify/polaris\")."
    <> jsPath
    <> ";"
  }

  where
    name = Array.fold namePath

    propsName = name <> "Props"
    elFnName = camelCase name
    rcFnName = elFnName <> "RC"

    jsPath = Array.intercalate "." namePath

    propsContent =
      Array.intercalate "\n  , " $ printProp <$> props

printProp :: Prop -> String
printProp { name, description, required, typ } =
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
printTyp (TypRecord props) =
  "{"
  <> ( Array.intercalate ", " $ printProp <$> props
     )
  <> "}"
printTyp (TypRef ns) =
  printRefName ns

printTyp TypForeign = "Foreign"
printTyp (TypFn { params, out }) = case params of
  [] -> "Effect " <> outPart
  _ ->
    "EffectFn" <> (show $ Array.length params) <> " "
    <> (Array.intercalate " " $ printTypWrapped <$> params) <> " "
    <> outPart
  where
    outPart = printTypWrapped out

printTypWrapped :: Typ -> String
printTypWrapped t = "(" <> printTyp t <> ")"

printTypeDef
  :: TypeDef
     -> { code :: String
        , exports :: Array String
        }
printTypeDef { name, typ } = { code, exports }
  where
    refName = printRefName name
    consName = printRefNameConstructor name

    code = case typ of
      Just (TypRecord props) ->
        printTypeDefRecord { refName, consName } props
      Just t ->
        "type " <> printRefName name <> " = " <> printTyp t <> "\n"
      Nothing ->
        "foreign import data " <> printRefName name <> " :: Type\n"

    exports = case typ of
      Just (TypRecord props) ->
        [ refName, consName ]
      _ ->
        [ refName ]

printTypeDefRecord
  :: { refName :: String
     , consName :: String
     }
  -> Array Prop
  -> String
printTypeDefRecord { refName, consName } props =
  lines
  [ "type " <> refName <> " ="
  , "  { " <> propsContent
  , "  }"
  , ""
  , consName <> " :: forall r. Coercible r " <> refName <> " => r -> " <> refName
  , consName <> " = coerce"
  , ""
  ]
  where
    propsContent =
      Array.intercalate "\n  , " $ printProp <$> props

printImport ::
  PSImport ->
  String
printImport PSIPrelude = "import Prelude"
printImport (PSIModule name entries) =
  "import " <> name <> " (" <> (intercalate ", " $ printImportEntry <$> entries) <> ")"

printImportEntry :: PSImportEntry -> String
printImportEntry (PSIEClass n) = "class " <> n
printImportEntry (PSIEType n) =
  if isSymbolicName n
  then "type (" <> n <> ")"
  else n
printImportEntry (PSIEFn n) =
  if isSymbolicName n
  then "(" <> n <> ")"
  else n

isSymbolicName :: String -> Boolean
isSymbolicName n =
  fromMaybe false (not <<< isLetter <$> charAt 0 n)
