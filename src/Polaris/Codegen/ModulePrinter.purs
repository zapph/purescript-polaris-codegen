module Polaris.Codegen.ModulePrinter
       ( printModule
       ) where

import Prelude

import Data.Array (length)
import Data.Array as Array
import Data.Array.NonEmpty (singleton, (:))
import Data.Array.NonEmpty as NEArray
import Data.Either (Either)
import Data.Foldable (foldl, traverse_)
import Data.Maybe (Maybe(..))
import Data.String.Extra (camelCase)
import Data.Tuple.Nested ((/\))
import Language.PS.CST (Export)
import Language.PS.SmartCST (Constraint(..), Declaration, Expr(..), Ident(..), Kind(..), ModuleName(..), OpName(..), ProperName(..), SmartQualifiedName(..), Type(..), TypeVarBinding(..), arrayType, booleanType, mkModuleName, mkRowLabels, numberType, stringType, (====>>))
import Polaris.Codegen.CSTSimple (CodegenError, ModuleBuilder, declareForeignData, declareForeignValue, declareType, declareValue, export_, mkModule)
import Polaris.Codegen.PrinterUtils (isCommonType, lines, printCST, printRefName, printRefNameConstructor)
import Polaris.Codegen.Types (ComponentSpec, Module, PSJSContent, Typ(..), TypeDef)

printModule :: Module -> Either CodegenError PSJSContent
printModule { name, psImports, typeDefs, specs } = do
  psContent <- printPSContent name mb
  pure
    { psContent
    , jsContent: printJSContent codeJsCodes
    }

  where
    mb = do
      traverse_ mkComponentCode specs
      traverse_ mkTypeDef typeDefs

    -- TODO integreate this back to ModuleBuilder
    codeJsCodes = mkJSCode <$> specs

printPSContent ::
  String ->
  ModuleBuilder Unit ->
  Either CodegenError String
printPSContent name mb =
  printCST <$> mkModule ("Polaris.Components." <> name) mb

printJSContent :: Array String -> String
printJSContent jsCodes =
  lines jsCodes <> "\n"

type ComponentCode =
  { exports :: Array Export
  , psDecls :: Array Declaration
  }

mkJSCode :: ComponentSpec -> String
mkJSCode { namePath, props } =
  "exports."
    <> rcFnName
    <> " = require(\"@shopify/polaris\")."
    <> jsPath
    <> ";"

  where
    -- todo remove these duplicates
    name = Array.fold namePath
    elFnName = camelCase name
    rcFnName = elFnName <> "RC"

    jsPath = Array.intercalate "." namePath

mkComponentCode :: ComponentSpec -> ModuleBuilder Unit
mkComponentCode { namePath, props } = do
  export_ =<< declareType propsName (toType (TypRecord props))
  export_ =<< declareValue elFnName elDeclType elDeclExpr
  export_ =<< declareForeignValue rcFnName rcType

  where
    name = Array.fold namePath

    propsName = name <> "Props"
    elFnName = camelCase name
    rcFnName = elFnName <> "RC"

    elDeclType =
      TypeForall (singleton (TypeVarName (Ident "r")))
      ( TypeConstrained
        ( Constraint
          { className: SmartQualifiedName__Simple (mkModuleName $ "Untagged" : singleton "Coercible") (ProperName "Coercible")
          , args: [ TypeVar (Ident "r")
                  , TypeConstructor (SmartQualifiedName__Ignore (ProperName propsName))
                  ]
          }
        )
        ((TypeVar (Ident "r")
          ====>>
          ( TypeConstructor (SmartQualifiedName__Simple (mkModuleName $ "React" : "Basic" : singleton "Hooks") (ProperName "JSX"))
          )
         ))
      )

    elDeclExpr =
      ExprOp
      (ExprApp
       (ExprIdent
        (SmartQualifiedName__Simple (mkModuleName $ "React" : "Basic" : singleton "Hooks") (Ident "element"))
       )
       (ExprIdent (SmartQualifiedName__Ignore (Ident rcFnName)))
      )
      (SmartQualifiedName__Simple (mkModuleName $ singleton "Prelude") (OpName "<<<"))
      (ExprIdent (SmartQualifiedName__Simple (mkModuleName $ "Untagged" : singleton "Coercible") (Ident "coerce")))

    rcType =
      TypeApp
      ( TypeConstructor
        (SmartQualifiedName__Simple (mkModuleName $ "React" : "Basic" : singleton "Hooks") (ProperName "ReactComponent"))
      )
      ( TypeConstructor
        (SmartQualifiedName__Ignore (ProperName propsName))
      )

toType' :: Boolean -> Typ -> Type
toType' false typ =
  TypeApp
  ( TypeConstructor $ (SmartQualifiedName__Simple $ mkModuleName $ "Untagged" : singleton "Union")
    (ProperName "UndefinedOr")
  ) (toType typ)
toType' true typ = toType typ

toType :: Typ -> Type
toType TypUnit =
  TypeConstructor $ SmartQualifiedName__Simple
  (ModuleName $ singleton (ProperName "Prelude"))
  (ProperName "Unit")
toType TypString = stringType
toType TypBoolean = booleanType
toType TypNumber = numberType
toType TypJSX =
  TypeConstructor $ SmartQualifiedName__Simple
  (mkModuleName $ "React" : "Basic" : singleton "Hooks")
  (ProperName "JSX")
toType (TypStringLiteral s) =
  TypeApp
  ( TypeConstructor $ SmartQualifiedName__Simple
    (mkModuleName $ singleton "Literals")
    (ProperName "StringLit")
  ) (TypeString s)
toType (TypBooleanLiteral b) =
  TypeApp
  ( TypeConstructor $ SmartQualifiedName__Simple
    (mkModuleName $ singleton "Literals")
    (ProperName "BooleanLit")
  ) (TypeString $ show b)
toType (TypArray a) =
  arrayType $ toType a
toType (TypUnion as) = case tail' of
  Nothing -> hdType
  Just tl ->
    TypeOp
    hdType
    ( SmartQualifiedName__Simple
      (mkModuleName $ "Untagged" : singleton "Union" )
      (OpName "|+|")
    )
    (toType (TypUnion tl))

  where
    { head, tail } = NEArray.uncons as
    tail' = NEArray.fromArray tail

    hdType = toType head
toType (TypRecord props) =
  TypeRecord
  { rowLabels: mkRowLabels $ toRowLabel <$> props
  , rowTail: Nothing
  }
  where
    toRowLabel { name, required, typ } =
      name /\ toType' required typ

toType TypForeign =
  TypeConstructor $ SmartQualifiedName__Simple
  (mkModuleName $ singleton "Foreign")
  (ProperName "Foreign")
toType (TypRef n) =
  TypeConstructor $
  if isCommonType n
  then SmartQualifiedName__Simple (mkModuleName $ "Polaris" : singleton "Types") nm
  else SmartQualifiedName__Ignore nm
  where
    nm = ProperName (printRefName n)
toType (TypFn { params, out }) =
  foldl (\acc t -> TypeApp acc (toType t)) cons consParams

  where
    cons = TypeConstructor $ case length params of
      0 ->
        SmartQualifiedName__Simple
        (mkModuleName $ singleton "Effect")
        (ProperName "Effect")
      n ->
        SmartQualifiedName__Simple
        (mkModuleName $ "Effect" : singleton "Uncurried")
        (ProperName ("EffectFn" <> show n))

    consParams = Array.snoc params out

mkTypeDef :: TypeDef -> ModuleBuilder Unit
mkTypeDef { name, typ } =
  case typ of
    Just t@(TypRecord props) -> do
      -- type <refName> = { ... }
      export_ =<< declareType refName (toType t)

      -- <consName> :: forall r. Coercible r <refName> => r -> <refName>
      -- <consName> = coerce
      export_ =<< declareValue consName
        (TypeForall (singleton (TypeVarName (Ident "r"))) $
         TypeConstrained
         ( Constraint
           { className: SmartQualifiedName__Simple (mkModuleName $ "Untagged" : singleton "Coercible") (ProperName "Coercible")
           , args: [ TypeVar (Ident "r")
                   , TypeConstructor (SmartQualifiedName__Ignore (ProperName refName))
                   ]
           }
         )
         ((TypeVar (Ident "r"))
          ====>> (TypeConstructor (SmartQualifiedName__Ignore (ProperName refName)))
         )
        )
        ( ExprIdent (SmartQualifiedName__Simple (mkModuleName $ "Untagged" : singleton "Coercible") (Ident "coerce"))
        )

    Just t -> do
      export_ =<< declareType refName (toType t)
    Nothing -> do
      export_ =<< declareForeignData refName (KindName (SmartQualifiedName__Ignore (ProperName "Type")))
      pure unit

  where
    refName = printRefName name
    consName = printRefNameConstructor name
