module Polaris.Codegen.ModulePrinter
       ( componentModuleBuilder
       ) where

import Prelude

import CST.Simple (cnst, exprIdent, exprIdent1, exprOp, knd, tvb, typApp, typCons, typForall, typOp, typRecord, typString, typVar, (*->), (*=>))
import CST.Simple as S
import CST.Simple.ModuleBuilder (addForeignData, addForeignJsValue, addType, addValue)
import Data.Array (length)
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String.Extra (camelCase)
import Data.Tuple.Nested ((/\))
import Polaris.Codegen.PrinterUtils (isCommonType, printRefName, printRefNameConstructor)
import Polaris.Codegen.Types (ComponentSpec, Module, Typ(..), TypeDef)

componentModuleBuilder :: Module -> S.ModuleBuilder Unit
componentModuleBuilder { typeDefs, specs } = do
  traverse_ addComponent specs
  traverse_ addTypeDef typeDefs

addComponent :: ComponentSpec -> S.ModuleBuilder Unit
addComponent { namePath, props } = do
  addType
    { export: true
    , name: propsName
    , typeVarBindings: []
    , type_: toType (TypRecord props)
    }
  addValue
    { export: true
    , name: elFnName
    , type_: typForall [ tvb "r" ] $
      cnst "Untagged.Coercible.Coercible" [ typVar "r", typCons propsName ] *=>
      typVar "r" *->
      typCons "React.Basic.Hooks.JSX"
    , binders: []
    , expr:
      exprOp
      (exprIdent1 "React.Basic.Hooks.element" (exprIdent rcFnName))
      "Prelude.(<<<)"
      (exprIdent "Untagged.Coercible.coerce")
    }
  addForeignJsValue
    { export: true
    , name: rcFnName
    , type_:
      typApp (typCons "React.Basic.Hooks.ReactComponent") [typCons propsName]
    , jsExpr: "require(\"@shopify/polaris\")." <> jsPath
    }

  where
    name = Array.fold namePath

    propsName = name <> "Props"
    elFnName = camelCase name
    rcFnName = elFnName <> "RC"

    jsPath = Array.intercalate "." namePath

addTypeDef :: TypeDef -> S.ModuleBuilder Unit
addTypeDef { name, typ } =
  case typ of
    Just t -> do
      addType
        { export: true
        , name: refName
        , typeVarBindings: []
        , type_: toType t
        }
      when (isTypRecord t) $ addValue
        { export: true
        , name: consName
        , type_: typForall [ tvb "r" ] $
          cnst "Untagged.Coercible.Coercible" [ typVar "r", typCons refName ] *=>
          typVar "r" *->
          typCons refName
        , binders: []
        , expr: exprIdent "Untagged.Coercible.coerce"
        }
    Nothing -> do
      addForeignData
        { export: true
        , name: refName
        , kind_: knd "Type"
        }
  where
    refName = printRefName name
    consName = printRefNameConstructor name

    isTypRecord (TypRecord _) = true
    isTypRecord _ = false


toType' :: Boolean -> Typ -> S.Type
toType' false typ =
  typApp (typCons "Untagged.Union.UndefinedOr") [ toType typ ]
toType' true typ =
  toType typ

toType :: Typ -> S.Type
toType TypUnit = typCons "Prelude.Unit"
toType TypString = typCons "String"
toType TypBoolean = typCons "Boolean"
toType TypNumber = typCons "Number"
toType TypJSX = typCons "React.Basic.Hooks.JSX"
toType (TypStringLiteral s) =
  typApp (typCons "Literals.StringLit") [ typString s ]
toType (TypBooleanLiteral b) =
  typApp (typCons "Literals.BooleanLit") [ typString (show b) ]
toType (TypArray a) =
  typApp (typCons "Array") [ toType a ]
toType (TypUnion as) = case tail' of
  Nothing -> hdType
  Just tl ->
    typOp
    hdType
    "Untagged.Union.(|+|)"
    (toType (TypUnion tl))

  where
    { head, tail } = NEArray.uncons as
    tail' = NEArray.fromArray tail

    hdType = toType head
toType (TypRecord props) =
  typRecord (toRowLabel <$> props) Nothing
  where
    toRowLabel { name, required, typ } =
      name /\ toType' required typ
toType TypForeign =
  typCons "Foreign.Foreign"
toType (TypRef n) =
  if isCommonType nm
  then typCons $ "Polaris.Types." <> nm
  else typCons nm
  where
    nm = printRefName n
toType (TypFn { params, out }) =
  typApp cons $ toType <$> consParams

  where
    cons = case length params of
      0 ->
        typCons "Effect.Effect"
      n ->
        typCons $ "Effect.Uncurried.EffectFn" <> show n

    consParams = Array.snoc params out
