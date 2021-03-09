module Polaris.Codegen.ModulePrinter
       ( componentModuleBuilder
       ) where

import Prelude

import CST.Simple (exprIdent, tvb, typCons, typConstrained, typForall, typRecord, typRecord_, typRow_, typVar, (*->), (*=>))
import CST.Simple as S
import CST.Simple.ModuleBuilder (addForeignData, addForeignJsValue, addType, addValue)
import Data.Array (length)
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String.Extra (camelCase)
import Data.Tuple.Nested (type (/\), (/\))
import Polaris.Codegen.Defs (cnst_Coercible, expr_coerce, expr_elem, expr_elemWithChildren, knd_Type, typOp_OneOf, typ_Array, typ_Effect, typ_EffectFn, typ_JSX, typ_PropsWithChildren, typ_ReactComponent, typ_UndefinedOr)
import Polaris.Codegen.PrinterUtils (isCommonType, printRefName, printRefNameConstructor)
import Polaris.Codegen.Types (ComponentSpec, Module, Typ(..), TypeDef, Prop)

type Names =
  { basePropsName' :: String
  , basePropsName :: String
  , propsName :: String
  , elFnName :: String
  , rcFnName :: String
  , jsPath :: String
  }

componentModuleBuilder :: Module -> S.ModuleBuilder Unit
componentModuleBuilder { typeDefs, specs } = do
  traverse_ addComponent specs
  traverse_ addTypeDef typeDefs

addComponent :: ComponentSpec -> S.ModuleBuilder Unit
addComponent { namePath, baseProps, hasJSXChildren } = do
  if hasJSXChildren
    then addChildrenCons n baseProps
    else addNoChildrenCons n baseProps
  addForeignJsValue
    { export: true
    , name: n.rcFnName
    , type_: typ_ReactComponent (typCons n.propsName)
    , jsExpr: "require(\"@shopify/polaris\")." <> n.jsPath
    }

  where
    n = mkNames namePath

addChildrenCons :: Names -> Array Prop -> S.ModuleBuilder Unit
addChildrenCons n baseProps = do
  addType
    { export: true
    , name: n.basePropsName'
    , typeVarBindings: []
    , type_: toTypeRow baseProps
    }
  addType
    { export: true
    , name: n.basePropsName
    , typeVarBindings: []
    , type_: typRecord [] (Just (typCons n.basePropsName'))
    }
  addType
    { export: true
    , name: n.propsName
    , typeVarBindings: []
    , type_: typ_PropsWithChildren (typCons n.basePropsName')
    }

  addValue
    { export: true
    , name: n.elFnName
    , type_: typForall [ tvb "r" ] $
      cnst_Coercible (typVar "r") (typCons n.basePropsName) *=>
      (typVar "r" *-> typ_Array typ_JSX *-> typ_JSX)
    , binders: []
    , expr: expr_elemWithChildren (exprIdent n.rcFnName)
    }


addNoChildrenCons :: Names -> Array Prop -> S.ModuleBuilder Unit
addNoChildrenCons n baseProps = do
  addType
    { export: true
    , name: n.propsName
    , typeVarBindings: []
    , type_: toType (TypRecord baseProps)
    }
  addValue
    { export: true
    , name: n.elFnName
    , type_: typForall [ tvb "r" ] $
      typConstrained
      (cnst_Coercible (typVar "r") (typCons n.propsName))
      (typVar "r" *-> typ_JSX)
    , binders: []
    , expr: expr_elem (exprIdent n.rcFnName)
    }

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
          cnst_Coercible (typVar "r") (typCons refName) *=>
          typVar "r" *->
          typCons refName
        , binders: []
        , expr: expr_coerce
        }
    Nothing -> do
      addForeignData
        { export: true
        , name: refName
        , kind_: knd_Type
        }
  where
    refName = printRefName name
    consName = printRefNameConstructor name

    isTypRecord (TypRecord _) = true
    isTypRecord _ = false


toType' :: Boolean -> Typ -> S.Type
toType' false typ =
  typ_UndefinedOr (toType typ)
toType' true typ =
  toType typ

toType :: Typ -> S.Type
toType (TypSType t) = t
toType (TypArray a) =
  typ_Array (toType a)
toType (TypUnion as) = case tail' of
  Nothing -> hdType
  Just tl ->
    typOp_OneOf
    hdType
    (toType (TypUnion tl))

  where
    { head, tail } = NEArray.uncons as
    tail' = NEArray.fromArray tail

    hdType = toType head
toType (TypRecord props) =
  typRecord_ (toRowLabel <$> props)
toType (TypRef n) =
  if isCommonType nm
  then typCons $ "Polaris.Types(" <> nm <> ")"
  else typCons nm
  where
    nm = printRefName n
toType (TypFn { params, out }) =
  case length params of
    0 ->
      typ_Effect (toType out)
    n ->
      typ_EffectFn n (toType <$> params) (toType out)

toTypeRow :: Array Prop -> S.Type
toTypeRow = typRow_ <<< map toRowLabel

toRowLabel :: Prop -> String /\ S.Type
toRowLabel { name, required, typ } =
  name /\ toType' required typ

mkNames :: Array String -> Names
mkNames namePath =
  { basePropsName': name <> "BaseProps'"
  , basePropsName: name <> "BaseProps"
  , propsName: name <> "Props"
  , elFnName
  , rcFnName: elFnName <> "RC"
  , jsPath: Array.intercalate "." namePath
  }
  where
    name = Array.fold namePath
    elFnName = camelCase name
