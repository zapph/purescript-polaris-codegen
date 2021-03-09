module Polaris.Codegen.Defs
       where

import Prelude

import CST.Simple (Constraint, Expr, Kind, Type, cnst2, exprIdent, exprIdent1, knd, typCons, typCons1, typConsN, typOp, typString)
import Data.Array as Array

cnst_Coercible :: Type -> Type -> Constraint
cnst_Coercible =
  cnst2 "Untagged.Coercible(class Coercible)"

expr_coerce :: Expr
expr_coerce =
  exprIdent "Untagged.Coercible(coerce)"

expr_elem :: Expr -> Expr
expr_elem =
  exprIdent1 "Polaris.Internal(elem)"

expr_elemWithChildren :: Expr -> Expr
expr_elemWithChildren =
  exprIdent1 "Polaris.Internal(elemWithChildren)"

knd_Type :: Kind
knd_Type =
  knd "Type"

typ_Array :: Type -> Type
typ_Array =
  typCons1 "Array"

typ_Boolean :: Type
typ_Boolean =
  typCons "Boolean"

typ_BooleanLit :: Boolean -> Type
typ_BooleanLit b =
  typCons1 "Literals(BooleanLit)" (typString $ show b)

typ_Effect :: Type -> Type
typ_Effect =
  typCons1 "Effect(Effect)"

typ_EffectFn :: Int -> Array Type -> Type -> Type
typ_EffectFn n params out =
  typConsN ("Effect.Uncurried(EffectFn" <> show n <> ")") (Array.snoc params out)

typ_Foreign :: Type
typ_Foreign =
  typCons "Foreign(Foreign)"

typ_Number :: Type
typ_Number =
  typCons "Number"

typ_JSX :: Type
typ_JSX =
  typCons "React.Basic.Hooks(JSX)"

typ_PropsWithChildren :: Type -> Type
typ_PropsWithChildren =
  typCons1 "Polaris.Internal(PropsWithChildren)"

typ_ReactComponent :: Type -> Type
typ_ReactComponent =
  typCons1 "React.Basic.Hooks(ReactComponent)"

typ_String :: Type
typ_String =
  typCons "String"

typ_StringLit :: String -> Type
typ_StringLit s =
  typCons1 "Literals(StringLit)" (typString s)

typ_TranslationDictionary :: Type
typ_TranslationDictionary =
  typCons "Polaris.Components.AppProvider(TranslationDictionary)"

typ_UndefinedOr :: Type -> Type
typ_UndefinedOr =
  typCons1 "Untagged.Union(UndefinedOr)"

typ_Unit :: Type
typ_Unit =
  typCons "Prelude(Unit)"

typOp_OneOf :: Type -> Type -> Type
typOp_OneOf t1 t2 =
  typOp t1 "Untagged.Union(type (|+|))" t2
