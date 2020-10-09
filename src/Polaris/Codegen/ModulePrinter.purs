module Polaris.Codegen.ModulePrinter
       ( printModule
       ) where

import Prelude

import Data.Array (length)
import Data.Array as Array
import Data.Array.NonEmpty (singleton, (:))
import Data.Array.NonEmpty as NEArray
import Data.Foldable (fold, foldl)
import Data.Maybe (Maybe(..))
import Data.String.Extra (camelCase)
import Data.Tuple.Nested ((/\))
import Language.PS.CST (Export(..))
import Language.PS.SmartCST (Constraint(..), DataHead(..), Declaration(..), Expr(..), Foreign(..), Guarded(..), Ident(..), Kind(..), ModuleName(..), OpName(..), ProperName(..), SmartQualifiedName(..), Type(..), TypeVarBinding(..), arrayType, booleanType, mkModuleName, mkRowLabels, numberType, stringType, (====>>))
import Language.PS.SmartCST as SmartCST
import Polaris.Codegen.PrinterUtils (isCommonType, lines, printCST, printRefName, printRefNameConstructor)
import Polaris.Codegen.Types (ComponentSpec, Module, PSJSContent, Typ(..), TypeDef)

printModule :: Module -> PSJSContent
printModule { name, psImports, typeDefs, specs } =
  { psContent: printPSContent { name, exports, psDecls }
  , jsContent: printJSContent codeJsCodes
  }

  where
    codes = mkComponentCode <$> specs
    codeExports = fold $ _.exports <$> codes
    codePsDecls = fold $ _.psDecls <$> codes
    codeJsCodes = _.jsCode <$> codes

    defs = printTypeDef <$> typeDefs
    defExports = fold $ _.exports <$> defs
    defPsDecls = fold $ _.decls <$> defs

    exports = codeExports <> defExports
    psDecls = codePsDecls <> defPsDecls

printPSContent ::
  { name :: String
  , exports :: Array Export
  , psDecls :: Array Declaration
  } ->
  String
printPSContent { name, exports, psDecls } =
  printCST $ SmartCST.Module
  { moduleName: mkModuleName $ "Polaris" : "Components" : singleton name
  , exports
  , declarations: psDecls
  }

printJSContent :: Array String -> String
printJSContent jsCodes =
  lines jsCodes <> "\n"

type ComponentCode =
  { exports :: Array Export
  , psDecls :: Array Declaration
  , jsCode :: String
  }

mkComponentCode :: ComponentSpec -> ComponentCode
mkComponentCode { namePath, props } =
  { exports:
    [ propsExport
    , elExport
    , rcExport
    ]
  , psDecls:
    [ propsDecl
    , elDeclSignature
    , elDeclValue
    , rcDecl
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

    propsExport = ExportType (ProperName propsName) Nothing
    elExport = ExportValue (Ident elFnName)
    rcExport = ExportValue (Ident rcFnName)

    propsDecl = DeclType
      { comments: Nothing
      , head: DataHead
        { dataHdName: ProperName propsName
        , dataHdVars: []
        }
      , type_: toType (TypRecord props)
      }

    -- <elFnName> :: forall r. Coercible r <propsName> => r -> JSX
    elDeclSignature = DeclSignature
      { comments: Nothing
      , ident: Ident elFnName
      , type_: TypeForall (singleton (TypeVarName (Ident "r")))
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
      }

    -- <elFnName> = element <rcFnName> <<< coerce
    elDeclValue = DeclValue
      { comments: Nothing
      , valueBindingFields:
        { name: Ident elFnName
        , binders: []
        , guarded: Unconditional
          { whereBindings: []
          , expr: ExprOp
            (ExprApp
             (ExprIdent
              (SmartQualifiedName__Simple (mkModuleName $ "React" : "Basic" : singleton "Hooks") (Ident "element"))
             )
             (ExprIdent (SmartQualifiedName__Ignore (Ident rcFnName)))
            )
            (SmartQualifiedName__Simple (mkModuleName $ singleton "Prelude") (OpName "<<<"))
            (ExprIdent (SmartQualifiedName__Simple (mkModuleName $ "Untagged" : singleton "Coercible") (Ident "coerce")))
          }
        }
      }

    rcDecl = DeclForeign
      { comments: Nothing
      , foreign_: ForeignValue
        { ident: Ident rcFnName
        , type_: TypeApp
          ( TypeConstructor
            (SmartQualifiedName__Simple (mkModuleName $ "React" : "Basic" : singleton "Hooks") (ProperName "ReactComponent"))
          )
          ( TypeConstructor
            (SmartQualifiedName__Ignore (ProperName propsName))
          )
        }
      }

    jsPath = Array.intercalate "." namePath

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

printTypeDef
  :: TypeDef
     -> { decls :: Array Declaration
        , exports :: Array Export
        }
printTypeDef { name, typ } =
  case typ of
    Just t@(TypRecord props) ->
      -- type <refName> = { ... }
      -- <consName> :: forall r. Coercible r <refName> => r -> <refName>
      -- <consName> = coerce
      { decls:
        [ typeDecl t

         -- <consName> :: forall r. Coercible r <refName> => r -> <refName>

        , DeclSignature
          { comments: Nothing
          , ident: Ident consName
          , type_: TypeForall (singleton (TypeVarName (Ident "r"))) $
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
          }

        , DeclValue
          { comments: Nothing
          , valueBindingFields:
            { name: Ident consName
            , binders: []
            , guarded: Unconditional
              { whereBindings: []
              , expr: ExprIdent (SmartQualifiedName__Simple (mkModuleName $ "Untagged" : singleton "Coercible") (Ident "coerce"))
              }
            }
          }
        ]
      , exports: [ typeExport, consExport ]
      }
    Just t ->
      -- type <refName> = <type>
      { decls:
        [ typeDecl t
        ]
      , exports: [ typeExport ]
      }
    Nothing ->
      -- foreign import data <refName> " :: Type
      { decls:
        [ DeclForeign
          { comments: Nothing
          , foreign_: ForeignData
            { name: ProperName refName
            , kind_: KindName (SmartQualifiedName__Ignore (ProperName "Type"))
            }
          }
        ]
      , exports: [ typeExport ]
      }
  where
    refName = printRefName name
    consName = printRefNameConstructor name

    typeExport = ExportType (ProperName refName) Nothing
    consExport = ExportValue (Ident consName)

    typeDecl t =
      DeclType
      { comments: Nothing
      , head: DataHead
        { dataHdName: ProperName refName
        , dataHdVars: []
        }
      , type_: toType t
      }

    exports = case typ of
      Just (TypRecord props) ->
        [ typeExport, consExport ]
      _ ->
        [ typeExport ]
