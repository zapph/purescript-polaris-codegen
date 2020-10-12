module Polaris.Codegen.CSTSimple
       ( CodegenError(..)
       , ModuleBuilder
       , mkModule
       , declareType
       , declareValue
       , declareForeignValue
       , declareForeignData
       , class AsExport
       , asExport
       , export_
       , export
       , declare
       ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.State (class MonadState, StateT, execStateT, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Language.PS.CST (Export(..))
import Language.PS.SmartCST (DataHead(..), Declaration(..), Expr, Foreign(..), Guarded(..), Ident, Kind, Module(..), ModuleName, ProperName, ProperNameType_TypeName, Type)
import Polaris.Codegen.Names as Name
import Polaris.Codegen.Names as Names

data CodegenError =
  CEInvalidModuleName String
  | CEInvalidProperName String
  | CEInvalidIdent String

derive instance codegenErrorGeneric :: Generic CodegenError _

instance codegenErrorShow :: Show CodegenError where
  show = genericShow

type ModuleBuilderState =
  { iexports :: List Export
  , idecls :: List Declaration
  }

newtype ModuleBuilder a =
  ModuleBuilder (StateT ModuleBuilderState (Either CodegenError) a)

derive newtype instance moduleBuilderFunctor :: Functor ModuleBuilder
derive newtype instance moduleBuilderApply :: Apply ModuleBuilder
derive newtype instance moduleBuilderApplicative :: Applicative ModuleBuilder
derive newtype instance moduleBuilderBind :: Bind ModuleBuilder
derive newtype instance moduleBuilderMonad :: Monad ModuleBuilder
derive newtype instance moduleBuilderMonadThrow :: MonadThrow CodegenError ModuleBuilder
derive newtype instance moduleBuilderMonadError :: MonadError CodegenError ModuleBuilder
derive newtype instance moduleBuilderMonadState ::
  MonadState
  { iexports :: List Export
  , idecls :: List Declaration
  }
  ModuleBuilder

mkModule :: String -> ModuleBuilder Unit -> Either CodegenError Module
mkModule name b = do
  moduleName <- note (CEInvalidModuleName name) $ Names.moduleName name
  mkModule' moduleName b

mkModule' :: ModuleName -> ModuleBuilder Unit -> Either CodegenError Module
mkModule' moduleName (ModuleBuilder statet) =
  execStateT statet mempty <#> \{ iexports, idecls } ->
  Module { moduleName
         , exports: ilistToArray iexports
         , declarations: ilistToArray idecls
         }

-- todo add typevar binding options
declareType :: String -> Type -> ModuleBuilder (ProperName ProperNameType_TypeName)
declareType name type_ = do
  pname <- mkProperName name
  declare $ DeclType
    { comments: Nothing
    , head: DataHead
      { dataHdName: pname
      , dataHdVars: []
      }
    , type_
    }
  pure pname

declareValue :: String -> Type -> Expr -> ModuleBuilder Ident
declareValue name type_ expr = do
  iname <- mkIdent name
  declare $ DeclSignature
    { comments: Nothing
    , ident: iname
    , type_
    }
  declare $ DeclValue
    { comments: Nothing
    , valueBindingFields:
      { name: iname
      , binders: []
      , guarded: Unconditional
        { whereBindings: []
        , expr
        }
      }
    }
  pure iname

declareForeignValue :: String -> Type -> ModuleBuilder Ident
declareForeignValue name type_ = do
  iname <- mkIdent name
  declare $ DeclForeign
    { comments: Nothing
    , foreign_: ForeignValue
      { ident: iname
      , type_
      }
    }
  pure iname

declareForeignData :: String -> Kind -> ModuleBuilder (ProperName ProperNameType_TypeName)
declareForeignData name kind_ = do
  pname <- mkProperName name
  declare $ DeclForeign
    { comments: Nothing
    , foreign_: ForeignData
      { name: pname
      , kind_
      }
    }
  pure pname

class AsExport a where
  asExport :: a -> Export

instance asExportProperNameTypeName :: AsExport (ProperName ProperNameType_TypeName) where
  asExport pname = ExportType pname Nothing

instance asExportIdent :: AsExport Ident where
  asExport = ExportValue

export_ :: forall a. AsExport a => a -> ModuleBuilder Unit
export_ a =
  modify_ (\s -> s { iexports = asExport a : s.iexports})

export :: forall a. AsExport a => a -> ModuleBuilder a
export a = export_ a $> a

declare :: Declaration -> ModuleBuilder Unit
declare decl =
  modify_ (\s -> s { idecls = decl : s.idecls } )

-- Utils

mkProperName :: forall proxy. String -> ModuleBuilder (ProperName proxy)
mkProperName n =
  lift' $ note (CEInvalidProperName n) $ Name.properName n

mkIdent :: String -> ModuleBuilder Ident
mkIdent n =
  lift' $ note (CEInvalidIdent n) $ Name.ident n

lift' :: forall a. Either CodegenError a -> ModuleBuilder a
lift' = ModuleBuilder <<< lift

ilistToArray :: forall a. List a -> Array a
ilistToArray = Array.fromFoldable <<< List.reverse
