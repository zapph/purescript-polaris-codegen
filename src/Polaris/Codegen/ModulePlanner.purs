module Polaris.Codegen.ModulePlanner
       ( planModule
       ) where

import Prelude

import Control.Monad.Except (lift)
import Control.Monad.State (StateT, get, modify_, runStateT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (foldl, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Polaris.Codegen.PrinterUtils (isCommonType)
import Polaris.Codegen.TypParser (parseTyp)
import Polaris.Codegen.Types (ComponentSpec, Module, PSImport(..), PSImportEntry(..), Prop, RawComponent, RawProp(..), Typ(..), TypeDef)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (eof)

planModule
  :: { name :: String
     , rawProps :: Array RawProp
     , rawSubComponents :: Array RawComponent
     }
     -> Either String Module
planModule { name, rawProps, rawSubComponents } = uncurry mkModule <$> runStateT plan { importPrelude: false, imports: Map.empty, typeDefMap: Map.empty }
  where
    plan = do
      mainSpec <- readRawComponent' mainToNamePath { name, rawProps }
      subSpec <- traverse (readRawComponent' subToNamePath) rawSubComponents
      let specs = Array.cons mainSpec subSpec
      pure { name, specs }

    mkComponentSpec
      :: (String -> Array String)
         -> { name :: String, props :: Array Prop }
         -> ComponentSpec
    mkComponentSpec toNamePath { name: n, props } =
      { namePath: toNamePath n
      , props
      }

    readRawComponent' toNamePath r = mkComponentSpec toNamePath <$> readRawComponent r
    mainToNamePath = Array.singleton
    subToNamePath n = [ name, n ]

    mkModule { name: n, specs } { typeDefMap, importPrelude, imports } =
      { name: n
      , psImports:
        (if importPrelude then [ PSIPrelude ] else [])
        <> (uncurry toPSIModule <$> Map.toUnfoldable imports)

      , specs
      , typeDefs: Array.fromFoldable $ Map.values typeDefMap
      }

    toPSIModule n es = PSIModule n (Set.toUnfoldable es)

type St =
  { importPrelude :: Boolean
  , imports :: Map String (Set PSImportEntry)
  , typeDefMap :: Map String TypeDef
  }
type F a = StateT St (Either String) a

readRawComponent :: RawComponent -> F { name :: String, props :: Array Prop }
readRawComponent { name, rawProps } = do
  addComponentImports
  (traverse readRawProp) rawProps <#> { name, props: _ }

  where
    addComponentImports = do
      addPreludeImport
      addImport
        "React.Basic.Hooks"
        [ PSIEType "JSX"
        , PSIEType "ReactComponent"
        , PSIEFn "element"
        ]
      addImport
        "Untagged.Coercible"
        [ PSIEClass "Coercible"
        , PSIEFn "coerce"
        ]


readRawProp :: RawProp -> F Prop
readRawProp (RawProp r) = do
  unless r.mandatory importUndefinedOr
  typ <- (lift $ readTyp' r."type") >>= fillInTypDef r.types
  pure
    { name: r.name
    , typ
    , required: r.mandatory
    , description: r.description
    }

  where
    readTyp' s = lmap (showParseError s) (runParser s (parseTyp <* eof))
    showParseError s e = "Given: " <> s <> ", Error: " <> show e

    importUndefinedOr =
      addImport "Untagged.Union" [ PSIEType "UndefinedOr" ]

fillInTypDef :: Maybe (Array RawProp) -> Typ -> F Typ
fillInTypDef _ (TypRef name) | isCommonType name = do
  addImport "Polaris.Types" [ PSIEType name ]
  pure $ TypRef name
fillInTypDef (Just rp) (TypRef name) = do
  props <- traverse readRawProp rp
  TypRef <$> recordTypDef { name, typ: Just $ TypRecord props }
fillInTypDef Nothing (TypRef name) =
  TypRef <$> recordTypDef { name, typ: Nothing }
fillInTypDef rp (TypArray tr) = do -- todo limit this only to @(TypRef _)?
  -- for array types, the rawprops on the current node
  tr' <- fillInTypDef rp tr
  pure $ TypArray tr'
fillInTypDef rp (TypUnion typs) = do
  addImport "Untagged.Union" [ PSIEType "|+|" ]
  TypUnion <$> traverse (fillInSubTypDef rp) typs
fillInTypDef rp (TypFn { params, out }) = do
  addFnImport
  params' <- traverse (fillInSubTypDef rp) params
  out' <- fillInSubTypDef rp out
  pure $ TypFn { params, out }

  where
    addFnImport = case Array.length params of
      0 -> addImport "Effect" [ PSIEType "Effect" ]
      n -> addImport "Effect.Uncurried" [ PSIEType $ "EffectFn" <> show n ]
fillInTypDef _ t@(TypStringLiteral _) = do
  addImport "Literals" [ PSIEType "StringLit" ] $> t
fillInTypDef _ t@(TypBooleanLiteral _) = do
  addImport "Literals" [ PSIEType "BooleanLit" ] $> t
fillInTypDef _ t@TypJSX = do
  addImport "React.Basic.Hooks" [ PSIEType "JSX" ] $> t
fillInTypDef _ t@TypForeign = do
  addImport "Foreign" [ PSIEType "Foreign" ] $> t
fillInTypDef _ typ =
  addPreludeImport $> typ

fillInSubTypDef :: Maybe (Array RawProp) -> Typ -> F Typ
fillInSubTypDef rp typ@(TypRef name) = do
  typ' <- traverse (readRawProp) subRp <#> map _.typ
  if typ' /= Just typ
    then TypRef <$> recordTypDef { name, typ: typ' }
    else pure typ

  where
    subRp = rp >>= Array.find (\(RawProp p) -> p.name == name)

fillInSubTypDef _ t =
  fillInTypDef Nothing t

recordTypDef :: TypeDef -> F String
recordTypDef { name, typ } = do
  typDefMap <- _.typeDefMap <$> get
  case Map.lookup name typDefMap, typ of
    Just { typ: Just t1 }, Just t2 ->
      if t1 == t2
      then pure name
      else recordTypDef { name: name <> "'",  typ }
    Just { typ: Just t1 }, Nothing ->
      pure name
    _, _ -> do
      modify_ (_ { typeDefMap = Map.insert name { name, typ } typDefMap })
      pure name

collectTypeDefs :: Array ComponentSpec -> Array TypeDef
collectTypeDefs ms = toTypeDef <$> Set.toUnfoldable set
  where
    set = foldr collectFromModule Set.empty ms
    collectFromModule {props} s = foldr (\p s' -> collectFromTyp p.typ s') s props

    collectFromTyp :: Typ -> Set String -> Set String
    collectFromTyp (TypRef name) s = Set.insert name s
    collectFromTyp (TypUnion ts) s = foldr collectFromTyp s ts
    collectFromTyp (TypFn { params, out }) s = foldr collectFromTyp s (Array.cons out params)
    collectFromTyp (TypArray t) s = collectFromTyp t s
    collectFromTyp (TypRecord es) s = foldr (collectFromTyp <<< _.typ) s es
    collectFromTyp _ s = s

    toTypeDef name = { name, typ: Nothing }

addImport :: String -> Array PSImportEntry -> F Unit
addImport moduleName entries =
  modify_ (\s -> s { imports = Map.alter alter moduleName s.imports })
  where
    alter = Just
      <<< (\b -> foldl (\m a -> Set.insert a m) b entries)
      <<< fromMaybe Set.empty

addPreludeImport ::  F Unit
addPreludeImport =
  modify_ (_ { importPrelude = true })
