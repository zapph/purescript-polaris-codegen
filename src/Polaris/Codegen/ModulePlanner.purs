module Polaris.Codegen.ModulePlanner
       ( planModule
       ) where

import Prelude

import Control.Monad.Except (lift)
import Control.Monad.State (StateT, get, modify_, runStateT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Polaris.Codegen.PrinterUtils (isCommonType)
import Polaris.Codegen.TypParser (parseTyp)
import Polaris.Codegen.Types (ComponentSpec, Module, Prop, RawComponent, RawProp(..), Typ(..), TypeDef, typJSX)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (eof)

planModule
  :: { name :: String
     , rawProps :: Array RawProp
     , rawSubComponents :: Array RawComponent
     }
     -> Either String Module
planModule { name, rawProps, rawSubComponents } = uncurry mkModule <$> runStateT plan Map.empty
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
      , baseProps
      , hasJSXChildren: not $ Array.null childrenProps
      }

      where
        { yes: childrenProps, no: baseProps } = Array.partition isJSXChildrenProp props

    isJSXChildrenProp p = p.name == "children" && p.typ == typJSX

    readRawComponent' toNamePath r = mkComponentSpec toNamePath <$> readRawComponent r
    mainToNamePath = Array.singleton
    subToNamePath n = [ name, n ]

    mkModule { name: n, specs } typeDefMap =
      { name: n
      , specs
      , typeDefs: Array.fromFoldable $ Map.values typeDefMap
      }

type St = Map String TypeDef
type F a = StateT St (Either String) a

readRawComponent :: RawComponent -> F { name :: String, props :: Array Prop }
readRawComponent { name, rawProps } = do
  (traverse readRawProp) rawProps <#> { name, props: _ }

readRawProp :: RawProp -> F Prop
readRawProp (RawProp r) = do
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

fillInTypDef :: Maybe (Array RawProp) -> Typ -> F Typ
fillInTypDef _ (TypRef name) | isCommonType name = do
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
  TypUnion <$> traverse (fillInSubTypDef rp) typs
fillInTypDef rp (TypFn { params, out }) = do
  params' <- traverse (fillInSubTypDef rp) params
  out' <- fillInSubTypDef rp out
  pure $ TypFn { params, out }
fillInTypDef _ t =
  pure t

fillInSubTypDef :: Maybe (Array RawProp) -> Typ -> F Typ
fillInSubTypDef rp typ@(TypRef name) = do
  typ' <- traverse readRawProp subRp <#> map _.typ
  if typ' /= Just typ
    then TypRef <$> recordTypDef { name, typ: typ' }
    else pure typ

  where
    subRp = rp >>= Array.find (\(RawProp p) -> p.name == name)

fillInSubTypDef _ t =
  fillInTypDef Nothing t

recordTypDef :: TypeDef -> F String
recordTypDef { name, typ } = do
  typDefMap <- get
  case Map.lookup name typDefMap, typ of
    Just { typ: Just t1 }, Just t2 ->
      if t1 == t2
      then pure name
      else recordTypDef { name: name <> "'",  typ }
    Just { typ: Just t1 }, Nothing ->
      pure name
    _, _ -> do
      modify_ (Map.insert name { name, typ })
      pure name
