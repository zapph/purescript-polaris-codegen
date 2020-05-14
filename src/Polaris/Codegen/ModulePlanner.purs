module Polaris.Codegen.ModulePlanner
       ( planModule
       ) where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Polaris.Codegen.TypParser (parseTyp)
import Polaris.Codegen.Types (ComponentSpec, Module, Prop, RawComponent, RawProp(..), Typ(..), TypeDef)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (eof)

planModule
  :: { name :: String
     , rawProps :: Array RawProp
     , rawSubComponents :: Array RawComponent
     }
     -> Either String Module
planModule { name, rawProps, rawSubComponents } = do
  mainSpec <- readRawComponent' mainToNamePath { name, rawProps }
  subSpec <- traverse (readRawComponent' subToNamePath) rawSubComponents
  let specs = Array.cons mainSpec subSpec
      typeDefs = collectTypeDefs specs

  pure { name, typeDefs, specs }

  where
    readRawComponent :: RawComponent -> Either String { name :: String, props :: Array Prop }
    readRawComponent { name: name', rawProps: rawProps' } =
      traverse
        readRawProp
        rawProps' <#> { name: name', props: _ }

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

readRawProp :: RawProp -> Either String Prop
readRawProp (RawProp r) = readTyp' r."type" <#> \typ ->
  { name: r.name
  , typ
  , required: r.mandatory
  , description: r.description
  }

  where
    readTyp' s = lmap (showParseError s) (runParser s (parseTyp <* eof))
    showParseError s e = "Given: " <> s <> ", Error: " <> show e

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
