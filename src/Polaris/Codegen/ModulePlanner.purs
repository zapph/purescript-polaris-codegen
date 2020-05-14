module Polaris.Codegen.ModulePlanner
       ( planModule
       ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Polaris.Codegen.PrinterUtils (printRefName)
import Polaris.Codegen.Types (ComponentSpec, Module, ModulePlan, Typ(..), TypeDef)

planModule :: Module -> ModulePlan
planModule { name, props, subcomponents } =
  { name, typeDefs, specs }
  where
    mainSpec =
      { namePath: [ name ]
      , props: props
      }

    subSpecs = subcomponents <#> \sub ->
      { namePath: [ name, sub.name ]
      , props: sub.props
      }

    specs = Array.cons mainSpec subSpecs

    typeDefs = collectTypeDefs specs

collectTypeDefs :: Array ComponentSpec -> Array TypeDef
collectTypeDefs ms = toTypeDef <$> Set.toUnfoldable set
  where
    set = foldr collectFromModule Set.empty ms
    collectFromModule {props} s = foldr (\p s' -> collectFromTyp p.typ s') s props

    collectFromTyp :: Typ -> Set (Array String) -> Set (Array String)
    collectFromTyp (TypRef name) s = Set.insert name s
    collectFromTyp (TypUnion ts) s = foldr collectFromTyp s ts
    collectFromTyp (TypFn { params, out }) s = foldr collectFromTyp s (Array.cons out params)
    collectFromTyp (TypArray t) s = collectFromTyp t s
    collectFromTyp (TypRecord es) s = foldr (collectFromTyp <<< _.typ) s es
    collectFromTyp _ s = s

    toTypeDef rn = { name: printRefName rn, typ: Nothing }
