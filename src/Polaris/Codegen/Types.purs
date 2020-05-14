module Polaris.Codegen.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign (Foreign)
import Foreign.Object (Object)
import Simple.JSON (class ReadForeign, readImpl)

newtype RawProp =
  RawProp { name :: String
           , "type" :: String
           , mandatory :: Boolean
           , description :: Maybe String
           , "types" :: Maybe (Array RawProp)
             --  , defaultValue :: Foreign
           }

instance rawPropReadForeign :: ReadForeign RawProp where
  readImpl f = RawProp <$> readImpl f

type RawComponent =
  { name :: String
  , rawProps :: Array RawProp
  }

type RawModule =
  { name :: String
  , rawProps :: Array RawProp
  , rawSubComponents :: Array RawComponent
  }

type ModuleExtras =
  { rawProps :: Maybe (Array (Object Foreign))
  , rawSubComponents :: Maybe (Array RawComponent)
  }

--  {"type":"string","kind":"string","mandatory":false,"tags":null,"description":"Space separated list of character encodings","defaultValue":null,"types":null,"__typename":"Property"}

type Module =
  { name :: String
  , typeDefs :: Array TypeDef
  , specs :: Array ComponentSpec
  }

type TypeDef = { name :: String, typ :: Maybe Typ }

type ComponentSpec =
  { namePath :: Array String
  , props :: Array Prop
  }

type Prop =
  { name :: String
  , typ :: Typ
  , required :: Boolean
  , description :: Maybe String
  }

data Typ
  = TypUnit
  | TypString
  | TypBoolean
  | TypNumber
  | TypStringLiteral String
  | TypBooleanLiteral Boolean
  | TypUnion (NonEmptyArray Typ)
  | TypFn { params :: Array Typ, out :: Typ }
  | TypJSX
  | TypRef String
  | TypArray Typ
  | TypRecord (Array Prop)
  | TypForeign -- for any type

derive instance eqTyp :: Eq Typ

derive instance typGeneric :: Generic Typ _
instance typShow :: Show Typ where
  show x = genericShow x

type PSJSContent =
  { psContent :: String
  , jsContent :: String
  }
