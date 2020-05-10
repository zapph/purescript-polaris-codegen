module Polaris.Codegen.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)

type RawEntry =
  { name :: String
  , "type" :: String
  , kind :: String
  , mandatory :: Boolean
  , description :: String
  , "types" :: Maybe (Array SubRawEntry)
--  , defaultValue :: Foreign
  }

type SubRawEntry =
  { "type" :: String
  , kind :: String
  }

--  {"type":"string","kind":"string","mandatory":false,"tags":null,"description":"Space separated list of character encodings","defaultValue":null,"types":null,"__typename":"Property"}

type Module =
  { name :: String
  , props :: Array PropEntry
  }

type PropEntry =
  { name :: String
  , typ :: Typ
  , required :: Boolean
  , description :: String
  }

data Typ
  = TypUnit
  | TypString
  | TypBoolean
  | TypNumber
  | TypStringLiteral String
  | TypUnion (NonEmptyArray Typ)
  | TypFn { params :: Array Typ, out :: Typ }
  | TypJSX
  | TypRef String
  | TypArray Typ
  | TypForeign -- for any type
  | TypUnknown String

derive instance typGeneric :: Generic Typ _
instance typShow :: Show Typ where
  show x = genericShow x
