module Polaris.Codegen.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign (Foreign)
import Foreign.Object (Object)
import Simple.JSON (class ReadForeign, readImpl)

newtype RawEntry =
  RawEntry { name :: String
           , "type" :: String
           , mandatory :: Boolean
           , description :: String
           , "types" :: Maybe (Array RawEntry)
             --  , defaultValue :: Foreign
           }

instance rawEntryReadForeign :: ReadForeign RawEntry where
  readImpl f = RawEntry <$> readImpl f

type ModuleExtras =
  { props :: Maybe (Array (Object Foreign))
  , subcomponents :: Maybe (Array { name :: String, props :: Array RawEntry })
  }

--  {"type":"string","kind":"string","mandatory":false,"tags":null,"description":"Space separated list of character encodings","defaultValue":null,"types":null,"__typename":"Property"}

type Module =
  { name :: String
  , props :: Array PropEntry
  , subcomponents :: Array Subcomponent
  }

type Subcomponent =
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
  | TypBooleanLiteral Boolean
  | TypUnion (NonEmptyArray Typ)
  | TypFn { params :: Array Typ, out :: Typ }
  | TypJSX
  | TypRef (Array String)
  | TypArray Typ
  | TypRecord (Array { name :: String, typ :: Typ })
  | TypForeign -- for any type

derive instance typGeneric :: Generic Typ _
instance typShow :: Show Typ where
  show x = genericShow x

type PSJSContent =
  { psContent :: String
  , jsContent :: String
  }
