module Polaris.Codegen.Names
       ( properName
       , moduleName
       , ident
       ) where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (Regex, regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (traverse)
import Language.PS.SmartCST (Ident(..), ModuleName(..), ProperName(..))
import Partial.Unsafe (unsafePartial)

properName :: forall a. String -> Maybe (ProperName a)
properName s =
  if Regex.test properNameRegex s
  then Just $ ProperName s
  else Nothing

properNameRegex :: Regex
properNameRegex =
  unsafeRegex "^[A-Z][A-Za-z0-9_]*$"

moduleName :: String -> Maybe ModuleName
moduleName s =
  ModuleName
  <$> ( (traverse properName <=< NonEmptyArray.fromArray)
        $ String.split (String.Pattern ".") s
      )

ident :: String -> Maybe Ident
ident s =
  if Regex.test identRegex s
  then Just $ Ident s
  else Nothing

identRegex :: Regex
identRegex =
  unsafeRegex "^[a-z][A-Za-z0-9_]*$"

-- Utils

unsafeRegex :: String -> Regex
unsafeRegex s =
  unsafePartial $ fromRight $ regex s RegexFlags.noFlags
