module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), fromRight)
import Data.Foldable (fold, intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String as String
import Data.String.Regex (Regex, regex)
import Data.String.Regex (match, test) as Regex
import Data.String.Regex.Flags (noFlags) as Regex
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (logShow)
import Effect.Exception (error)
import Foreign (renderForeignError)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, readJSON)

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

main :: Effect Unit
main = runAff_ logShow do
  readRawEntries "./data/Forms/Form.json" >>= traverse readRawEntry

type F = Aff

readRawEntries :: FilePath -> F (Array RawEntry)
readRawEntries = readContent

readContent :: forall a. ReadForeign a => FilePath -> F a
readContent path =
  readTextFile UTF8 "./data/Forms/Form.json" >>= \s -> case readJSON s of
    Left e ->
      errMessage $ intercalate ", " $ map renderForeignError $ e
    Right a ->
      pure a

readRawEntry :: RawEntry -> F PropEntry
readRawEntry r = readTyp refs kt <#> \typ ->
  { name: r.name
  , typ
  , required: r.mandatory
  , description: r.description
  }
  where
    kt = { k: r.kind, t: r."type" }

    refs = Object.fromFoldable $ toRefEntry <$> fold r."types"

    toRefEntry { kind: k, "type": t } =
      Tuple t { k, t }

type KT = { k :: String, t :: String }

readTyp :: Object KT -> KT -> F Typ
readTyp refs {k, t} = case k, t of
  "string", "string" -> pure TypString
  "boolean", "boolean" -> pure TypBoolean
  "interface", "React.ReactNode" -> pure TypJSX
  "union", _ -> TypUnion <$> readUnion refs t
  "interface", s ->
    (TypStringLiteral <$> readStringLiteral s)
    <|> (TypRef <$> readRef s)
  "method", s -> TypFn <$> readFn refs s
  _, _ -> errMessage $ "Unknown kind: " <> k <> ", type: " <> t

readUnion :: Object KT -> String -> F (NonEmptyArray Typ)
readUnion refs s =
  resolveParts >>= traverse resolvePartTyp
  where
    parts = split (Pattern " | ") s

    resolveParts =
      noteErr "Cannot find union parts" $ NonEmptyArray.fromArray parts
    resolvePartTyp name = resolveKT refs name

findKT :: Object KT -> String -> F KT
findKT refs name = case Object.lookup name refs of
  Just kt -> pure kt
  Nothing -> errMessage $ "Cannot find kt entry for: " <> name

resolveKT :: Object KT -> String -> F Typ
resolveKT refs name = findKT refs name >>= readTyp refs

stringLiteralRE :: Regex
stringLiteralRE =
  unsafePartial $ fromRight $ regex "^\"([^\"]+)\"$" Regex.noFlags

readStringLiteral :: String -> F String
readStringLiteral s =
  noteErr ("Invalid string literal: " <> s) match
  where
    match =
      Regex.match stringLiteralRE s >>= (_ !! 1) >>= identity

refRE :: Regex
refRE =
  unsafePartial $ fromRight $ regex "^[A-Z][a-zA-Z0-9]+$" Regex.noFlags

readRef :: String -> F String
readRef s =
  if Regex.test refRE s
  then pure s
  else errMessage $ "Invalid ref name: " <> s

methodRE :: Regex
methodRE =
  unsafePartial $ fromRight $ regex "^[(](.*)[)] => (\\w+)$" Regex.noFlags

readFn :: Object KT -> String -> F { params :: Array Typ, out :: Typ }
readFn refs s = do
  { paramsPart, outPart } <- readParts
  params <- readParamsPart paramsPart
  out <- readOutPart outPart
  pure { params, out }

  where
    findParts = do
      parts <- Regex.match methodRE s
      paramsPart <- join $ parts !! 1
      outPart <- join $ parts !! 2
      pure { paramsPart, outPart }

    readParts = noteErr ("Unable to find method parts: " <> s) findParts

    readParamsPart s' =
      traverse readParamPart $ String.split (Pattern ", ") s'

    readParamPart s' = case String.split (Pattern ": ") s' of
      [_, t] -> resolveKT refs t
      _ -> errMessage $ "invalid param: " <> s'

    readOutPart "void" = pure TypUnit
    readOutPart t = findKT refs t >>= readTyp refs

noteErr :: forall a. String -> Maybe a -> F a
noteErr msg a = case a of
  Just a' -> pure a'
  Nothing -> errMessage msg

errMessage :: forall a. String -> F a
errMessage = throwError <<< error

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
  | TypStringLiteral String
  | TypUnion (NonEmptyArray Typ)
  | TypFn { params :: Array Typ, out :: Typ }
  | TypJSX
  | TypRef String

derive instance typGeneric :: Generic Typ _
instance typShow :: Show Typ where
  show x = genericShow x
