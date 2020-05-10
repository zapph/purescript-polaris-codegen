module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), fromRight)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), split)
import Data.String as String
import Data.String.Regex (Regex, regex)
import Data.String.Regex (match, test) as Regex
import Data.String.Regex.Flags (noFlags) as Regex
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log, warn)
import Effect.Exception (error)
import Effect.Exception as Exception
import Foreign (renderForeignError)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath, basenameWithoutExt)
import Polaris.Codegen.Types (Module, PropEntry, RawEntry, Typ(..))
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, readJSON)

main :: Effect Unit
main = runAff_ logResult do
  listDataFiles >>= traverse readModule
  where
    logResult (Left e) = warn $ "ERROR: " <> (Exception.message e)
    logResult (Right a) = log "OK"

type F = Aff

listDataFiles :: F (Array FilePath)
listDataFiles =
  readDirRel "./data" >>= traverse readDirRel <#> join

  where
    readDirRel path = map (\s -> path <> "/" <> s) <$> readdir path

readModule :: FilePath -> F Module
readModule path = do
  log path
  rs <- readRawEntries path
  props <- traverse readPropEntry rs

  pure { name, props }

  where
    readRawEntries :: FilePath -> F (Array RawEntry)
    readRawEntries = readContent

    name = basenameWithoutExt path ".json"

readContent :: forall a. ReadForeign a => FilePath -> F a
readContent path =
  readTextFile UTF8 path >>= \s -> case readJSON s of
    Left e ->
      errMessage $ intercalate ", " $ map renderForeignError $ e
    Right a ->
      pure a

readPropEntry :: RawEntry -> F PropEntry
readPropEntry r = readTyp r."type" <#> \typ ->
  { name: r.name
  , typ
  , required: r.mandatory
  , description: r.description
  }

readTyp :: String -> F Typ
readTyp t = case t of
  "string" -> pure TypString
  "boolean" -> pure TypBoolean
  "number" -> pure TypNumber
  "any" -> pure TypForeign
  "React.ReactNode" -> pure TypJSX

  _ ->
    (TypUnion <$> readUnion t)
    <|> (TypStringLiteral <$> readStringLiteral t)
    <|> (TypRef <$> readRef t)
    <|> (TypFn <$> readFn t)
    <|> (TypArray <$> readArray t)
    <|> typUnknown
  where
    typUnknown = do
      warn $ "encountered unknown type: " <> t
      pure $ TypUnknown t

readUnion :: String -> F (NonEmptyArray Typ)
readUnion s =
  resolveParts >>= traverse readTyp
  where
    parts = case NonEmptyArray.fromArray (split (Pattern " | ") s) of
      Just na | NonEmptyArray.length na > 1 -> Just na
      _ -> Nothing

    resolveParts =
      noteErr "Cannot find union parts" parts

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
  unsafePartial $ fromRight $ regex "^[A-Z][a-zA-Z0-9_]+$" Regex.noFlags

readRef :: String -> F String
readRef s =
  if Regex.test refRE (escapeAmpersand s)
  then pure s
  else errMessage $ "Invalid ref name: " <> s

  where
    -- cheap way to handle `Foo & Bar`
    escapeAmpersand = String.replaceAll (Pattern " & ") (Replacement "_")

methodRE :: Regex
methodRE =
  unsafePartial $ fromRight $ regex "^[(](.*)[)] => (\\w+)$" Regex.noFlags

readFn :: String -> F { params :: Array Typ, out :: Typ }
readFn s = do
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

    readParamsPart s' = case String.split (Pattern ", ") s' of
      [] -> pure []
      [""] -> pure []
      ps -> traverse readParamPart ps

    readParamPart s' = case String.split (Pattern ": ") s' of
      [_, t] -> readTyp t
      _ -> errMessage $ "invalid param: " <> s'

    readOutPart "void" = pure TypUnit
    readOutPart t = readTyp t

arrayRE :: Regex
arrayRE =
  unsafePartial $ fromRight $ regex "^(?:(\\w+)|\\((.+)\\))\\[\\]$" Regex.noFlags

readArray :: String -> F Typ
readArray s =
  (noteErr "not array elem" m) >>= readTyp
  where
    m :: Maybe String
    m = Regex.match arrayRE s >>= \nel ->
      join (nel !! 1) <|> join (nel !! 2)

noteErr :: forall a. String -> Maybe a -> F a
noteErr msg a = case a of
  Just a' -> pure a'
  Nothing -> errMessage msg

errMessage :: forall a. String -> F a
errMessage = throwError <<< error
