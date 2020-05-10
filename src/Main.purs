module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
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
import Polaris.Codegen.TypParser (parseTyp)
import Polaris.Codegen.Types (Module, PropEntry, RawEntry)
import Simple.JSON (class ReadForeign, readJSON)
import Text.Parsing.Parser (runParser)

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
readPropEntry r = readTyp' r."type" <#> \typ ->
  { name: r.name
  , typ
  , required: r.mandatory
  , description: r.description
  }

  where
    readTyp' s = case runParser s parseTyp of
      Left e -> errMessage $ "Given: " <> s <> ", Error: " <> show e
      Right a -> pure a

errMessage :: forall a. String -> F a
errMessage = throwError <<< error
