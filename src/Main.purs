module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldr, intercalate)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log, warn)
import Effect.Exception (error)
import Effect.Exception as Exception
import Foreign (renderForeignError)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, mkdir, readTextFile, readdir, writeTextFile)
import Node.Path (FilePath, basenameWithoutExt)
import Polaris.Codegen.LocalesModulePrinter (printLocalesJSModule, printLocalesPSModule)
import Polaris.Codegen.ModulePrinter (printJSModule, printPSModule)
import Polaris.Codegen.TypParser (parseTyp)
import Polaris.Codegen.TypeRefsModulePrinter (printTypeRefsModule)
import Polaris.Codegen.Types (Module, PropEntry, RawEntry, Typ(..))
import Simple.JSON (class ReadForeign, readJSON)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (eof)

main :: Effect Unit
main = runAff_ logResult do
  unlessM (exists generatedSrcDir) (mkdir generatedSrcDir)

  modules <- listDataFiles >>= traverse readModule
  traverse_ writeModule modules
  writeTypeRefsModule modules

  listLocales >>= writeLocalesModule

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
  log $ "Reading " <> path
  rs <- readRawEntries path

listLocales :: F (Array FilePath)
listLocales =
  map (\p -> basenameWithoutExt p ".json") <$> readdir "../node_modules/@shopify/polaris/locales"

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

generatedSrcDir :: FilePath
generatedSrcDir = "../src/generated"

writeModule :: Module -> F Unit
writeModule m@{ name } =
  writePSJSSrc
    { base: generatedSrcDir <> "/Polaris.Components." <> name
    , psContent: printPSModule m
    , jsContent: printJSModule m
    }

writeTypeRefsModule :: Array Module -> F Unit
writeTypeRefsModule ms = do
  log $ "Writing " <> typesPath
  writeTextFile UTF8 typesPath typesContent
  where
    typesPath = generatedSrcDir <> "/Polaris.Types.purs"
    typesContent = printTypeRefsModule refNames

    refNames = collectRefNames ms

writeLocalesModule :: Array String -> F Unit
writeLocalesModule ls =
  writePSJSSrc
    { base: generatedSrcDir <> "/Polaris.Components.Locales"
    , psContent: printLocalesPSModule ls
    , jsContent: printLocalesJSModule ls
    }

writePSJSSrc :: { base :: String, jsContent :: String, psContent :: String } -> F Unit
writePSJSSrc { base, jsContent, psContent } = do
  log $ "Writing " <> psPath
  writeTextFile UTF8 psPath psContent
  writeTextFile UTF8 jsPath jsContent

  where
    psPath = base <> ".purs"
    jsPath = base <> ".js"

readPropEntry :: RawEntry -> F PropEntry
readPropEntry r = readTyp' r."type" <#> \typ ->
  { name: r.name
  , typ
  , required: r.mandatory
  , description: r.description
  }

  where
    readTyp' s = case runParser s (parseTyp <* eof) of
      Left e -> errMessage $ "Given: " <> s <> ", Error: " <> show e
      Right a -> pure a

errMessage :: forall a. String -> F a
errMessage = throwError <<< error

collectRefNames :: Array Module -> Array (Array String)
collectRefNames ms = Set.toUnfoldable set
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
