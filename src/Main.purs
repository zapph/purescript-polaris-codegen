module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldr, intercalate)
import Data.List (foldMap)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..), stripSuffix)
import Data.String as String
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
import Polaris.Codegen.LocalesModulePrinter (printLocalesModule)
import Polaris.Codegen.ModulePrinter (printModule)
import Polaris.Codegen.TypParser (parseTyp)
import Polaris.Codegen.TypeRefsModulePrinter (printTypeRefsModule)
import Polaris.Codegen.Types (Module, ModuleExtras, PropEntry, RawEntry, Typ(..), PSJSContent)
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

type ModuleFilePaths =
  { propsFilePath :: FilePath
  , extrasFilePath :: Maybe FilePath
  }

listDataFiles :: F (Array ModuleFilePaths)
listDataFiles =
  readDirRel "./data" >>= traverse readDirRel <#> (join <<< map getModuleFilePaths)

  where
    readDirRel path = map (\s -> path <> "/" <> s) <$> readdir path

    allFilePaths = readDirRel "./data" >>= traverse readDirRel <#> join

getModuleFilePaths :: Array FilePath -> Array ModuleFilePaths
getModuleFilePaths allFilePaths = foldMap f allFilePaths
  where
    isExtrasFilePath =
      isJust <<< stripSuffix (Pattern "-extras.json")

    f path
      | isExtrasFilePath path = mempty
      | otherwise = pure $
        { propsFilePath: path
        , extrasFilePath: extrasFilePath path
        }

    extrasFilePath propsFilePath =
      if Array.elem p allFilePaths
      then Just p
      else Nothing

      where
        p = String.replace
            (Pattern ".json")
            (Replacement "-extras.json")
            propsFilePath

listLocales :: F (Array FilePath)
listLocales =
  map (\p -> basenameWithoutExt p ".json") <$> readdir "../node_modules/@shopify/polaris/locales"

readModule :: ModuleFilePaths -> F Module
readModule { propsFilePath, extrasFilePath } = do
  log $ "Reading " <> propsFilePath
  rs' <- readRawEntries propsFilePath

  extra <- traverse readExtra extrasFilePath
  let rs = case extra >>= _.props of
        Just e -> applyExtras rs' e
        Nothing -> rs'

      rawSubcomponents = fold (extra >>= _.subcomponents)

  props <- traverse readPropEntry rs
  subcomponents <- traverse readSubcomponent rawSubcomponents

  pure { name
       , props
       , subcomponents
       }

  where
    readRawEntries :: FilePath -> F (Array RawEntry)
    readRawEntries = readContent

    name = basenameWithoutExt propsFilePath ".json"

    readExtra :: FilePath -> F ModuleExtras
    readExtra e = do
      log $ "Reading " <> e
      readContent e

    applyExtras :: Array RawEntry -> Array RawEntry -> Array RawEntry
    applyExtras rs' overrides =
      Array.filter noOverwrite rs' <> overrides
      where
        noOverwrite { name: rName } =
          isNothing $ Array.find (\r -> r.name == rName) overrides

    readSubcomponent { name: name', props: rawProps } =
      traverse readPropEntry rawProps <#> { name: name', props: _ }

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
    (generatedSrcDir <> "/Polaris.Components." <> name)
    (printModule m)

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
    (generatedSrcDir <> "/Polaris.Components.Locales")
    (printLocalesModule ls)

writePSJSSrc :: String -> PSJSContent -> F Unit
writePSJSSrc base { jsContent, psContent } = do
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
