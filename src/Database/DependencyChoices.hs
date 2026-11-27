{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The dependencies the engine keeps for a database, apart from what its
configuration says: those clicked on its setup page, and those it found by
linking against the databases loaded beside it. They live in one file of the
data directory, written whole at every change: the entries of the databases
the engine holds from memory, those of any other database as they were, since
an engine started on another configuration from the same directory may hold
it.
-}
module Database.DependencyChoices (
    Kept (..),
    Keeper (..),
    keptOf,
    settleWith,
    standingKept,
    overriddenDeclarations,
    keptAfter,
    renderKept,
    decodeKept,
    choicesFile,
    readKept,
    writeKept,
) where

import Control.Exception (IOException, try)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath ((</>))
import TOML (DecodeTOML (..), Decoder, getArrayOf, getFieldWith)
import qualified TOML

import Config (DatabaseConfig (..), Dependencies (..), DependencyOrigin (..))
import Database.UploadedDatabase (getDataDir)

-- | Who settled the dependencies the engine keeps for a database.
data Keeper = ByClick | ByLinking
    deriving (Show, Eq)

-- | The dependencies the engine keeps for one database.
data Kept = Kept {keptBy :: !Keeper, keptDepends :: ![Text]}
    deriving (Show, Eq)

instance DecodeTOML Kept where
    tomlDecoder = Kept <$> getFieldWith keeper "origin" <*> getFieldWith (getArrayOf tomlDecoder) "depends"
      where
        keeper :: Decoder Keeper
        keeper =
            tomlDecoder >>= \case
                "clicked" -> pure ByClick
                "found" -> pure ByLinking
                other -> fail ("origin is \"clicked\" or \"found\", not \"" <> T.unpack other <> "\"")

-- | What the engine keeps of a database's dependencies: nothing its configuration already says.
keptOf :: DatabaseConfig -> Maybe Kept
keptOf config = case dcDepends config of
    Settled Clicked names -> Just (Kept ByClick names)
    Settled Found names -> Just (Kept ByLinking names)
    Settled Configured _ -> Nothing
    Unsettled -> Nothing

{- | A database's dependencies once what the engine keeps is read: a click
wins over the configuration, which wins over what linking found.
-}
settleWith :: Map Text Kept -> DatabaseConfig -> DatabaseConfig
settleWith kept config = config{dcDepends = settled (M.lookup (dcName config) kept) (dcDepends config)}
  where
    settled :: Maybe Kept -> Dependencies -> Dependencies
    settled (Just (Kept ByClick names)) _ = Settled Clicked names
    settled _ declared@(Settled _ _) = declared
    settled (Just (Kept ByLinking names)) Unsettled = Settled Found names
    settled Nothing Unsettled = Unsettled

{- | The entries a start can stand on, and one sentence for each left out or
naming a database the engine does not hold. An entry that would close a loop
through the dependencies the configuration declares, or through the entries
taken before it, is left out: the start could load none of the databases. An
entry naming a database unknown here stays, said, as it may come back.
-}
standingKept :: Map Text Kept -> [DatabaseConfig] -> (Map Text Kept, [String])
standingKept kept configs = (standing, map unknown (M.toList (M.filter (any (`S.notMember` known) . keptDepends) standing)) <> looping)
  where
    known :: S.Set Text
    known = S.fromList (map dcName configs)

    declared :: Map Text [Text]
    declared = M.fromList [(dcName c, names) | c <- configs, Settled Configured names <- [dcDepends c]]

    held :: Map Text Kept
    held = M.restrictKeys kept known

    (standing, looping) = M.foldrWithKey admit (M.empty, []) held

    admit :: Text -> Kept -> (Map Text Kept, [String]) -> (Map Text Kept, [String])
    admit name entry (taken, said)
        | any (reaches (dependenciesWith taken) name) (keptDepends entry) =
            (taken, (T.unpack name <> " keeps " <> listed (keptDepends entry) <> " in dependencies.toml, which would close a loop; it is left out") : said)
        | otherwise = (M.insert name entry taken, said)

    -- The dependencies of a database once these entries are taken, a click over the configuration.
    dependenciesWith :: Map Text Kept -> Text -> [Text]
    dependenciesWith taken name = case (M.lookup name taken, M.lookup name declared) of
        (Just (Kept ByClick names), _) -> names
        (_, Just names) -> names
        (Just (Kept ByLinking names), Nothing) -> names
        (Nothing, Nothing) -> []

    reaches :: (Text -> [Text]) -> Text -> Text -> Bool
    reaches dependenciesOf target = go S.empty
      where
        go :: S.Set Text -> Text -> Bool
        go seen name
            | name == target = True
            | S.member name seen = False
            | otherwise = any (go (S.insert name seen)) (dependenciesOf name)

    unknown :: (Text, Kept) -> String
    unknown (name, entry) =
        T.unpack name
            <> " keeps "
            <> listed (filter (`S.notMember` known) (keptDepends entry))
            <> " in dependencies.toml, which no database here is named: it does not link there until its setup page changes"

-- | One sentence per configured database whose dependencies a click replaces.
overriddenDeclarations :: Map Text Kept -> [DatabaseConfig] -> [String]
overriddenDeclarations kept = mapMaybe overridden
  where
    overridden :: DatabaseConfig -> Maybe String
    overridden config = case (dcDepends config, M.lookup (dcName config) kept) of
        (Settled Configured declared, Just (Kept ByClick clicked))
            | not (dcIsUploaded config) && S.fromList declared /= S.fromList clicked ->
                Just $
                    T.unpack (dcName config)
                        <> " links to "
                        <> listed clicked
                        <> ", as chosen on its setup page, not to the "
                        <> listed declared
                        <> " its configuration names"
        _ -> Nothing

listed :: [Text] -> String
listed [] = "nothing"
listed names = T.unpack (T.intercalate ", " names)

{- | The entries after a change: those of the databases the engine holds,
from memory; those of any other database as they were, but the ones forgotten.
-}
keptAfter :: [Text] -> [DatabaseConfig] -> Map Text Kept -> Map Text Kept
keptAfter forgotten configs before =
    M.union
        (M.fromList [(dcName c, k) | c <- configs, Just k <- [keptOf c]])
        (M.withoutKeys before (S.fromList (forgotten <> map dcName configs)))

-- | The file holding these entries.
renderKept :: Map Text Kept -> Text
renderKept kept =
    T.unlines $
        [ "# Written by the engine at every change: the dependencies chosen on a"
        , "# database's setup page (clicked), or found by linking (found)."
        ]
            <> concatMap (uncurry entry) (M.toList kept)
  where
    entry :: Text -> Kept -> [Text]
    entry name (Kept by names) =
        [ ""
        , "[" <> quoted name <> "]"
        , "origin = " <> quoted (origin by)
        , "depends = [" <> T.intercalate ", " (map quoted names) <> "]"
        ]

    origin :: Keeper -> Text
    origin ByClick = "clicked"
    origin ByLinking = "found"

    quoted :: Text -> Text
    quoted t = "\"" <> T.concatMap escape t <> "\""

    escape :: Char -> Text
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = T.singleton c

-- | The file's content read back, or why it does not read.
decodeKept :: Text -> Either Text (Map Text Kept)
decodeKept = either (Left . T.pack . show) Right . TOML.decode

-- | Where the engine keeps the file.
choicesFile :: IO FilePath
choicesFile = (</> "dependencies.toml") <$> getDataDir

{- | What the file holds, none when it is absent. One that does not read is
said, and nothing in it is kept: the next change writes it again.
-}
readKept :: IO (Either Text (Map Text Kept))
readKept = do
    path <- choicesFile
    present <- doesFileExist path
    if not present
        then pure (Right M.empty)
        else either (\e -> Left (T.pack (path <> ": " <> show (e :: IOException)))) (either (Left . ((T.pack path <> ": ") <>)) Right . decodeKept) <$> try (TIO.readFile path)

{- | Write the file again after a change ('keptAfter'). Written beside and then
renamed, so a reader never meets half of it. A file that does not read is
left as it is, and the change refused, rather than written over with the
entries it holds lost.
-}
writeKept :: [Text] -> [DatabaseConfig] -> IO (Either Text ())
writeKept forgotten configs = do
    path <- choicesFile
    dir <- getDataDir
    readKept >>= \case
        Left err -> pure (Left err)
        Right before -> do
            result <- try $ do
                createDirectoryIfMissing True dir
                TIO.writeFile (path <> ".tmp") (renderKept (keptAfter forgotten configs before))
                renameFile (path <> ".tmp") path
            pure $ either (\e -> Left (T.pack (path <> ": " <> show (e :: IOException)))) Right result
