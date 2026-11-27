{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The dependencies the engine keeps for a database, apart from what its
configuration says: those clicked on its setup page, and those it found by
linking against the databases loaded beside it. They live in one file of the
data directory, written whole from memory at every change, so the file is
never more than a picture of what the engine holds.
-}
module Database.DependencyChoices (
    Kept (..),
    Keeper (..),
    keptOf,
    settleWith,
    overriddenDeclarations,
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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath ((</>))
import TOML (DecodeTOML (..), Decoder, getArrayOf, getField, getFieldWith)
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

-- | One sentence per database whose configured dependencies a click replaces.
overriddenDeclarations :: Map Text Kept -> [DatabaseConfig] -> [String]
overriddenDeclarations kept = mapMaybe overridden
  where
    overridden :: DatabaseConfig -> Maybe String
    overridden config = case (dcDepends config, M.lookup (dcName config) kept) of
        (Settled Configured declared, Just (Kept ByClick clicked))
            | declared /= clicked ->
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

-- | The file, as the engine writes it from the databases it holds.
renderKept :: [DatabaseConfig] -> Text
renderKept configs =
    T.unlines $
        [ "# Written by the engine at every change: the dependencies chosen on a"
        , "# database's setup page (clicked), or found by linking (found)."
        ]
            <> concat [entry (dcName c) k | c <- configs, Just k <- [keptOf c]]
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

{- | Write the file whole, from the databases the engine holds. Written beside
and then renamed, so a reader never meets half of it.
-}
writeKept :: [DatabaseConfig] -> IO (Either Text ())
writeKept configs = do
    path <- choicesFile
    dir <- getDataDir
    result <- try $ do
        createDirectoryIfMissing True dir
        TIO.writeFile (path <> ".tmp") (renderKept configs)
        renameFile (path <> ".tmp") path
    pure $ either (\e -> Left (T.pack (path <> ": " <> show (e :: IOException)))) Right result
